;; this was initial attempt at implementing relativization, following
;; the Sigma implementation.  It turned out to be extremely complex,
;; full of corner cases.  We are trying an alternative implementation
;; that may be a little less performant, but probably simpler, with
;; less corner cases (see issue #34).

(defun remove-subclass-indicator (s)
  "Remove the + from a symbol that was used to indicate a subclass
   type."
  (symbolicate (subseq (symbol-name s) 0 (1- (length (symbol-name s))))))

(defun find-explicit-vars-instantiations (f &optional negative)  
  (unless negative
    (cond 
      ((atom f) nil)
      ((quantifierp f) (find-explicit-vars-instantiations (caddr f))) 
      ((and (consp f) (logical-operatorp (car f)))
       (flatten (mapcar (lambda (x) (find-explicit-vars-instantiations x (car-is f 'not))) (cdr f))))
      ((trivialp f)       
       (when (and (car-is f 'instance) (regular-varp (cadr f)))
         (list (cadr f))))
      (t
       (cons (find-explicit-vars-instantiations (car f)) (find-explicit-vars-instantiations (cdr f)))))))

(defun relativize-quantified-variables (formula &optional is-negative-literal)
  (if (consp formula)
      (cond 
        ((quantifier-termp (car formula))
         (let* ((quantified-variables (cadr formula))
                (restrictions (create-restrictions quantified-variables formula))
                (operator (if (eq (car formula) 'forall) '=> 'and)))
           (cons (car formula) 
                 (list (cadr formula)
                       (if restrictions
                           (list operator restrictions (relativize-quantified-variables (caddr formula)))
                           (relativize-quantified-variables (caddr formula)))))))
        ((logical-operatorp (car formula))
         (cons (car formula) (mapcar (lambda (x) (relativize-quantified-variables x (eq 'not (car formula)))) (cdr formula))))
        (t
         (cons (relativize-quantified-variables (car formula)) (relativize-quantified-variables (cdr formula)))))
      formula))

(defun relativize-formula (formula)
  "Generate additional preconditions so that formulas are considered
   only if the type requirements of their variables is being
   met. Example from Pease, A., and Sutcliffe, G., (2007) First Order
   Reasoning on a Large Ontology, in Proceedings of the CADE-21
   workshop on Empirically Successful Automated Reasoning on Large
   Theories (ESARLT).  See section Relativize variables in NOTES.text"
  (let* ((free-vars (find-free-variables formula))
         (restrictions (create-restrictions free-vars formula)))
    (if restrictions
        `(=> ,restrictions ,(relativize-quantified-variables formula))
        (relativize-quantified-variables formula))))

(defun create-restrictions (vars formula)
  "From a list of VARS and FORMULA, generate the appropriate type
   restriction as follows.

   First, create an alist of variables and types.  Since in SUO-KIF we
   could potentially have a variable have more than one type, we
   initially build an alist of variables and the list of their types,
   like:

   ((?X (Class1 Class2) (?Y (Class3))))

   Next, distribute the variables over the types, like:

   ((?X . Class1) (?X . Class2) (?Y . Class3))

   We also need to remove the variables that have been explicitly
   instantiated in the antecedent of the formula. (?)  For example:

   (=>
     (and
       (subrelation ?PRED1 ?PRED2)
       (instance ?PRED2 ?CLASS)
       (subclass ?CLASS InheritableRelation))
     (instance ?PRED1 ?CLASS))

   We already have an instantiation of ?PRED2 in the antecedent, so it
   will be removed from the list of sort restrictions.

   An then generate the conjunction of restrictions:

   (and (instance ?X Class1) (instance ?X Class2) (instance ?Y Class3))."
  (labels ((get-antecedent (f)
             "If the formula is a rule (=> or <=>), return its first
              argument, otherwise returns the entire formula."
             (if (and (consp f) (or (car-is f '=>) (car-is f '<=>)))
                 (cadr f)
                 f))
           (remove-explicit-restrictions (restrictions)
             (let ((instantiated-variables (find-explicit-vars-instantiations (get-antecedent formula))))
              (remove-if (lambda (x) (member (car x) instantiated-variables)) restrictions)))
           (infer-type (x) (get-implicit-types x formula))
           (type-restriction (var type)
             (cond ((regular-typep type) 
                    `(instance ,var ,type))
                   ((subclass-typep type)
                    `(subclass ,var ,(remove-subclass-indicator type))))))
    (when vars
      (let* ((vars-types (mapcar #'infer-type vars))
             (distributed-vars (remove-explicit-restrictions (union* (distribute-alist (mapcar #'list vars vars-types)))))
             (restrictions 
              (remove nil (mapcar (lambda (x) (type-restriction (car x) (cdr x))) distributed-vars))))
        (when restrictions
          (if (= 1 (length restrictions))
              (car restrictions)
              (cons 'and restrictions)))))))

;; Copyright 2016 IBM

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(in-package #:suo-kif)

(defparameter *max-row-expansion* 7 "Maximum number of variables to expand row vars")

(defparameter *excluded-predicates*
  '(documentation domain format termFormat externalImage
    relatedExternalConcept relatedInternalConcept formerName 
    abbreviation conventionalShortName conventionalLongName)
  "Predicates to be excluded from the final TPTP conversion")

(defparameter *kb* nil
 "All axioms defined in the KIF files.")

(defparameter *transformed-kb* nil
  "*KB*, after all transformation passes.")

(defun excluded-predicatep (f)
  (member (car f) *excluded-predicates*))

(defun car-is (formula symbol)
  "Checks if the CAR of formula is SYMBOL."
  (eq (car formula) symbol))

(defun nth-arg-is (formula idx symbol)
  "Checks if the parameter at position IDX is SYMBOL."
  (eq (nth idx formula) symbol))

(defun instance-of (formula type)
  "Checks if FORMULA defines an instance of TYPE."
  (and (car-is formula 'instance) (nth-arg-is formula 2 type)))

(defun read-kif (files)
  (let ((res nil))
    (dolist (file files)
      (with-open-file (kb file)
        (do ((st (read kb nil nil)
                 (read kb nil nil)))
            ((null st) res)
          (push st res))))
    (remove-duplicates res :test #'equal)))

(defun collect-relation-domains (formulas subrelations)
  "Collects all domain information from FORMULAS.

   First collect the types and positions of the arguments and store
   them in a temporary hash where each entry is a list of lists, one
   for each argument/position parameter.  For example:

   (domain domain 1 Relation)
   (domain domain 2 PositiveInteger)
   (domain domain 3 SetOrClass)
   >> key: 'domain, value: ((1 Relation) (2 PositiveInteger) (3 SetOrClass))

   Then, order the lists and extract the just types, so in the end
   with end up with a hash where each key has a list of types.
   Following the example above:
   >> key: 'domain, value: (Relation PositiveInteger SetOrClass)

   If the type is defined as a domainSubClass, append + to the
   domain (following Sigmakee)."
  (let ((r (make-hash-table))
        (temp (make-hash-table)))
    (dolist (formula formulas)
      (when (or (car-is formula 'domain) (car-is formula 'domainSubclass))
        (let ((relation (cadr formula))
              (position (caddr formula))
              (type (if (car-is formula 'domain) 
                        (cadddr formula) 
                        (symbolicate (cadddr formula) '+))))
          (push (cons position type) (gethash relation temp)))))
    ;; now we fill the gaps of the subrelations by copying the missing
    ;; domain/domainSubclass from their parents
    (dolist (subrelation-info subrelations)
      (let* ((child (car subrelation-info))
             (parent (cdr subrelation-info))
             (child-types (gethash child temp))
             (parent-types (gethash parent temp)))
        (dolist (type-info (set-difference parent-types child-types  :key #'car))
          (push type-info (gethash child temp)))))
    (maphash (lambda (k v)
               (setf (gethash k r) 
                     (mapcar #'cdr (sort v #'< :key #'car)))) temp)
    r))

;; TODO: unify collect-relation-hierarchy/collect-class-hierarchy?
(defun collect-relation-hierarchy (formulas)
  "Collects information about the hierarchy of relations as defined by
the SUBRELATION statement."
  (let ((parent-relations nil))
    (dolist (formula formulas)
      (when (and (= 3 (length formula)) (car-is formula 'subrelation))
        (let ((child (cadr formula))
              (parent (caddr formula)))
          (setf parent-relations (acons child parent parent-relations)))))
    parent-relations))

(defun collect-class-hierarchy (formulas)
  "Collects information about the class hierarchy as defined by the
SUBCLASS statement."
  (let ((subclasses (make-hash-table))
        (superclasses (make-hash-table)))
    (dolist (formula formulas)
      (when (and (= 3 (length formula)) (car-is formula 'subclass))
        (let ((child (cadr formula))
              (parent (caddr formula)))
          (setf (gethash child superclasses) parent)
          (push child (gethash parent subclasses)))))
    (values subclasses superclasses)))

(defun collect-variable-arity-predicates (formulas)
  "Collects all predicates that are defined as having variable
arity via (instance <PRED> VariableArityRelation)."
  (let ((r nil))
    (dolist (formula formulas)
      (when (instance-of formula 'VariableArityRelation)
        (let ((predicate (cadr formula)))
          (push predicate r))))
    r))

(defun collect-instances (formulas)
  "Collects all the instances and their types."
  (let ((r (make-hash-table)))
    (dolist (formula formulas)
      (when (car-is formula 'instance)
        (let ((name (cadr formula))
              (type (caddr formula)))
          (push type (gethash name r)))))
    r))

(defun topmost-relation (relation)
  "Goes up in the relation hierarchy to find the topmost relation that is a parent of relation."
  (let ((parent (assoc relation *parent-relation*)))
    (if parent
        (topmost-relation (cdr parent))
        relation)))

(defun variable-arityp (relation)
  "Checks whether RELATION is a variable-arity relation."
  (or 
   (not (null (member relation *variable-arity-relations*)))
   (not (null (member (topmost-relation relation) *variable-arity-relations*)))))

(defun immediate-subrelations-of (r)
  (mapcar #'car (rassoc-all r *parent-relation*)))

;; TODO: check if relation is actually a relation symbol
(defun find-relation-hierarchy (relation &optional (flatten nil))
  "Returns the full hierarchy starting in RELATION and recursively
finding its children. Optionally flatten the output."
  (when relation
    (let ((h (cons relation (mapcar #'find-relation-hierarchy (immediate-subrelations-of relation)))))
      (if flatten
          (flatten h)
          h))))

;; TODO: check if class is actually a class symbol
(defun find-class-hierarchy (class &optional (flatten nil))
  "Returns the full hierarchy starting in CLASS and recursively
finding its children. Optionally flatten the output via parameter
FLATTEN."
  (when class
    (let ((h (cons class (mapcar #'find-class-hierarchy (gethash class *subclasses*)))))
      (if flatten
          (flatten h)
          h))))

(defun find-class-parents (class)
  "Returns the parents of CLASS as a simple list, where the last
element is the topmost class."
  (let ((p (gethash class *superclasses*)))
    (when p
      (list* p (find-class-parents p)))))

(defun subclassp (x y)
  "Checks whether X is a subclass of Y."
  (not (null (member x (cdr (find-class-hierarchy y t))))))

(defun remove-superclasses (set)
  "Reduce set of the set of things that aren't related via superclass
relation."
  (dolist (x set)
    (dolist (y set)
      (when (subclassp x y)
        (removef set y))))
  set)

(defun get-explicit-types (variable formula)
  "Check of explicit type instantiations, defined as INSTANCE or
SUBCLASS statements where VARIABLE appears."
  (flet ((infer-explicit-type (f)
           (when (consp f)
             (when (or (car-is f 'instance) (car-is f 'subclass))
               (let ((p (position variable f)))
                 (when (and p (= p 1))
                   (let ((potential-type (caddr f)))
                     (when (not (variablep potential-type))
                       potential-type))))))))
    (let ((types (map-fold formula #'infer-explicit-type #'bubble-up)))
      (if (atom types) (list types) types))))

(defun get-implicit-types (variable formula)
  "Get implicit types, defined as being part of the domain declaration
of relations where VARIABLE appears."
  (labels ((infer-variable (v f)
             (cond 
               ((atom f) nil)
               ((quantifierp f) (mapcar (lambda (x) (infer-variable v x)) (cddr f)))
               ((consp f)
                (let ((p (position variable f)))
                  (if (and p (> p 0))
                      (nth (1- p) (gethash (car f) *domains*))
                      (mapcar (lambda (x) (infer-variable v x)) (cdr f))))))))
    (let ((minimal-set (remove-superclasses (remove-duplicates (flatten (infer-variable variable formula))))))
      minimal-set)))

(defun infer-variable-type (variable formula)
  "Combines the output of GET-EXPLICIT-TYPES and GET-IMPLICIT-TYPES to
infer the type of VARIABLE."
  (let ((set (remove-superclasses
              (remove nil
                      (union (get-implicit-types variable formula) 
                             (get-explicit-types variable formula))))))
    set))

(defun find-instances-of (type)
  "Returns all the instances of TYPE, including subclasses of TYPE."
  (let ((r)
        (types (find-class-hierarchy type t)))
    (maphash (lambda (instance-name instance-types)
               (when (intersection types instance-types)
                 (push instance-name r))) *instances*)
    (remove-duplicates r)))

;; TODO: check if relation is variable arity
(defun find-relation-arity (relation)
  "Returns the arity of RELATION. Only handles relations that have
domain/domainSubClass information defined."
  (let ((domains (gethash relation *domains*)))
    (when domains
        (length domains))))

;; TODO: handle multiple row variables in the same formula? (should be
;; an error)
(defun find-row-vars (formula)
  "Returns a hash table indicating, for each row variable in formula,
   which relations it appears in and the number of regular variables
   that it appears in context.

   For example:

   (=>
      (and
         (range ?FUNCTION ?CLASS)
         (equal (AssignmentFn ?FUNCTION @ROW) ?VALUE))
      (instance ?VALUE ?CLASS))

   Will return a hash table assigning @ROW to (AssignmentFn . 1) since
   @ROW appears in AssignmentFn, which already contains one regular
   var."
  (flet ((find-vars (formula hash)
           (when (consp formula)
             (let ((var (find-if #'row-varp formula)))
               (when var
                 (push (cons (car formula) (count-if #'regular-varp (cdr formula)))
                       (gethash var hash)))))))
    (let ((hash (make-hash-table)))
      (map-fold formula (lambda (x) (find-vars x hash)))
      hash)))

(defun find-predicate-vars (formula)
  "Returns a list of regular variables found in the predicate position
in FORMULA, along with their inferred types and the number of
arguments of the sub-formula they appear in.."
  (labels ((contains-row-var (l)
             (some #'row-varp l))
           (find-vars (f hash)
             (when (and (consp f) (> (length f) 1))
               (let ((pred (car f)))
                 (when (regular-varp pred)
                   (push (cons (infer-variable-type pred formula) 
                               (if (contains-row-var (cdr f))
                                   nil
                                   (length (cdr f)))) 
                         (gethash pred hash)))))))
    (let ((result (make-hash-table)))
      (map-fold formula (lambda (x) (find-vars x result)))
      result)))

(defun regular-typep (s)
  "Returns whether S is a 'regular' type or not.  A regular type is
one that is not suffixed with a +"
  (not (ends-with #\+ (symbol-name s))))

(defun subclass-typep (s)
  "Returns whether S is a 'regular' type or not.  A regular type is
one that is suffixed with a +"
  (ends-with #\+ (symbol-name s)))

(defun expand-predicate-vars (source-formula)
  "Expand variables in predicates, as follows:

   - From the hash table returned from find-predicate-vars, expand
   each value by replacing the type with all the instances of that
   type.

   - Evaluate the usage of each variable and remove from the hash
     table all relations that have different arity from those found.

   The strategy here to perform the substitutions is similar to the
   one used in EXPAND-ROW-VARS.  First, generate a list of possible
   values for each variable and then create the cross-product of those
   possible values."
  (labels ((restrict-arities (instances arity)
             (if arity
                 (remove-if-not (lambda (x) 
                                  (let ((relation-arity (find-relation-arity x)))
                                    (or (variable-arityp x) 
                                        (and relation-arity (= arity relation-arity))))) instances)
                 instances))
           (find-relations (relation-names relation-arity)
             "Find all relations that have the following requirements:
              1. satisfy all the types designed by RELATION-NAMES
              2. satisfy the arity designed by RELATION-ARITY."
             ;; TODO: if relation-names is NIL we couldn't deduce the
             ;; type of the variable.  Right now we are just ignoring
             ;; it.  But maybe we should simply return ALL possible
             ;; instances in this case?
             (when relation-names
               (restrict-arities (reduce #'intersection (mapcar #'find-instances-of relation-names)) relation-arity))))
    (let* ((formulas)
           (predicate-vars (hash-table-alist (find-predicate-vars source-formula)))
           (instantiated-predicate-vars 
            (mapcar (lambda (x) 
                      (cons (car x)
                            (list 
                             (let ((relation-names (caadr x))
                                   (relation-arity (cdadr x)))
                               (find-relations relation-names relation-arity)))))
                    predicate-vars)))
      (if instantiated-predicate-vars
          (dolist (binding (apply #'map-product #'list (distribute-alist instantiated-predicate-vars)))
            (push (sublis binding source-formula) formulas))
          (push source-formula formulas))
      formulas)))

(defun expand-row-var (rvar n)
  "Converts the row variable RVAR to a list of N regular variables. Used
  in the row variables expansion code. For example:

  (expand-row-var '@ROW 5)
  ==>
  (?ROW0 ?ROW1 ?ROW2 ?ROW3 ?ROW4)"
  (when (> n 0)
    (let ((var (subseq (symbol-name rvar) 1)))
      (mapcar (lambda (x) (symbolicate "?" var (write-to-string x))) (iota n)))))
 
;; TODO: check name clashes with existing functions/relations 
(defun rename-variable-arity-relations (formula)
  "Renames the relations that have variable arities for the cases
where the theorem prover doesn't handle overloaded predicates."
  (labels ((disambiguated-name (f)
             (symbolicate (car f) (write-to-string (length (cdr f)))))
           (insert-new-relation-signature (rel new-rel arity)
             "Assuming that the last sort specified as the domain of
              REL is the sort that is going to be used for the rest of
              parameters.  Insert into the global *domains* hash table
              the sort information about NEW-REL."
             (let ((rel-domain (gethash rel *domains*)))
               (when rel-domain
                 (let ((arity-diff (- arity (length rel-domain))))
                   (when (> arity-diff 0)
                     (let ((missing-sorts (make-list arity-diff :initial-element (car (last rel-domain)))))
                       (setf (gethash new-rel *domains*) (append rel-domain missing-sorts))))))))
           (insert-new-relation-instance (rel new-rel)
             (let ((original-types (gethash rel *instances*)))
               (setf (gethash new-rel *instances*) original-types)))
           (variable-arity-predicate (f)
             (and (consp f) (member (car f) *variable-arity-relations*)))
           (disambiguate-relation-name (f)
             (let ((rel (car f))
                   (new-rel (disambiguated-name f)))
               (unless (gethash new-rel *instances*)
                 (insert-new-relation-instance rel new-rel))
               (unless (gethash new-rel *domains*)
                 (insert-new-relation-signature rel new-rel (length (cdr f))))
               (sublis `((,rel . ,new-rel)) f))))
    `(,(treemap formula #'variable-arity-predicate #'disambiguate-relation-name))))

;; http://stackoverflow.com/questions/36720381/sublis-and-splicing/36722620#36722620
(defun sublis1 (bindings tree)
  (cond
    ((null tree) tree)
    ((atom tree) ;; a dotted list in the tree.
     (cdr (assoc tree bindings)))
    ((let ((entry (assoc (car tree) bindings)))
       (when entry
         (append (cdr entry) (sublis1 bindings (cdr tree))))))
    ((atom (car tree))
     (cons (car tree)
           (sublis1 bindings (cdr tree))))
    (t
     (cons (sublis1 bindings (car tree))
           (sublis1 bindings (cdr tree))))))

(defun expand-row-vars (source-formula)
  "Expand row variables by treating them as syntactic sugar for lists
of regular variables up to the arity of the relation.  If a relation
is of variable arity or we don't know the maximum arity, use a fixed
number (defined by *max-row-expansion*).

1. Collect all row variables and their context;

2. Decide, for each row variable, the minimum and maximum number of
   variables to be expanded into.  For relations with a predefined
   number of variables, say, 2, minimum == maximum.  For variable
   arity relations, minimum = 0, maximum = *max-row-expansion*

3. For each row variable present in the formula, make the appropriate
   substitution as follows:

   - first, expand all row variables that have minimum == maximum at
     once.  This will generate a new formula.

   - Then, for every combination of row variable expansion make the
     product of those expansions, generating new formulas."
  (labels ((get-mininum-arity-relation (relation)
             "The minimum arity of a relation is either the predefined
              number of domain declarations for that relation, for
              variable arity relations, or the fixed arity of the
              relation."
             (let ((arity (find-relation-arity relation)))
               (if arity arity 1)))
           (get-maximum-arity-relation (relation)
             "The maximum arity of a relation is either *max-row-expansion*, for
              variable arity relations, or the fixed arity of the relation."
             (let ((arity (find-relation-arity relation)))
               (if (or (variable-arityp relation) (not arity))
                   *max-row-expansion*
                   arity)))
           (get-minimum-var-expansion (var row-vars)
             "The minimum var expansion (ie., the minimum amount of
              regular vars that a row var can be expanded to is the
              maximum of all the minimum arities of all the relations
              that VAR appears.  Example:

              (and (foo @ROW) (bar @ROW))

              min arity (foo) = 2
              min arity (bar) = 1

              Then we need to expand @ROW to at least two regular
              variables, since FOO requires at least two parameters."
             (reduce #'max (mapcar #'get-mininum-arity-relation  (mapcar #'car (gethash var row-vars)))))
           (get-maximum-var-expansion (var row-vars)
             "The maximum var expansion (ie., the maximum amount of
              regular vars that a row var can be expanded to is the
              minimum of all the maximum arities of all the relations
              that VAR appears.  Example:

              (and (foo @ROW) (bar @ROW))

              max arity (foo) = 7
              max arity (bar) = 2

              Then we need to expand @ROW to at most two regular
              variables, since BAR requires at most two parameters."
             (reduce #'min (mapcar #'get-maximum-arity-relation (mapcar #'car (gethash var row-vars)))))
           (get-var-expansion (var row-vars)
             "Returns the initial assessment of the number of regular
              variables that VAR needs to be expanded into."
             (let* ((max1 (get-maximum-var-expansion var row-vars))
                    (min1 (get-minimum-var-expansion var row-vars))
                    (l (1+ (- max1 min1))))
               (when (> l 0)
                (iota l :start min1))))
           (expand-row (var formula n row-vars)
             "Expand all instances of VAR.

              To generate the actual expansion, we need to compute the
              difference between the number of non-row vars and how
              much rows we are expanding now.  For example, say we
              need to expand @ROW into four variables, ?ROW1 ?ROW2
              ?ROW3 ?ROW4.  If this row variable appears on a formula
              that already has regular variables on it, we only need
              to expand up to the difference.

              (foo ?V @ROW) ==> (foo ?V ?ROW1 ?ROW2 ?ROW3)

              If @ROW was alone, we would need to expand all the way to four:

              (foo @ROW) ==> (foo ?ROW1 ?ROW2 ?ROW3 ?ROW4).

              The situation for quantifiers is a bit different, since
              they need to range over all the expanded rows, hence the
              MAX-EXPANSION being the union of all the generated
              variables."
             (let ((max-expansion))
               (dolist (relation-info (gethash var row-vars))
                 (let* ((relation (car relation-info))
                        (expanded-vars (expand-row-var var (- n (cdr relation-info)))))
                   (setf max-expansion (union max-expansion expanded-vars))
                   (if expanded-vars
                       (setf formula 
                             (expand-row-in-predicates var formula relation expanded-vars))
                       (setf formula nil))))
               (when (and max-expansion formula)
                 (setf formula (expand-row-in-quantifiers var formula max-expansion))))
             formula)
           (expand-row-in-quantifiers (var formula expanded-vars)
             (let ((binding `((,var . ,expanded-vars))))
               (cond 
                 ((atom formula) formula)
                 ((quantifierp formula)
                  (cons (car formula) (cons (sublis1 binding (cadr formula))
                                            (mapcar (lambda (x) (expand-row-in-quantifiers var x expanded-vars)) (cddr formula)))))
                 (t 
                  (cons (car formula) (mapcar (lambda (x) (expand-row-in-quantifiers var x expanded-vars)) (cdr formula)))))))

           (expand-row-in-predicates (var formula relation expanded-vars)
             (let ((binding `((,var . ,expanded-vars))))
               (cond 
                 ((atom formula) formula)
                 ((eq (car formula) relation)
                  (let ((converted-formula (sublis1 binding formula)))
                    (cons (car converted-formula) (mapcar (lambda (x) (expand-row-in-predicates var x relation expanded-vars)) (cdr converted-formula)))))
                 (t 
                  (cons (car formula) (mapcar (lambda (x) (expand-row-in-predicates var x relation expanded-vars)) (cdr formula))))))))
    (let ((formulas)
          (substitution-template)
          (row-vars (find-row-vars source-formula)))
      (if (not (zerop (hash-table-count row-vars)))
          (progn
            (maphash (lambda (var rel-info)
                       (declare (ignore rel-info))
                       (let ((expansion (get-var-expansion var row-vars)))
                         (push (mapcar (lambda (x) (cons var x)) expansion) substitution-template)))
                     row-vars)
            ;; at this point SUBSTITUTION-TEMPLATE contains something like:
            ;; (((@row1 . 1) (@row1 . 2)) ((@row2 . 1) (@row2 . 2))) this
            ;; indicates all the minimum and maximum expansions for each row
            ;; variable this form is suitable to be used in MAP-PRODUCT to
            ;; generate all possible combinations of variables and
            ;; expansions.
            (dolist (sub (apply #'map-product #'list substitution-template))
              ;; sub is a single substitution indictator for all row
              ;; variables in SOURCE-FORMULA, for example: ((@ROW2 . 1)
              ;; (@ROW1 . 1)) this means that, in parallel, we need to
              ;; expand @ROW1 to one regular variable and @ROW2 to one
              ;; regular variable.  We need to go through the list of
              ;; predicates where these variables appear to compute the
              ;; actual number of substitutions.
              (let ((tmp (copy-tree source-formula)))
                (dolist (var-sub sub)
                  (let ((v (car var-sub))
                        (n (cdr var-sub)))
                    (setf tmp (expand-row v tmp n row-vars))))
                (when tmp
                  (push tmp formulas))))
            formulas)
          (list source-formula)))))

(defun find-free-variables (formula &optional context)
  "Look for free variables.  Recurse down the formula and follow these rules:
   1. if a variable is found and is not in the context, then it is a
   free variable;
   2. if a non-variable is found, return nil;
   3. if a quantifier is found, add its variables to the context and
   recurse down the formula being quantified;
   4. else, recurse down the CAR/CDR of the formula, preserving the
   current context."
  (cond
    ((and (regular-varp formula) (not (member formula context)))
     (list formula))
    ((atom formula) nil)
    ((quantifierp formula) 
     (find-free-variables (caddr formula) (union (cadr formula) context)))
    (t (union (find-free-variables (car formula) context) 
              (find-free-variables (cdr formula) context)))))

(defun quantify-free-variables (formula &optional (axiom? t))
  (let ((quantifier (if axiom? 'forall 'exists))
        (free-variables (find-free-variables formula)))
    (if free-variables
        `((,quantifier ,free-variables ,formula))
        `(,formula))))

(defun generate-instantiations (f)
  "From a F, generate new (instance <X> SetOrClass) as follows:
 
   If F is (instance X Y) then generate:

   (instance Y SetOrClass)

   If F is (subclass X Y) then generate:

   (instance X SetOrClass)
   (instance Y SetOrClass)"
  (let ((instantiations))
    (cond
      ((and (car-is f 'instance))
       (unless (variablep (caddr f))
         (push `(instance ,(caddr f) SetOrClass) instantiations)))
      ((car-is f 'subclass)
       (unless (variablep (cadr f))
         (push `(instance ,(cadr f) SetOrClass) instantiations))
       (unless (variablep (caddr f))
         (push `(instance ,(caddr f) SetOrClass) instantiations))))
    instantiations))

(defun save-pass (file formulas)
  (with-output-to-file (out file :if-exists :supersede)
    (dolist (f formulas)
      (print f out))))

(defun binarize-and-prenex (f)
  `(,(prenex (binarize f))))

(defun compile-suo-kif (&key in-files
                          (out-file "output.tptp") 
                          (tptp nil) 
                          (subclass-closure nil)
                          (debug-passes t)
                          (save-passes nil))
  "First pass of the compiler: reads the filename, and capture all
metadata to be used in later passes."
  (let* ((kb (read-kif in-files))
         (augmented-kb)
         (passes '(expand-predicate-vars
                   expand-row-vars
                   rename-variable-arity-relations
                   quantify-free-variables 
                   binarize-and-prenex
                   relativize-formula))
         (p)
         (q)
         (real-time 0))

    (memoize 'find-class-hierarchy)
    (memoize 'topmost-relation)
    (memoize 'subclassp)

    (setf *kb* kb)
    (setf *variable-arity-relations* (collect-variable-arity-predicates kb))
    (setf *parent-relation* (collect-relation-hierarchy kb))
    (setf *domains* (collect-relation-domains kb *parent-relation*))
    (multiple-value-setq (*subclasses* *superclasses*) (collect-class-hierarchy kb))
    (setf *instances* (collect-instances kb))

    (dolist (f kb)
      (unless (excluded-predicatep f)
        (push f augmented-kb)
        (mapcar (lambda (x) 
                  (when x
                    (let ((instance-types (gethash (cadr x) *instances*)))
                      (unless (or (not x) (member 'SetOrClass instance-types))
                        (push 'SetOrClass (gethash (cadr x) *instances*))
                        (push x augmented-kb)))))
                (generate-instantiations f))))

;; Experimental code:
;;
;; If (subclass A B) and (subclass B C) then I generate (subclass A C).
;;
;; Also, in the same example above, if I have
;;
;; (instance i A)
;;
;; then I generate also:
;;
;; (instance i B)
;; (instance i C)
;;
    (when subclass-closure
      (when save-passes
        (save-pass "closure-classes.kif" (transitive-closure-classes (hash-table-keys *subclasses*)))
        (save-pass "closure-instances.kif" (transitive-closure-instances (hash-table-keys *instances*))))

      (setf augmented-kb (append augmented-kb (transitive-closure-classes (hash-table-keys *subclasses*))))
      (setf augmented-kb (append augmented-kb (transitive-closure-instances (hash-table-keys *instances*)))))

    (format t "Original  KB: ~a~%" (length kb))
    (format t "Augmented KB: ~a~%" (length augmented-kb))

    (setf p augmented-kb)

    (dolist (pass passes)
      (when debug-passes
        (format t "[executing ~a (~a formulas): " pass (length p)))
      (setq real-time 
            (timings (lambda () 
                       (dolist (f p)
                         (dolist (n (funcall pass f))
                           (push n q))))))
      (when debug-passes
        (format t "~w s]~%" (float real-time)))
      (setf p q)
      (setf q nil)
      (when save-passes
        (save-pass (format nil "~a.kif" (symbol-name pass)) p)))

    (setf *transformed-kb* p)
    
    (when (and tptp out-file)
      (setq real-time (timings (lambda ()  (kif-tptp out-file *transformed-kb*))))
      (when debug-passes
        (format t "[saving to TPTP: ~w s]~%" (float real-time)))))
  t)

(defun relativize-formula (f &optional ctxs)
  `(,(relativize-formula1 f ctxs)))

(defun relativize-formula1 (f &optional ctxs)
  "Assuming F in prenex normal form, recurse down F until we leave the
   quantifiers, but capturing them in CTXS.  Then create the
   apropriate restrictions on the formula (see RELATIVIZE-FORMULA*)."
  (cond
    ((quantifierp f)
     `(,(car f) ,(cadr f) ,(relativize-formula1 (caddr f) (cons (cons (car f) (cadr f)) ctxs))))
    (t (relativize-formula* f ctxs))))

(defun find-explicit-instantiations (f &optional negative)  
  (unless negative
    (cond 
      ((atom f) nil)
      ((quantifierp f) (find-explicit-instantiations (caddr f))) 
      ((and (consp f) (logical-operatorp (car f)))
       (flatten (mapcar (lambda (x) (find-explicit-instantiations x (car-is f 'not))) (cdr f))))
      ((trivialp f)       
       (when (and (car-is f 'instance) (regular-varp (cadr f)))
         (list (cadr f))))
      (t
       (cons (find-explicit-instantiations (car f)) (find-explicit-instantiations (cdr f)))))))

(defun relativize-formula* (f ctxs)
  (labels ((get-antecedent (f)
             "If the formula is a rule (=> or <=>), return its first
              argument, otherwise returns the entire formula."
             (if (and (consp f) (or (car-is f '=>) (car-is f '<=>)))
                 (cadr f)
                 f))
           (remove-subclass-indicator (s)
             "Remove the + from a symbol that was used to indicate a
              subclass type."
             (symbolicate (subseq (symbol-name s) 0 (1- (length (symbol-name s))))))
           (create-restriction (var type)
             "Create a restriction stating that VAR needs to be SORT."
             (cond ((regular-typep type) 
                    `(instance ,var ,type))
                   ((subclass-typep type)
                    `(subclass ,var ,(remove-subclass-indicator type)))))
           (create-conjunction (vars formula)
             "From a list of VARS and FORMULA, generate the
              appropriate type restriction as follows.

              First, create an alist of variables and types.  Since in
              SUO-KIF we could potentially have a variable have more
              than one type, we initially build an alist of variables
              and the list of their types, like:

              ((?X (Class1 Class2) (?Y (Class3))))

              Next, distribute the variables over the types, like:

              ((?X . Class1) (?X . Class2) (?Y . Class3))

              (and (instance ?X Class1) (instance ?X Class2) 
                   (instance ?Y Class3))."
             (when vars
               (let* ((vars-types (mapcar (lambda (x) (get-implicit-types x formula)) vars))
                      (distributed-vars (union* (distribute-alist (mapcar #'list vars vars-types))))
                      (restrictions 
                       (remove nil (mapcar (lambda (x) (create-restriction (car x) (cdr x))) distributed-vars))))
                 (when restrictions
                   (if (= 1 (length restrictions))
                       (car restrictions)
                       (cons 'and restrictions))))))
           (create-restrictions (formula contexts)
             "Create restrictions of VARS over FORMULA based on
              CONTEXTS, which is a stack of quantifiers and their
              variables."
             (if contexts
                 (let* ((ctx (car contexts))
                        (instantiated-vars (find-explicit-instantiations (get-antecedent formula)))
                        (quantifier (car ctx))
                        (quantified-vars (cdr ctx))
                        (op (ecase quantifier (forall '=>) (exists 'and)))
                        (conjunction (create-conjunction (set-difference quantified-vars instantiated-vars) formula)))
                   (if conjunction
                       `(,op (,@conjunction)
                             (,@(create-restrictions formula (cdr contexts))))
                       `(,@(create-restrictions formula (cdr contexts)))))
                 formula)))
    (create-restrictions f ctxs)))

(defun transitive-closure-classes (classes)
  "Generates the transitive closure of the subclass relation.  So,
if (subclass C1 C2) and (subclass C2 C3) then generate (subclass C1
C3)."
  (let ((f))
    (dolist (c classes)
      (dolist (sc (find-class-parents c))
        (push `(subclass ,c ,sc) f)))
    (remove-duplicates f :test #'equal)))

(defun transitive-closure-instances (instances)
  "For each instance (instance I C), generate new instances that cover
the transitive closure of the subclass relation of C."
  (let ((f))
    (dolist (i instances)
      (let ((super-types))
       (dolist (type (gethash i *instances*))
         (dolist (super-type (find-class-parents type))
           (push super-type super-types)))
       (setf super-types (remove-duplicates super-types))
       (dolist (super-type super-types)
         (push `(instance ,i ,super-type) f))))
    (remove-duplicates f :test #'equal)))

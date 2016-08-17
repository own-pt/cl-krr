;; it turned out that this might be wrong, we are analyzing it.

(defun relativize-formula (f &optional ctx)
  "Add the implication (type restriction) as closer as possible to the
  atomic sentences:

[=>
   [and
      [subrelation ?PRED1 ?PRED2]
      [instance ?PRED2 ?CLASS]
      [subclass ?CLASS InheritableRelation]]
   [instance ?PRED1 ?CLASS]]

[=> [=> [and [instance ?X SetOrClass] [instance ?Y SetOrClass]]  [subclass ?X ?Y]]
    [and [=> [instance ?X Entity] [instance ?X SetOrClass]]
         [=> [instance ?Y Entity] [instance ?Y SetOrClass]]]]

  For TFF we will need to infer the most general type of the
  variables, raising an exception with we find incompatible types
  candidates for the same variable.

  Let's try to document the possibilities. We'll start with one
  variable.

  > Free variable:

[foo ?X]

translates to:

[=> [instance ?X <Sort>]
    [foo ?X]]

  >  Universally quantified variable: same as [1]

  >  Existentially quantified variable:

[exists [?X] [foo ?X]]

  translates to:

[exists [?X] [and [instance ?X <Sort>] [foo ?X]]

   Things get complicated when we move to two or more variables. The
   base cases where both variables are under the same quantifier are
   the same as above. What if they are under different quantifiers?
   Looks like we will have to add the different types of restrictions
   [=> vs and] according to the order of the quantifiers.

   Example 1:

[forall [?X] [exists [?Y] [foo ?X ?Y]]]

   Example 2:

[exists [?X] [forall [?Y] [foo ?X ?Y]]]

   Some experiments with how TFF behaves; here's the output from the
   tptp2X -ttff2fof utility:

    TFF

tff(axiom2,axiom,
        ! [X1: t1, Xn: tn] : p(X1,Xn)).

tff(axiom3,axiom,
        ? [X1: t1, Xn: tn] : p(X1,Xn)).


tff(axiom4,axiom,
        ! [X1: t1] : 
        ? [Xn: tn] : p(X1,Xn)).

tff(axiom5,axiom,
        ? [X1: t1] :
        ! [Xn: tn] : p(X1,Xn)).

    After conversion:

fof(fof_axiom2,axiom,(
    ! [X1,Xn] : 
      ( ( t1(X1)
        & tn(Xn) )
     => p(X1,Xn) ) )).

fof(fof_axiom3,axiom,(
    ? [X1,Xn] : 
      ( t1(X1)
      & tn(Xn)
      & p(X1,Xn) ) )).

fof(fof_axiom4,axiom,(
    ! [X1] : 
      ( t1(X1)
     => ? [Xn] : 
          ( tn(Xn)
          & p(X1,Xn) ) ) )).

fof(fof_axiom5,axiom,(
    ? [X1] : 
      ( t1(X1)
      & ! [Xn] : 
          ( tn(Xn)
         => p(X1,Xn) ) ) ))."
  (labels ((remove-subclass-indicator (s)
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
           (create-restrictions (vars formula contexts)
             "Create restrictions of VARS over FORMULA based on
              CONTEXTS, which is a stack of quantifiers and their
              variables."
             (unless contexts
               (setf contexts (list `(forall ,@vars))))
             (if vars
                 (let* ((ctx (car contexts))
                        (quantifier (car ctx))
                        (quantified-vars (cdr ctx))
                        (op (ecase quantifier ('forall '=>) ('exists 'and)))
                        (conjunction (create-conjunction (intersection vars quantified-vars) formula)))
                   (if conjunction
                       `(,op (,@conjunction)
                             (,@(create-restrictions (set-difference vars quantified-vars) formula (cdr contexts))))
                       `(,@(create-restrictions (set-difference vars quantified-vars) formula (cdr contexts)))))
                 formula))
           (relativize-formula1 (formula contexts)
             "Relativize the FORMULA variables using CONTEXTS to
              generate the restrictions.  Existentially quantified
              variables are restricted via AND; universally
              quantified, via =>.  Formulas that are themselves
              restrictions, such as (INSTANCE <X> <Y>) and (SUBCLASS
              <X> <Y>) are skipped to avoid redundant restrictions."
             (if (atom formula)
                 formula
                 (if (or (car-is formula 'instance) (car-is formula 'subclass))
                     formula
                     (let* ((vars (find-all-if #'regular-varp (cdr formula)))
                            (restrictions (create-restrictions vars formula contexts)))
                       restrictions)))))
    (when f
      (cond
        ((atom f) f)
        ((quantifierp f)
         `(,(car f) ,(cadr f) 
            ,@(mapcar (lambda (x) (relativize-formula x (cons (cons (car f) (cadr f)) ctx))) (cddr f))))
        ((logical-operatorp (car f))
         `(,(car f) ,@(mapcar (lambda (x) (relativize-formula x ctx)) (cdr f))))
        (t
         (if (or (car-is f 'instance) (car-is f 'subclass))
             f
             (relativize-formula1 (mapcar (lambda (x) (relativize-formula1 x ctx)) f) ctx)))))))

(defun formula-structure (f)
  "Returns a TREE representing the overall structure of the formula
F.  To be used in static analyses."
  (labels ((formula-type (f)
             (cond 
               ((eq f 'True) :relation)
               ((eq f 'False) :relation)
               ((member f +quantifiers+) :quantifier)
               ((regular-varp f) :var)
               ((strictly-relationp f) :relation)
               ((kif-functionp f) :function)
               ((or (member f +binary-logical-operators+)
                    (member f +unary-logical-operators+)) :logical)
               (t :symbol))))
    (when f
      (if (consp f)
          (cons (formula-type (car f)) (mapcar #'formula-structure (cdr f)))
          (formula-type f)))))

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

(defun binary-logical-formula-tptp (l)
  (ecase (car l)
    ('and (format nil "(~{~a~^ & ~})" (mapcar #'formula-tptp (cdr l))))
    ('or (format nil "(~{~a~^ | ~})" (mapcar #'formula-tptp (cdr l))))
    ('=> (format nil "(~a => ~a)" (formula-tptp (cadr l)) (formula-tptp (caddr l))))
    ('<=> (format nil "(~a <=> ~a)" (formula-tptp (cadr l)) (formula-tptp (caddr l))))
    ('equal (format nil "(~a = ~a)"  (formula-tptp (cadr l)) (formula-tptp (caddr l))))))

(defun unary-logical-formula-tptp (l)
  (let ((op (ecase (car l)
              ('not "~"))))
    (format nil "(~a ~a)" op (formula-tptp (cadr l)))))

(defun atom-tptp (a)
  (cond 
    ((eq 'True a) "$true")
    ((eq 'False a) "$false")
    ((numberp a) (write-to-string a))
    ((stringp a) (format nil "'~a'" (escape-quotes (remove #\return (remove #\newline a)))))
    ((regular-varp a) (variable-tptp a))
    (t (let ((name (symbol-name a)))
         (if (relationp a)
             (replace-special-chars (format nil "s_~a_m" name))
             (replace-special-chars (format nil "s_~a" name)))))))

(defun relation-name-tptp (r)
  (case r
    ('<= "lesseq")
    ('< "less")
    ('> "greater")
    ('>= "greatereq")
    ('lessThanOrEqualTo "lesseq")
    ('lessThan "less")
    ('greaterThan "greater")
    ('greatherThanOrEqualTo "greatereq")
    ('MultiplicationFn "times")
    ('DivisionFn "divide")
    ('AdditionFn "plus")
    ('SutractionFn "minus")
    (otherwise (format nil "s_~a" (symbol-name r)))))

(defun predicate-tptp (l)
  (format nil "~a(~{~a~^, ~})" (relation-name-tptp (car l)) (mapcar #'formula-tptp (cdr l))))

(defun formula-tptp (l)
  (cond 
    ((atom l) (atom-tptp l))
    ((quantifierp l) (quantifier-tptp (car l) (cadr l) (caddr l)))
    ((binary-logical-formulap l) (binary-logical-formula-tptp l))
    ((unary-logical-formulap l) (unary-logical-formula-tptp l))
    (t (predicate-tptp l))))

(defun variable-tptp (variable)
  (replace-special-chars (string-upcase (subseq (symbol-name variable) 1))))

(defun atoms-tptp (atoms)
  (format nil "~{~a~^,~}" (mapcar #'atom-tptp atoms)))

(defun quantifier-tptp (quantifier variables formula)
  (let ((fmt (ecase quantifier 
               ('forall "! [~a] : (~a)") 
               ('exists "? [~a] : (~a)"))))
   (format nil fmt (atoms-tptp variables) (formula-tptp formula))))

(defun can-translate-to-FOF (f &optional ctx)
  "Checks if F is a traditional first-order logic formula that is
supported by the FOF variant of TPTP."
  (cond
    ((atom f) (not (member f '(True False))))
    ((quantifier-termp (car f)) (every #'identity (mapcar (lambda (x) (can-translate-to-FOF x ctx)) (cddr f))))
    ((logical-operatorp (car f))
     (unless (member :predicate ctx)
       (every #'identity (mapcar (lambda (x) (can-translate-to-FOF x (cons :logic ctx))) (cdr f)))))
    ((and (relationp (car f)) (not (kif-functionp (car f))))
     (unless (or (member :function ctx) (member :predicate ctx)) 
       (every #'identity (mapcar (lambda (x) (can-translate-to-FOF x (cons :predicate ctx))) (cdr f)))))
    ((kif-functionp (car f))
     (unless (eq :logic (car ctx))
       (every #'identity (mapcar (lambda (x) (can-translate-to-FOF x (cons :function ctx))) (cdr f)))))
    (t (and (can-translate-to-FOF (car f) ctx) (can-translate-to-FOF (cdr f) ctx)))))

(defun kif-tptp (file formulas &optional (statement-type "axiom"))
  (with-output-to-file (out file :if-exists :supersede)
    (let ((count 0))
      (dolist (f formulas)
        (format out "~%/*~%~a~%*/~%" f)
        (if (can-translate-to-FOF f)
            (format out "fof(a~a,~a,~a).~%" (incf count) statement-type (formula-tptp f))
            (format out "%% no translation to TPTP/FOF available.~%"))))))

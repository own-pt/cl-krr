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

(defun row-varp (s)
  "Checks if S is a row variable, i.e., starts with @."
  (and (symbolp s) (eq #\@ (elt (symbol-name s) 0))))

(defun regular-varp (s)
  "Checks if S is a regular variable, i.e., starts with ?."
  (and (symbolp s) (eq #\? (elt (symbol-name s) 0))))

(defun variablep (s)
  (or (regular-varp s) (row-varp s)))

(defun quantifier-termp (s)
  (member s +quantifiers+))

(defun quantifierp (f)
  "Check if the formula fragment is a quantifier."
  (and (consp f) (or (eq 'exists (car f)) (eq 'forall (car f)))))

(defun relationp (s)
  "Checks if S is a relation."
  (let ((types (gethash (topmost-relation s) *instances*)))
    (some #'identity (mapcar (lambda (x) (subclassp x 'Relation)) types))))

(defun kif-functionp (s)
  "Checks if S is a function."
  (let ((types (gethash (topmost-relation s) *instances*)))
    (or
     (member 'Function types)
     (some #'identity (mapcar (lambda (x) (subclassp x 'Function)) types)))))

(defun strictly-relationp (s)
  "Checks if S is strictly a relation (i.e., not a function)."
  (and (relationp s) (not (kif-functionp s))))

(defun logical-operatorp (s)
  (or (member s +binary-logical-operators+)
      (member s +unary-logical-operators+)))

(defun binary-logical-formulap (formula)
  (or (member (car formula) +binary-logical-operators+)
      (eq (car formula) 'equal)))

(defun unary-logical-formulap (formula)
  (member (car formula) +unary-logical-operators+))

;; TODO: rewrite this, since we are already doing things that are
;; specific to SUO-KIF.  We should receive different functions for
;; different fragments found (such as logical operators, predicates,
;; quantifiers, etc.)
(defun map-fold (l map-fn &optional fold-fn) 
  "MAP/FOLD over formulas.  Applies MAP-FN over all instances of
   either atoms of trivial formulas in L.  Join the results via the
   binary function FOLD-FN.  Mostly recurse down the tree, but care
   needs to be taken when finding quantifiers."
  (cond 
    ((atom l) (funcall map-fn l))
    ((quantifierp l) (map-fold (caddr l) map-fn fold-fn)) 
    ((trivialp l)       
     (funcall map-fn l))
    (t
     (if fold-fn
         (funcall fold-fn
                  (map-fold (car l) map-fn fold-fn)
                  (map-fold (cdr l) map-fn fold-fn))
         (progn
           (map-fold (car l) map-fn fold-fn)
           (map-fold (cdr l) map-fn fold-fn))))))

;; http://www.rosettacode.org/wiki/Time_a_function#Common_Lisp
(defun timings (function)
  (let ((real-base (get-internal-real-time))
        (run-base (get-internal-run-time)))
    (funcall function)
    (values (/ (- (get-internal-real-time) real-base) internal-time-units-per-second)
            (/ (- (get-internal-run-time) run-base) internal-time-units-per-second))))

;; http://norvig.com/paip/auxfns.lisp
;; http://norvig.com/license.html
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

;; http://norvig.com/paip/auxfns.lisp
;; http://norvig.com/license.html
(setf (symbol-function 'find-all-if) #'remove-if-not)

;; http://norvig.com/paip/auxfns.lisp
;; http://norvig.com/license.html
(defun rassoc-all (item alist)
  (find-all item alist :key #'cdr))

(defun distribute-alist (l)
  "Takes an alist where the values are lists and converts it into all
  the different tuples associated with the keys.  For example:

  ((A (1 2 3)) (B (4 5 6)))

  will be transformed into:

  (((A . 1) (A . 2) (A . 3)) ((B . 4) (B . 5) (B . 6)))"
  (mapcar (lambda (x)
            (mapcar (lambda (y)
                      (cons (car x) y)) (cadr x))) l))

(defun trivialp (l)
  "Check is list L is 'trivial', i.e., all its elements are atoms."
  (every #'identity (mapcar #'atom l)))

(defun bubble-up (x y)
  "To be used in MAP-FOLD operations.  The idea is to 'bubble up' a
   value that is found deep inside a tree after applying a map
   function.  If both X and Y are not NIL, return a CONS.  Otherwise
   return the non-NIL value."
  (if (and x y)
      (union (if (consp x) x (list x)) (if (consp y) y (list y)))
      (or x y)))

;; http://stackoverflow.com/questions/3693323/how-do-i-manipulate-parse-trees
(defun treemap (tree matcher transformer)
  (cond ((null tree) nil)
        ((consp tree)
         (if (funcall matcher tree)
             (funcall transformer tree)
           (cons (first tree)
                 (mapcar (lambda (child)
                           (treemap child matcher transformer))
                         (rest tree)))))
        (t tree)))

(defun escape-quotes (string)
  (regex-replace-all "'" string '("\\" :match)))

(defun replace-special-chars (string)
  (regex-replace-all "-" string "_"))

(defun union* (list-of-lists)
  (let ((r))
    (dolist (l list-of-lists)
      (setf r (union r l)))
    r))

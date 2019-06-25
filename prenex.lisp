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

(defun gen-variable ()
  (symbolicate "?" (gensym)))

(defun rename-variables (v e) 
  (let ((bindings (mapcar (lambda (x) (cons x (gen-variable))) v)))
    (sublis bindings e)))

(named-readtables:in-readtable :fare-quasiquote)  

(defun prenex1 (f)
  "One step of the conversion to prenex normal form.  Returns NIL if
   no pattern matches."
  (flet ((tr1* (v e pat)
           "Expands the variables (V) and expression (E) to the
            appropriate values in patern PAT.  Returns NIL if no
            substituation was performed.."
           (let* ((p (prenex1 e))
                  (bindings (pairlis '(V E) `(,v ,p))))
             (when p (sublis bindings pat))))
         (tr2 (v e1 e2 ren pat)
           "Expands the variables (V) and expressions (E1 E2) to the
            appropriate values in pattern PAT.  REN dictates whether
            E1 or E2 will have its internal variables renamed."
           (let* ((r1 (if (eq ren :e1) (rename-variables v e1) e1))
                  (r2 (if (eq ren :e2) (rename-variables v e2) e2))
                  (p1 (prenex1 r1))
                  (p2 (prenex1 r2))
                  (bindings (pairlis '(V E1 E2) `(,v ,(or p1 r1) ,(or p2 r2)))))
             (sublis bindings pat)))
         (tr2* (e1 e2 pat)
           "This is a special form of the TR2 transform that returns
            NIL if no changes were made.  We use this to stop the
            recusion when we match (=> e1 e2) (and e1 e2), etc."
           (let* ((p1 (prenex1 e1))
                  (p2 (prenex1 e2))
                  (bindings (pairlis '(E1 E2) `(,(or p1 e1) ,(or p2 e2)))))
             (when (or p1 p2) (sublis bindings pat)))))
    (match f
      (`(<=> (|exists| ,v ,e1) ,e2) 
        `(|and| (=> (|exists| ,v ,e1) ,e2)
              (=> ,e2 (|exists| ,v ,e1))))

      (`(<=> ,e1 (|exists| ,v ,e2))
        `(|and| (=> ,e1 (|exists| ,v ,e2))
              (=> (|exists| ,v ,e2) ,e1)))

      (`(<=> (|forall| ,v ,e1) ,e2)
        `(|and| (=> (|forall| ,v ,e1) ,e2)
              (=> ,e2 (|forall| ,v ,e1))))

      (`(<=> ,e1 (|forall| ,v ,e2))
        `(|and| (=> ,e1 (|forall| ,v ,e2))
              (=> (|forall| ,v ,e2) ,e1)))

      (`(=> (|forall| ,v ,e1) ,e2) (tr2 v e1 e2 :e2 '(|exists| V (=> E1 E2))))
      (`(=> (|exists| ,v ,e1) ,e2) (tr2 v e1 e2 :e2 '(|forall| V (=> E1 E2))))
      (`(=> ,e1 (|exists| ,v ,e2)) (tr2 v e1 e2 :e1 '(|exists| V (=> E1 E2))))
      (`(=> ,e1 (|forall| ,v ,e2)) (tr2 v e1 e2 :e1 '(|forall| V (=> E1 E2))))

      (`(|not| (|exists| ,v ,e)) `(|forall| ,v (|not| ,e)))
      (`(|not| (|forall| ,v ,e)) `(|exists| ,v (|not| ,e)))

      (`(|or| ,e1 (|exists| ,v ,e2)) (tr2 v e1 e2 :e1 '(|exists| V (|or| E1 E2))))
      (`(|or| ,e1 (|forall| ,v ,e2)) (tr2 v e1 e2 :e1 '(|forall| V (|or| E1 E2))))
      (`(|or| (|exists| ,v ,e1) ,e2) (tr2 v e1 e2 :e2 '(|exists| V (|or| E1 E2))))
      (`(|or| (|forall| ,v ,e1) ,e2) (tr2 v e1 e2 :e2 '(|forall| V (|or| E1 E2))))

      (`(|and| ,e1 (|forall| ,v ,e2)) (tr2 v e1 e2 :e1 '(|forall| V (|and| E1 E2))))
      (`(|and| (|forall| ,v ,e1) ,e2) (tr2 v e1 e2 :e2 '(|forall| V (|and| E1 E2))))
      (`(|and| ,e1 (|exists| ,v ,e2)) (tr2 v e1 e2 :e1 '(|exists| V (|and| E1 E2))))
      (`(|and| (|exists| ,v ,e1) ,e2) (tr2 v e1 e2 :e2 '(|exists| V (|and| E1 E2))))

      ;; these are the "end of the recursion" matches.  they use the
      ;; special forms tr1* and tr2* that return NIL if there is no
      ;; further substitution made.
      (`(|exists| ,v ,e) (tr1* v e '(|exists| V E)))
      (`(|forall| ,v ,e) (tr1* v e '(|forall| V E)))

      (`(<=> ,e1 ,e2) (tr2* e1 e2 '(<=> E1 E2)))
      (`(=> ,e1 ,e2)  (tr2* e1 e2  '(=> E1 E2)))
      (`(|or| ,e1 ,e2)  (tr2* e1 e2  '(|or| E1 E2)))
      (`(|and| ,e1 ,e2) (tr2* e1 e2 '(|and| E1 E2)))
      (otherwise nil))))

(named-readtables:in-readtable :standard)

(defun prenex (f)
  "Convert F to prenex normal form."
  (do* ((tr2 (prenex1 f))
        (tr1 f))
       ((null tr2) tr1)
    (setf tr1 tr2)
    (setf tr2 (prenex1 tr1))))

(defun binarize (f)
  "Convert n-arity AND and OR predicates into their binary form."
  (flet ((and-or-p (f)
           (member (car f)  '(|or| |and|))))
    (cond
      ((atom f) f)
      ((and (and-or-p f) (> (length f) 3))
       `(,(car f) ,(binarize (cadr f)) ,(binarize `(,(car f) ,@(cddr f)))))
      (t 
       `(,(car f) ,@(mapcar #'binarize (cdr f)))))))


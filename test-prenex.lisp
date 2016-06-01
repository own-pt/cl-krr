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

;; TODO: we need test cases to cover these patterns:

;; (`(exists ,v ,e) (tr1* v e '(exists V E)))
;; (`(forall ,v ,e) (tr1* v e '(forall V E)))
;; (`(<=> ,e1 ,e2) (tr2* nil e1 e2 :none '(<=> E1 E2)))
;; (`(=> ,e1 ,e2) (tr2* nil e1 e2 :none '(=> E1 E2)))
;; (`(or ,e1 ,e2) (tr2* nil e1 e2 :none '(or E1 E2)))
;; (`(and ,e1 ,e2) (tr2* nil e1 e2 :none '(and E1 E2)))

(defun test-prenex ()
  "generate one TPTP file for each of the cases in FORMULAS.

the TPTP is of the form F1 <=> F2, where F1 is the original formula
and F2 is the formula in PNF.  You can then use an ATP to prove if
this is a tautology or not.
"
  (let ((formulas 
         '(
           (exists (?X) (and (foo ?X) (exists (?Y) (bar ?Y))))
           (forall (?X) (and (foo ?X) (forall (?Y) (bar ?Y))))
           
           (<=> 
            (and (exists (?X) (foo1 ?X)) (bar1 ?Y))
            (and (exists (?X) (foo2 ?X)) (bar2 ?Y)))

           (<=> 
            (and (forall (?X) (foo1 ?X)) (bar1 ?Y))
            (and (forall (?X) (foo2 ?X)) (bar2 ?Y)))

           (=> 
            (and (exists (?X) (foo1 ?X)) (bar1 ?Y))
            (and (exists (?X) (foo2 ?X)) (bar2 ?Y)))

           (=> 
            (and (forall (?X) (foo1 ?X)) (bar1 ?Y))
            (and (forall (?X) (foo2 ?X)) (bar2 ?Y)))

           (and
            (and (exists (?X) (foo1 ?X)) (bar1 ?Y))
            (and (exists (?X) (foo2 ?X)) (bar2 ?Y)))

           (and
            (and (forall (?X) (foo1 ?X)) (bar1 ?Y))
            (and (forall (?X) (foo2 ?X)) (bar2 ?Y)))

           (or
            (and (exists (?X) (foo1 ?X)) (bar1 ?Y))
            (and (exists (?X) (foo2 ?X)) (bar2 ?Y)))

           (or
            (and (forall (?X) (foo1 ?X)) (bar1 ?Y))
            (and (forall (?X) (foo2 ?X)) (bar2 ?Y)))


           (<=> (exists (?X) (foo ?X)) (bar ?Y))
           (<=> (bar ?Y) (exists (?X) (foo ?X)))
           (<=> (forall (?X) (foo ?X)) (bar ?Y))
           (<=> (bar ?Y) (forall (?X) (foo ?X)))
           (=> (forall (?X) (foo ?X)) (bar ?Y))
           (=> (exists (?X) (foo ?X)) (bar ?Y))
           (=> (bar ?Y) (forall (?X) (foo ?X)))
           (=> (bar ?Y) (exists (?X) (foo ?X)))
           (not (exists (?X) (foo ?X)))
           (not (forall (?X) (foo ?X)))
           (or (bar ?Y) (exists (?X) (foo ?X)))
           (or (bar ?Y) (forall (?X) (foo ?X)))
           (or (exists (?X) (foo ?X)) (bar ?Y))
           (or (forall (?X) (foo ?X)) (bar ?Y))
           (and (bar ?Y) (exists (?X) (foo ?X)))
           (and (bar ?Y) (forall (?X) (foo ?X)))
           (and (exists (?X) (foo ?X)) (bar ?Y))
           (and (forall (?X) (foo ?X)) (bar ?Y))))
        (c 0))
    (dolist (f formulas)
      (with-output-to-file (s (format nil "c~a.tptp" (incf c)))
        (format s "fof(c,conjecture,(~a) <=> (~a)).~%"
                (formula-tptp f) (formula-tptp (prenex f)))))))


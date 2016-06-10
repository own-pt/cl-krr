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

(fiveam:def-suite kif-suite)
(fiveam:def-suite tptp-translation)

(fiveam:in-suite tptp-translation)

(fiveam:test variable-tptp-0 
  (fiveam:is (equal "XA" (variable-tptp '?XA))))

(fiveam:test variable-tptp-1
  (fiveam:is (equal "XA" (variable-tptp '?xa))))

(fiveam:test variable-list-tptp-0
  (fiveam:is (equal "XA,XB,XC" (variable-list-tptp '(?xa ?xb ?xc)))))

;; these tests need (compile-suo-kif "samples/merge.kif") to be executed
(fiveam:in-suite kif-suite)

(fiveam:test find-all-variable-arity-predicates
  (fiveam:is (set-equal '(contraryAttribute exhaustiveAttribute exhaustiveDecomposition
                   disjointDecomposition partition AssignmentFn ListFn
                   GreatestCommonDivisorFn LeastCommonMultipleFn) *variable-arity-relations*)))

(fiveam:test is-simple-formula1
  (fiveam:is (trivialp '(instance exhaustiveDecomposition Predicate))) )

(fiveam:test is-simple-formula2
  (fiveam:is (not (trivialp '(=> (and (instance ?REL CaseRole) (instance ?OBJ Object) (?REL ?PROCESS ?OBJ)) (exists (?TIME) (overlapsSpatially (WhereFn ?PROCESS ?TIME) ?OBJ)))))))

(fiveam:test find-instances-of-COLORATTRIBUTE
  (fiveam:is (set-equal '(Monochromatic Polychromatic) (find-instances-of '(ColorAttribute)))))

(fiveam:test domains-of-INSTANCE
  (fiveam:is (equal '(Entity SetOrClass) (gethash 'instance *domains*))))

(fiveam:test domains-of-DOMAIN1
  (fiveam:is (equal '(Relation PositiveInteger SetOrClass) (gethash 'domain *domains*))))

(fiveam:test domains-of-DOMAIN2
  (fiveam:is (equal '(Attribute+ Attribute) (gethash 'exhaustiveAttribute *domains*))))

(fiveam:test infer-variable-type-simple-formula1
  (fiveam:is (equal '(Attribute+) (infer-variable-type '?X '(exhaustiveAttribute ?X ?Y)))))

(fiveam:test infer-variable-type-simple-formula2
  (fiveam:is (equal '(Physical) (infer-variable-type '?PROCESS '(WhereFn ?PROCESS ?TIME)))))

(fiveam:test infer-variable-type-complex-formula1
  (fiveam:is (equal '(Physical) (infer-variable-type '?PROCESS '(=> (and (instance ?REL CaseRole) (instance ?OBJ Object) (?REL ?PROCESS ?OBJ)) (exists (?TIME) (overlapsSpatially (WhereFn ?PROCESS ?TIME) ?OBJ)))))))

(fiveam:test infer-variable-type-complex-formula2
  (fiveam:is (equal '(TransitiveRelation) (infer-variable-type '?REL '(<=> (instance ?REL TransitiveRelation) (forall (?INST1 ?INST2 ?INST3) (=> (and (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST3)) (?REL ?INST1 ?INST3))))))))

(fiveam:test infer-variable-type-complex-formula3
  (fiveam:is (eq nil (infer-variable-type '?INST1 '(<=> (instance ?REL TransitiveRelation) (forall (?INST1 ?INST2 ?INST3) (=> (and (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST3)) (?REL ?INST1 ?INST3))))))))

(fiveam:test infer-variable-type-complex-formula4
  (fiveam:is (eq nil (infer-variable-type '?INST2 '(<=> (instance ?REL TransitiveRelation) (forall (?INST1 ?INST2 ?INST3) (=> (and (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST3)) (?REL ?INST1 ?INST3))))))))

(fiveam:test infer-variable-type-with-superclass1 
  (fiveam:is (equal '(Predicate) (infer-variable-type '?REL1 '(=>
                                                        (and
                                                         (subrelation ?REL1 ?REL2)
                                                         (instance ?REL1 Predicate)
                                                         (instance ?REL2 Predicate)
                                                         (?REL1 @ROW))
                                                        (?REL2 @ROW))))))

(fiveam:test infer-variable-type-with-superclass2
  (fiveam:is (equal '(Predicate) (infer-variable-type '?REL2 '(=>
                                                     (and
                                                      (subrelation ?REL1 ?REL2)
                                                      (instance ?REL1 Predicate)
                                                      (instance ?REL2 Predicate)
                                                      (?REL1 @ROW))
                                                     (?REL2 @ROW))))))


(fiveam:test infer-variable-type-complex-formula5
  (fiveam:is (eq nil (infer-variable-type '?INST3 '(<=> (instance ?REL TransitiveRelation) (forall (?INST1 ?INST2 ?INST3) (=> (and (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST3)) (?REL ?INST1 ?INST3))))))))

(fiveam:test find-row-vars4
  (fiveam:is (= 0 (hash-table-count (find-row-vars '(DOMAINSUBCLASS MAKINGFN 1 MAKING))))))

(fiveam:test rename-variable-arity-relations1
  (fiveam:is (equal '(<=>
               (disjointDecomposition4 ?CLASS ?ROW1 ?ROW2 ?ROW3)
               (and
                (disjoint ?ROW1 ?ROW2)
                (disjoint ?ROW2 ?ROW3)
                (disjoint ?ROW3 ?ROW1)))
             (rename-variable-arity-relations '(<=>
                                                (disjointDecomposition ?CLASS ?ROW1 ?ROW2 ?ROW3)
                                                (and
                                                 (disjoint ?ROW1 ?ROW2)
                                                 (disjoint ?ROW2 ?ROW3)
                                                 (disjoint ?ROW3 ?ROW1)))))))

(fiveam:test rename-variable-arity-relations2
  (fiveam:is (equal '(<=>
               (disjointDecomposition4 ?CLASS ?ROW1 ?ROW2 ?ROW3)
               (and
                (disjointDecomposition1 ?CLASS)
                (disjoint ?ROW1 ?ROW2)
                (disjoint ?ROW2 ?ROW3)
                (disjoint ?ROW3 ?ROW1)))
             (rename-variable-arity-relations '(<=>
                                                (disjointDecomposition ?CLASS ?ROW1 ?ROW2 ?ROW3)
                                                (and
                                                 (disjointDecomposition ?CLASS)
                                                 (disjoint ?ROW1 ?ROW2)
                                                 (disjoint ?ROW2 ?ROW3)
                                                 (disjoint ?ROW3 ?ROW1)))))))

(fiveam:test find-predicate-vars0 
  (fiveam:is (= 0 (hash-table-count (find-predicate-vars '(exists (?X) (philosofer ?X)))))))

;;; under construction below this line

;;; TODO: convert these into test cases!
;;; case where there are no row variables
(defun test-row-variables-in-quantifiers ()
  (let ((formula '(forall (@ROW ?ITEM)
                   (equal 
                    (ListLengthFn (ListFn @ROW ?ITEM)) 
                    (SuccessorFn (ListLengthFn (ListFn @ROW)))))))
    (expand-row-vars formula)))

(defun test-expand-row-vars0 ()
  (expand-row-vars '(=>
                     (and
                      (range ?FUNCTION ?CLASS)
                      (equal (AssignmentFn ?FUNCTION ?ROW) ?VALUE))
                     (instance ?VALUE ?CLASS))))

;;; one row variable, variable arity
(defun test-expand-row-vars1 ()
  (expand-row-vars '(=>
                     (and
                      (range ?FUNCTION ?CLASS)
                      (equal (AssignmentFn ?FUNCTION @ROW) ?VALUE))
                     (instance ?VALUE ?CLASS))))

;;; two row variables, variable arity
(defun test-expand-row-vars2 ()
  (expand-row-vars '(=> (and (contraryAttribute @ROW1) 
                         (identicalListItems 
                          (ListFn @ROW1) (ListFn @ROW2)))
                     (contraryAttribute @ROW2))))

;;; one row varibable, fixed arity
(defun test-expand-row-vars3 ()
  (expand-row-vars '(and (contraryAttribute @ROW1) (instance @ROW1))))

;;; two row variables, mixed arities
(defun test-expand-row-vars4 ()
  (expand-row-vars '(and (contraryAttribute @ROW1) (instance @ROW2))))

(defun test-expand-row-vars5 ()
  (let ((formula '(=>
                   (and
                    (instance ?REL IntentionalRelation)
                    (inScopeOfInterest ?AGENT @ROW)
                    (inList ?OBJ (ListFn @ROW)))
                   (inScopeOfInterest ?AGENT ?OBJ))))
    (expand-row-vars formula)))

(defun test-expand-row-vars5b ()
  (let ((formula '(forall (@ROW) (=>
                              (and
                               (instance ?REL IntentionalRelation)
                               (inList ?OBJ (ListFn @ROW))
                               (inScopeOfInterest ?AGENT @ROW))
                              (inScopeOfInterest ?AGENT ?OBJ)))))
    (expand-row-vars formula)))

(defun test-expand-row-vars6 ()
  (expand-row-vars '(forall (@ROW ?ITEM)
                     (equal 
                      (ListLengthFn (ListFn @ROW ?ITEM)) 
                      (SuccessorFn (ListLengthFn (ListFn @ROW)))))))

(defun test-expand-row-vars7 ()
  (expand-row-vars '(forall (@ROW ?ITEM)
                     (equal 
                      (ListOrderFn 
                       (ListFn @ROW ?ITEM) 
                       (ListLengthFn (ListFn @ROW ?ITEM))) ?ITEM))))

;; TODO: implement this!
;; 
;; (defun alist-equal (h1 h2) nil)

;; (fiveam:test find-row-vars1
;;   (fiveam:is (alist-equal '((@ROW (AssignmentFn . 1)))  (hash-table-alist (find-row-vars '(=> (and (range ?FUNCTION ?CLASS) (equal (AssignmentFn ?FUNCTION @ROW) ?VALUE)) (instance ?VALUE ?CLASS)))))))

;; (fiveam:test find-row-vars2
;;   (fiveam:is (alist-equal '((@ROW1 (contraryAttribute . 0) (ListFn . 0)) (@ROW2 (ListFn . 0) (contraryAttribute . 0))) (hash-table-alist (find-row-vars '(=> (and (contraryAttribute @ROW1) (identicalListItems (ListFn @ROW1) (ListFn @ROW2))) (contraryAttribute @ROW2)))))))

;; (fiveam:test find-row-vars3
;;   (fiveam:is (alist-equal '((?REL1 @ROW 0) (?REL2 @ROW 0)) (find-row-vars '(=> (and (subrelation ?REL1 ?REL2) (instance ?REL1 Predicate) (instance ?REL2 Predicate) (?REL1 @ROW)) (?REL2 @ROW))))))

(defun find-predicate-vars1 ()
  (hash-table-alist (find-predicate-vars '(=>
                                           (and
                                            (subrelation ?REL1 ?REL2)
                                            (instance ?REL1 Predicate)
                                            (instance ?REL2 Predicate)
                                            (?REL1 ?X ?Y))
                                           (?REL2 ?X ?Y ?Z)))))

;; (defun foo2 ()
;;   (hash-table-alist (find-predicate-vars '(=>
;;                                            (and
;;                                             (subrelation ?REL1 ?REL2)
;;                                             (instance ?REL1 Predicate)
;;                                             (instance ?REL2 Predicate)
;;                                             (?REL1 1 2 3))
;;                                            (?REL2 1 2 3)))))

;; (defun foo ()
;;   (expand-predicate-vars '(=>
;;                            (and
;;                             (subrelation ?REL1 ?REL2)
;;                             (instance ?REL1 Predicate)
;;                             (instance ?REL2 Predicate)
;;                             (?REL1 1 2 3 4))
;;                            (?REL2 1 2 3 4))))

;; (defun test-find-regular-vars ()
;;   (let ((formula '(=>
;;                    (and
;;                     (instance ?TRANSFER Transfer)
;;                     (agent ?TRANSFER ?AGENT)
;;                     (patient ?TRANSFER ?PATIENT))
;;                    (not
;;                     (equal ?AGENT ?PATIENT)))))
;;     (find-regular-vars formula)))

;; (defun test-quantify-free-variables ()
;;   (let ((formula '(=> (foo ?X ?Y))))
;;     (find-free-variables formula)))

;; (defun test-micro-sumo ()
;;   (let ((formula '(=> 
;;                    (instance ?X BinaryPredicate)
;;                    (and (?X ?A ?B) (?X ?B ?A)))))
;;     (dolist (f (expand-predicate-vars formula))
;;       (print f))))

;; (defun test-get-explicit-type0 ()
;;   (let ((formula '((or (instance ?X Predicate) (instance ?X Foo) (instance ?X Bar)))))
;;     (get-explicit-types '?X formula)))

;; (defun test-quantify-free-variables ()
;;   (let ((formula '(=> (instance ?X OrganismRemains)
;;                    (holdsDuring (WhenFn ?X) (attribute ?X Dead))) 
;;           (subclass OrganismRemains OrganicObject)))
;;     (find-free-variables formula)))

;; (defun test-can-translate-to-TPTP ()
;;   (let ((formula '(forall (?X)
;;                    (=> (and (instance ?X HumanCorpse))
;;                     (=> (instance ?X HumanCorpse) (instance ?X (DeadFn Human)))))))
;;     (can-translate-to-TPTP formula)))

(defun test-predicates-in-var ()
  (let ((formula '(<=>
                   (and
                    (instance ?REL TotalValuedRelation)
                    (instance ?REL Predicate))
                   (exists (?VALENCE)
                    (and
                     (instance ?REL Relation)
                     (valence ?REL ?VALENCE)
                     (=>
                      (forall (?NUMBER ?ELEMENT ?CLASS)
                              (=>
                               (and
                                (lessThan ?NUMBER ?VALENCE)
                                (domain ?REL ?NUMBER ?CLASS)
                                (equal ?ELEMENT (ListOrderFn (ListFn @ROW) ?NUMBER)))
                               (instance ?ELEMENT ?CLASS)))
                      (exists (?ITEM)
                              (?REL @ROW ?ITEM))))))))
    (expand-predicate-vars formula)))


(defun test-vars-in-predicates ()
  (let ((formula '(=>
                   (and (holdsDuring ?INTERVAL (?REL ?INST1 ?INST2))
                    (instance ?INST1 Physical) (instance ?INST2 Physical))
                   (and (time ?INST1 ?INTERVAL) (time ?INST2 ?INTERVAL)))))
    (find-predicate-vars formula)))

(defun test-relativize1 ()
  (let ((formula '(=>
                   (and
                    (instance ?TRANSFER Transfer)
                    (agent ?TRANSFER ?AGENT)
                    (patient ?TRANSFER ?PATIENT))
                   (not
                    (equal ?AGENT ?PATIENT)))))
    (relativize-formula formula)))

(defun test-relativize2 ()
  (let ((formula '(=>
                   (and (instance ?REM (DeadFn ?ORGTYPE))
                    (instance ?DEATH Death) (result ?DEATH ?REM)
                    (experiencer ?DEATH ?ORG))
                   (instance ?ORG ?ORGTYPE))))
    (relativize-formula formula)))

(defun test-relativize3 ()
  (let ((formula '(=>
                   (instance subclass TransitiveRelation)
                   (forall (?INST1 ?INST2 ?INST3)
                    (=>
                     (and
                      (subclass ?INST1 ?INST2)
                      (subclass ?INST2 ?INST3))
                     (subclass ?INST1 ?INST3))))))
    (relativize-formula formula)))

(defun test-relativize4 ()
  (relativize-formula '(=>
         (immediateSubclass ?CLASS1 ?CLASS2)
         (not (exists (?CLASS3)
               (and
                (subclass ?CLASS3 ?CLASS2)
                (subclass ?CLASS1 ?CLASS3)
                (not (equal ?CLASS2 ?CLASS3))
                (not (equal ?CLASS1 ?CLASS3))))))))

(defun test-relativize5 ()
  (let ((formula '(and
                   (forall (?A ?B)
                    (subclass ?A ?B))
                   (subclass ?X ?Y))))
    (relativize-formula formula)))

(defun test-relativize6 ()
  (let ((formula '(forall (?A) 
                   (exists (?B)
                    (equal ?A ?B)))))
    (relativize-formula formula)))

(defun test-relativize7 ()
  (relativize-formula '(subclass OrganismRemains OrganicObject)))

(defun test-relativize8 ()
  (relativize-formula '(=>
                        (and (subrelation contraryAttribute contraryAttribute)
                         (instance contraryAttribute Predicate)
                         (contraryAttribute7 ?ROW0 ?ROW1 ?ROW2 ?ROW3 ?ROW4 ?ROW5 ?ROW6))
                        (contraryAttribute7 ?ROW0 ?ROW1 ?ROW2 ?ROW3 ?ROW4 ?ROW5 ?ROW6))))


(defun test-relativize9 ()
  (relativize-formula '(holdsDuring ?T2 (part ?OBJ ?REM))))

(defun test-can-translate0 ()
  (can-translate-to-FOL '(=>
                           (equal (GreatestCommonDivisorFn @ROW) ?NUMBER)
                           (forall (?ELEMENT)
                            (=>
                             (inList ?ELEMENT (ListFn @ROW))
                             (equal (RemainderFn ?ELEMENT ?NUMBER) 0))))))

(defun test-can-translate1 ()
  (can-translate-to-FOL '(=>
                           (GreatestCommonDivisorFn @ROW)
                           (=>
                            (inList ?ELEMENT
                             (ListFn @ROW))
                            (instance ?ELEMENT Number)))))

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

(defun transitive-closure-classes (classes)
  "Generates the transitive closure of the subclass relation.  So,
if (subclass C1 C2) and (subclass C2 C3) then generate (subclass C1
C3)."
  (let ((f))
    (dolist (c classes)
      (dolist (sc (find-class-parents c))
        (push `(subclass ,c ,sc) f)))
    f))

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
    f))

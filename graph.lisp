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

(defun walk-form (frm fn)
  (cond
    ((symbolp frm) (when frm (funcall fn frm)))
    ((consp frm)
     (walk-form (car frm) fn)
     (walk-form (cdr frm) fn))))

(defun precompute-graph (kb)
  "Computes a graph from the list of formulas in KB. We define the
  graph as follows:

   nodes => relation symbols

   edges => if symbol A and B are used in the same formula, there is a
   edge between A and B."
  (flet ((common-relation (x)
           (member x '(instance subclass subrelation domain))))
    (let ((h (make-hash-table :test #'equal)))
      (dolist (f kb)
        (let ((l))
          (walk-form f (lambda (x) (when (and (relationp x) (not (common-relation x))) (push x l))))
          (setf l (remove-duplicates l))
          (dolist (s l)
            (push s (gethash f h))
            (push f (gethash s h)))
          (when (> (length l) 1)
            (map-permutations 
             (lambda (x)
               (let ((n (gethash (car x) h)))
                 (unless (member (cadr x) n)
                   (push (cadr x) (gethash (car x) h)))))
             l :length 2))))
      h)))

(defun print-graph-statistics (kb &aux (graph (precompute-graph kb)))
  (with-output-to-file (s "sumo.txt" :if-exists :supersede)
    (maphash
     (lambda (k v)
       (format s "~a, ~a~%" k (length v)))
     graph)))


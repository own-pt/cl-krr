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

(asdf:defsystem #:suo-kif
  :description "SUO-KIF programming environment, including a TPTP translator."
  :author "Fabricio Chalub <fchalub@br.ibm.com> and Alexandre Rademaker <alexrad@br.ibm.com>"
  :license "Apache 2.0"
  :depends-on (#:cl-fad
               #:alexandria
               #:cl-ppcre
               #:optima
               #:graph-algorithms
               #:fare-quasiquote-optima
               #:fare-quasiquote-readtable
               #:fiveam)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "tptp-fof" :depends-on ("utils"))
               (:file "prenex" :depends-on ("utils"))
               (:file "tests")
               (:file "graph" :depends-on ("utils"))
               (:file "suo-kif" :depends-on ("utils" "prenex" "tptp-fof"))))


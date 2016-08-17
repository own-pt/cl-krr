;; from https://github.com/nilqed/SNARK/blob/master/run-snark

(defvar snark-tptp-options)
(setf snark-tptp-options
      '(
        (agenda-length-limit nil)
        (agenda-length-before-simplification-limit nil)
;;      (use-hyperresolution t)
        (use-resolution t)
;;      (use-term-ordering :recursive-path)
;;      (use-clausification nil)
        (use-ur-resolution t)
        (use-paramodulation t)
        (use-factoring :pos)
        (use-literal-ordering-with-hyperresolution 'literal-ordering-p)
        (use-literal-ordering-with-paramodulation  'literal-ordering-p)
        (ordering-functions>constants t)
        (assert-context :current)
;;      (run-time-limit $runtimelimit)
        (listen-for-commands nil)
        (use-closure-when-satisfiable t)
        (print-rows-when-given nil)
        (print-rows-when-derived nil)
        (print-unorientable-rows nil)
        (print-row-wffs-prettily nil)
        (print-final-rows :tptp)                ;System on TPTP uses value :tptp
        (print-options-when-starting nil)       ;System on TPTP uses this
        (use-variable-name-sorts nil)
        (use-purity-test t)
        (use-relevance-test t)
        (declare-tptp-symbols1)
        (declare-tptp-symbols2)))

(refute-file "$1" :options snark-tptp-options :format :tptp)

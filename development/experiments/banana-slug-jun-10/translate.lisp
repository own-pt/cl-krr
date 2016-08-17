(ql:quickload :suo-kif)
(in-package :suo-kif)
(compile-suo-kif :in-files '("Merge.kif" "banana-slug.kif") :out-file "/tmp/bs.tptp" :subclass-closure t :save-passes nil :tptp t)

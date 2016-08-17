(ql:quickload :suo-kif)
(in-package :suo-kif)
(compile-suo-kif :in-files '("Merge.kif" "Mid-Level-Ontology.kif") :subclass-closure nil :save-passes nil :tptp t)

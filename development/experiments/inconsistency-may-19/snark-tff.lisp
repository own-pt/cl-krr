
;; using types

;; question how to connect the SNARK types with the (instance ...)
;; declared types? We may also declare the relation between the types

(declare-sort 'Hole)
(declare-sort 'SelfConnectedObject)
(declare-sort 'Entity)
(declare-sort 'SetOrClass)

(declare-predicate-symbol 'hole :sort (Hole SelfConnectedObject))
(declare-predicate-symbol 'instance :sort (Entity SetOrClass))

(assert '(forall ((?HOLE Hole) (?OBJ SelfConnectedObject))
	  (implies 
	   (hole ?HOLE ?OBJ)
	   (not 
	    (instance ?OBJ Hole)))))

(assert '(forall ((?HOLE Hole) (?OBJ SelfConnectedObject))
	  (and
	   (implies
	    (instance ?HOLE Hole)
	    (exists ((?OBJ SelfConnectedObject))
		    (hole ?HOLE ?OBJ)))
	   (implies
	    (exists ((?OBJ SelfConnectedObject))
		    (hole ?HOLE ?OBJ))
	    (instance ?HOLE Hole)))))

(assert '(forall ((?TOP SelfConnectedObject) (?S SelfConnectedObject) (?O SelfConnectedObject))
	  (implies
	   (and
	    (top ?TOP ?O)
	    (side ?S ?O))
	   (not
	    (equal ?TOP ?S)))))


(initialize)
(use-resolution t)

(assert '(forall (?OBJ ?HOLE)
	  (implies (and
		    (instance ?OBJ SelfConnectedObject)
		    (instance ?HOLE Hole))
	   (implies (hole ?HOLE ?OBJ)
	    (not (instance ?OBJ Hole))))) :name 'sumo)

(prove '(forall (?OBJ ?HOLE)
	  (implies (implies
		    (and (instance ?HOLE Hole)
			 (instance ?OBJ SelfConnectedObject))
		    (hole ?HOLE ?OBJ))
	   (not (instance ?OBJ Hole)))) :name 'ibm)



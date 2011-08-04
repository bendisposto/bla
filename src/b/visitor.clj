(ns b.visitor
	(:import de.be4.classicalb.core.parser.analysis.DepthFirstAdapter LispTransformer)
	)


(defn create [start]

    (let [sb (StringBuffer.) visitor (
	       proxy [DepthFirstAdapter] []
	        (defaultIn [n] (doto sb (.append "(") (.append "b.core/") (.append (.. n (getClass) (getSimpleName)))))
        	(defaultOut [n] (.append sb ")"))
            (caseAIntegerExpression [n] (doto sb 
					(.append "(b.core/AIntegerExpression ") 
					(.append (.. n (getLiteral) (getText))) 
				    (.append ")")))
	)
	] 
	    (.apply start visitor)
	    (.toString sb)
    )
		
		
)
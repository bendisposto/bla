(ns b.visitor
	(:import de.be4.classicalb.core.parser.analysis.DepthFirstAdapter)
	)


(defn makeId 
	([x]  (list (.getText x)))
    ([x & xs] (conj  (apply makeId xs) (.getText x))))

(defn getId [n] (keyword (apply str  (interpose "." (apply makeId (.getIdentifier n))))))

(defn create [start]

    (let [sb (StringBuffer.) visitor (
	       proxy [DepthFirstAdapter] []
	        (defaultIn [n] (doto sb (.append "(") (.append "b.core/") (.append (.. n (getClass) (getSimpleName)))))
        	(defaultOut [n] (.append sb ")"))
            (caseAIntegerExpression [n] (doto sb 
					(.append "(b.core/AIntegerExpression ") 
					(.append (.. n (getLiteral) (getText))) 
				    (.append ")")))
			(caseAIdentifierExpression [n] 

			(doto sb 
					(.append "(b.core/AIdentifierExpression ") 
					(.append (getId n)) 
					(.append ")")))	
	)
	] 
	    (.apply start visitor)
	    (.toString sb)
    )
		
		
)
(ns b.reader
	(:import de.be4.classicalb.core.parser.BParser)
	(:import de.be4.classicalb.core.parser.analysis.DepthFirstAdapter)
	(:gen-class))

(defn makeId 
	([x]  (list (.getText x)))
    ([x & xs] (conj  (apply makeId xs) (.getText x))))

(defn getId [n] (keyword (apply str  (interpose "." (apply makeId (.getIdentifier n))))))

(defn mk_clojure_ast [start]
    (let [sb (StringBuffer.) 
          visitor (proxy [DepthFirstAdapter] []
	        (defaultIn [n] (doto sb (.append "(") (.append "b.interpreter/") (.append (.. n (getClass) (getSimpleName)))))
        	(defaultOut [n] (.append sb ")"))
            (caseAIntegerExpression [n] (doto sb 
					(.append "(b.interpreter/AIntegerExpression  ") 
					(.append (.. n (getLiteral) (getText))) 
				    (.append ")")))
			(caseAIdentifierExpression [n] (doto sb 
					(.append "(b.interpreter/AIdentifierExpression ") 
					(.append (getId n)) 
					(.append ")"))))
	    ] 
	    (.apply start visitor)
	    (.toString sb)))

(defn bparser [x] (BParser/parse x))

(defn parse [text] (-> text BParser/parse, mk_clojure_ast, clojure.core/read-string))
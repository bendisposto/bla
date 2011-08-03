(ns b.visitor
	(:import LispTransformer))

(defn create [start]

    (let [visitor (LispTransformer.)] 
	    (.apply start visitor)
	    (.getAst visitor)
    )
		
		
)


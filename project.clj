(defproject b "1.0.0"
  :description "Interpreter for B"
  :repositories {"cobra" "http://cobra.cs.uni-duesseldorf.de/artifactory/repo/"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 ;[org.clojure/clojure-contrib "1.2.0"]
                 ;[org.clojure/core.logic "0.6.4"]
                 ;[org.clojure/math.combinatorics "0.0.1"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/core.match "0.2.0-alpha3"]
				 [de.prob/bparser "2.0.58"]
				 [de.prob/parserbase "2.0.58"]               
				 [de.prob/prologlib "2.0.58"]]
  :dev-dependencies [[swank-clojure "1.3.1"]]
  :main b.interpreter)


               
        

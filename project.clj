(defproject b "1.0.0"
  :description "Interpreter for B"
  :repositories {"cobra" "http://cobra.cs.uni-duesseldorf.de/artifactory/repo/"}
  :dependencies [[org.clojure/clojure "1.2.1"]
				 [de.prob/bparser "2.0.58"]
				 [de.prob/parserbase "2.0.58"]               
				 [de.prob/prologlib "2.0.58"]]
  :dev-dependencies [[swank-clojure "1.3.1"]]
  :main b.core)


               
        
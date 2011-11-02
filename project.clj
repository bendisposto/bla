(defproject bla "1.0.0"
  :description "B Lisp Animator, an Interpreter for the B-Method in Clojure"
  :repositories {"cobra" "http://cobra.cs.uni-duesseldorf.de/artifactory/repo/"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/algo.monads "0.1.0"]
		 [de.prob/bparser "2.0.58"]
		 [de.prob/parserbase "2.0.58"]               
		 [de.prob/prologlib "2.0.58"]]
  :dev-dependencies [[swank-clojure "1.3.3"]
                     [midje "1.3-alpha4"]
                     [lein-midje "1.0.4"]
                     [lein-eclipse "1.0.0"]
                     [marginalia "0.7.0-SNAPSHOT"]
                     [lein-marginalia "0.6.1"]]
  :marginalia {:javascript ["http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"]}
  :main b.interpreter)


               
        

(defproject bla "1.0.0"
  :description "B Lisp Animator, an Interpreter for the B-Method in Clojure"
  :repositories {"cobra" "http://cobra.cs.uni-duesseldorf.de/artifactory/repo/"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/core.logic "0.7.4"]
		 [de.prob/bparser "2.4.6-SNAPSHOT"]
		 [de.prob/parserbase "2.4.6-SNAPSHOT"]               
		 [de.prob/prologlib "2.4.6-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.4.0"]
                     [midje "1.3-alpha4"]
                     [lein-midje "1.0.4"]
                     [lein-eclipse "1.0.0"]
                     [marginalia "0.7.0"]
                     [lein-marginalia "0.6.1"]]
  :profiles {:dev {:dependencies [[midje "1.3.2-SNAPSHOT"]] :plugins [[lein-midje "2.0.0-SNAPSHOT"]]}}


  :marginalia {:javascript ["http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"]}
  :main b.interpreter)


               
        

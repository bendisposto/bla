(ns tools.repl
  (:import (java.lang.reflect Modifier Field Method Constructor)) 
  (:require [clojure.string :as str]))

(defn- sortable [t]
  (str/replace t #"\d+" #(format "%04d" (Integer/parseInt %))))

(defn- indexed [l] (map-indexed (fn [x y] [x y]) l)) 

(defn- param-str [m]
  (str " ("
       (str/join ","
                 (map (fn [[c i]]
                    (if (> i 3)
                      (str (.getSimpleName c) "*" i)
                      (str/join ","
                                (replicate i (.getSimpleName c)))))
                  (reduce (fn [pairs y]
                            (let [[x i] (peek pairs)]
                              (if (= x y)
                                (conj (pop pairs) [y (inc i)])
                                (conj pairs [y 1]))))
                          []
                          (.getParameterTypes m)))) ")"))

(defn- member-details [m] (let
                              [static? (Modifier/isStatic (.getModifiers m))
                               method? (instance? Method m)
                               ctor? (instance? Constructor m)
                               text (if ctor? (str "<init>" (param-str m))
                                        (str (when static? "static ")
                                             (.getName m) " : "
                                             (if method?
                                               (str (.getSimpleName (.getReturnType m)) (param-str m))
                                               (str (.getSimpleName (.getType m))))))]
                            (assoc (bean m) :static? static? :method? method? :field? (instance? Field m) :ctor? ctor? :sort-val [(not static?) method? (sortable text)] :text text :member m)))

(defn show
  ([x] (show x (constantly true)))
  ([x selector] (let [c (if (class? x) x (class x))
                      members (sort-by :sort-val (map member-details (concat (.getFields c) (.getMethods c) (.getConstructors c))))]
                  (if (number? selector)
                    (:member (nth members selector))
                    (let [pred (if (ifn? selector)
                                 selector
                                 #(re-find (re-pattern (str "(?i)" selector)) (:name %)))]
                      (println "=== " (Modifier/toString (.getModifiers c)) c " ===")
                      (doseq [[i m] (indexed members)] (when (pred m) (printf "[%2d] %s\n" i (:text m)))))))))
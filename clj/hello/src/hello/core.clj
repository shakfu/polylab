(ns hello.core
  (:require hello.model)
  (:require [clojure.string :as s])
  (:gen-class))


(defn love
  "a basic love function"
  [x y]
  (+ x y))

(def love-map {
  :x 10
  :y 20
  :z 30
})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

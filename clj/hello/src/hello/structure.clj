(ns hello.structure
  (:require [clojure.string :as str]))

(defstruct field :name :type :elements)

(defn love
  "a basic love function"
  [x y]
  (+ x y))

(def love-map {
  :x 10
  :y 20
  :z 30
})

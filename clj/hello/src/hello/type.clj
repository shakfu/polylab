(ns hello.type
  (:require [clojure.string :as str]))

  ;; define a couple of shape types
  (deftype Circle [radius])
  (deftype Square [length width])

  ;; multimethod to calculate the area of a shape
  (defmulti area class)
  (defmethod area Circle [c]
      (* Math/PI (* (.radius c) (.radius c))))
  (defmethod area Square [s]
      (* (.length s) (.width s)))

  ;; create a couple shapes and get their area
  (def myCircle (Circle. 10))
  (def mySquare (Square. 5 11))

  (area myCircle)
  (area mySquare)

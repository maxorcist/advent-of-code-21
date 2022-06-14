(ns clj.day22
  (:require [clj.filereader :as filereader]))

(def input (filereader/input "day22"))
(def x (first input))

(disj #{[2 6 2] [2 2 2]} [2 6 2])

;;(re-seq #"on|off|(x|y|z).(-?\d+)\.+(-?\d+)" x)
(def patt (re-pattern #"on|off|(x|y|z).(-?\d+)\.+(-?\d+)"))
(re-seq patt x)
(defn range-of-s [[start end]] (range (Integer/parseInt start) (inc (Integer/parseInt end))))

(let [coll #{}]
  (reduce #(condp = (second %2)
             \x (assoc %1 0 ())
             \y
             \z
             ) [0 0 0] x))
;; neec to get range
(as-> ["ijdaiod" "x" "12" "22"] m
     (range-of-s (take-last 2 m))
     )

(take-last 2 x)
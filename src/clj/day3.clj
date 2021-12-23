(ns clj.day3
  (:require [clj.filereader :as read-file]))

(def testdata ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])
(def data (read-file/input "day3"))

(defn half-count [data] (/ (count data) 2))
(defn add-char [num char] (+ num (Character/digit char 2)))
(defn coll-copy [coll] (vec (repeat (count coll) 0)))
(defn bit-values [coll] (reverse (take (count coll) (iterate #(* 2 %) 1))))

(defn data-reducer [data] (reduce #(map (fn [a b] (add-char a b)) %1 %2) (coll-copy (first data)) data))
(defn map-gamma-epsilon [reducer data] (map #(if (< (half-count data) %) 1 0) (reducer data)))
(defn flip-bits [bits] (map #(if (= % 0) 1 0) bits))

(defn map-values [bits] (map * (bit-values bits) bits))
(defn eval-decimal [bits] (reduce + (map-values bits)))
(defn eval-str-decimal [str] (eval-decimal (reduce #(conj %1 (Character/digit %2 2)) [] str)))

(defn run [coll]
  (*
    (eval-decimal (map-gamma-epsilon data-reducer coll))
    (eval-decimal (flip-bits (map-gamma-epsilon data-reducer coll)))
))

(def oxygen "1")
(def co2 "0")
(defn count-prevalence-at-index [index coll]
  (apply - (reduce #(if (= \1 (nth %2 index))
              [(inc (first %1)) (second %1)]
              [(first %1) (inc (second %1))]
              ) [0 0] coll)))
(defn pos-neg-zero [number]
  (cond
    (< number 0) -1
    (> number 0) 1
    (= number 0) 0))
(defn compare-and-return
  ([zero one] (compare-and-return zero one "0"))
  ([zero one prefer]
   (if (= prefer "0")
     (if (> (count zero) (count one)) one zero)
     (if (> (count zero) (count one)) zero one)
     )))
(defn group-by-index-char [index coll prefer]
  (loop [zero [] one [] coll coll]
    (if (empty? coll)
      (compare-and-return zero one prefer)
      (let [char (nth (first coll) index)]
        (if (= char \0)
          (recur (conj zero (first coll)) one (rest coll))
          (recur zero (conj one (first coll)) (rest coll))
          )
        )
      )))

(defn run2 [coll prefer]
  (eval-str-decimal (loop [n 0 coll coll]
     (if (<= (count coll) 1)
       (first coll)
       (recur
         (inc n)
         (group-by-index-char n coll prefer))
       ))))

(* (run2 testdata oxygen) (run2 testdata co2))
(* (run2 data oxygen) (run2 data co2))

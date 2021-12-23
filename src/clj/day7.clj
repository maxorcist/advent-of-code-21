(ns clj.day7
  (:require [clj.filereader :as read-file]))

(def testdata '(16 1 2 0 4 2 7 1 2 14))
(def data
  (->> (read-file/input "day7")
       (apply #(clojure.string/split % #","))
       (map #(Integer/parseInt %))
       ))

(defn diff [n1 n2](- (max n1 n2) (min n1 n2)))
(defn calc-fuel-for-pos [i data]
  [i (reduce #(+ (diff i %2) %1) 0 data)]
  )
(defn diff-tri [n1 n2] (reduce + (range (inc (diff n1 n2)))))

(defn compare-results [[a1 a2] [b1 b2]]
  (cond
    (nil? a2) [b1 b2]
    (nil? b2) [a1 a2]
    (< a2 b2) [a1 a2]
    (>= a2 b2) [b1 b2]
    ))
(defn calc-coll-fuel [data]
  (let [start (apply min data)
        end (apply max data)]
    (loop [curr start best []]
      (if (>= curr end)
        best
        (recur
          (inc curr)
          (compare-results best (calc-fuel-for-pos curr data)))
        ))
    ))

;; redefine func
(defn calc-fuel-for-pos [i data]
  [i (reduce #(+ (diff-tri i %2) %1) 0 data)]
  )
(calc-coll-fuel testdata)
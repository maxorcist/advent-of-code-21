(ns clj.day5
  (:require [clj.filereader :as read-file]))

(def testdata [
    [[0 9]  [5 9]]
    [[8 0]  [0 8]]
    [[9 4]  [3 4]]
    [[2 2]  [2 1]]
    [[7 0]  [7 4]]
    [[6 4]  [2 0]]
    [[0 9]  [2 9]]
    [[3 4]  [1 4]]
    [[0 0]  [8 8]]
    [[5 5]  [8 2]]])
(defn split-into-vec [coll]
  [(vec (take 2 coll))(vec (drop 2 coll))])

(def data (->> (read-file/input "day5")
      (map #(clojure.string/split % #"( -> |,)"))
      (map (fn [s] (map #(Integer/parseInt %) s)))
      (map #(split-into-vec %))
      vec
      ))

(->> (read-file/input "day5")
;;     (map #(clojure.string/split % #"( -> |,)"))
     ;;(map (fn [s] (map #(Integer/parseInt %) s)))
     ;;(map #(split-into-vec %))
     ;;vec
     )
(defn straight-line? [start end]
  (or
    (= (first start) (first end))
    (= (second start) (second end))
    ))
(defn points-between [[start end]]
  (let [x1 (first start) x2 (first end)
        y1 (second start) y2 (second end)]
   (for [x (range (min x1 x2) (inc (max x1 x2)))
         y (range (min y1 y2) (inc (max y1 y2)))]
     [x y])))

(defn add-point [m k]
  (if (get m k)
    (update m k inc)
    (assoc m k 1)))
(defn run-data [data]
  (->> data
      (filter #(straight-line? (first %) (second %)))
      (map points-between)
      (reduce into [])
      (reduce add-point {})
       vals
       (filter #(< 1 %))
       count
      ))
(defn straight-or-diagonal [[start end]]
  (if (straight-line? start end)
    (points-between [start end])
    ((fn [[[x1 y1] [x2 y2]]]
       (let [r1 (range (min x1 x2) (inc (max x1 x2)))
             r2 (range (min y1 y2) (inc (max y1 y2)))]
         (map vector
              (if (< x1 x2) r1 (reverse r1))
              (if (< y1 y2) r2 (reverse r2))
             ))) [start end])
    ))
(straight-or-diagonal (second testdata))
(defn run-data-diagonal [data]
  (->> data
       (map #(straight-or-diagonal %))
       (reduce into [])
       (reduce add-point {})
       vals
       (filter #(< 1 %))
       count
       ))
()

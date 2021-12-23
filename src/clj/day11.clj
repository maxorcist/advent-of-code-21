(ns clj.day11
  (:require [clj.filereader :as read-file]))

(defn to-ints-and-vec [coll]
  (->> coll
       (map #(vec (map (fn [s] (Integer/parseInt (str s))) %)))
       vec
       ))
(def testdata
  (to-ints-and-vec ["5483143223"
                    "2745854711"
                    "5264556173"
                    "6141336146"
                    "6357385478"
                    "4167524645"
                    "2176841721"
                    "6882881134"
                    "4846848554"
                    "5283751526"]))
(def data (to-ints-and-vec ["4575355623"
                            "3325578426"
                            "7885165576"
                            "4871455658"
                            "3722545312"
                            "8362663832"
                            "5562743324"
                            "4165776412"
                            "1817813675"
                            "4255524632"]))

(defn inc-all [coll]
  (->> coll
       (map (fn [s] (vec (map #(inc %) s))))
       vec))
(defn neighbors [[rowi i]]
  (for [x (range (dec rowi) (+ 2 rowi))
        y (range (dec i) (+ 2 i)) :when (not= [x y] [rowi i])]
    [x y]))
(defn nil-or-0 [val]
  (or (nil? val) (= 0 val))
  )
(defn flash! [position [result coll]]
  (let [val (get-in coll position)]
    (if (nil-or-0 val)
      [result coll]
      (let [val (inc val) coll (update-in coll position inc)]
        ;;(println result coll)
        (if (< 9 val)
          (reduce #(flash! %2 %1) [(inc result) (assoc-in coll position 0)] (neighbors position))
          [result coll]))
      )))

(defn single-run [[flashes coll]]
  (loop [flashes flashes coll (inc-all coll) rowi 0 i 0]
    (if (nil? (get coll rowi))
      [flashes coll]
      (if (nil? (get-in coll [rowi i]))
        (recur flashes coll (inc rowi) 0)
        (let [[result coll] (if (< 9 (get-in coll [rowi i])) (flash! [rowi i] [0 coll]) [0 coll])]
          ;;(println result coll)
          (recur (+ result flashes) coll rowi (inc i))))
      )))
(defn run [steps coll]
  (take (inc steps) (iterate (fn [[flashes coll]] (single-run [flashes coll])) [0 coll])))
(defn every-0? [coll]
  (every? #(= 0 %) (flatten coll)))
(every-0? (second (last (run 195 testdata))))
(defn run-while-and-count-steps [coll]
  (loop [n 0 [flashes coll] [0 coll]]
    (if (every-0? coll)
      [n flashes coll]
      (recur (inc n) (single-run [flashes coll])))))
(run-while-and-count-steps data)
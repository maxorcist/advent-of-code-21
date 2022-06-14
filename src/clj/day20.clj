(ns clj.day20
  (:require [clj.filereader :as filereader])
  (:import (javax.management.openmbean OpenMBeanParameterInfoSupport)))

(defn to-bin [char] (if (= char \.) 0 1))
(defn to-bin-vec [str] (vec (map to-bin str)))
(defn input-to-bin [coll]
  [(to-bin-vec (first coll)) (vec (map to-bin-vec (drop 2 coll)))])

(def testdata (input-to-bin ["..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
                             ""
                             "#..#."
                             "#...."
                             "##..#"
                             "..#.."
                             "..###"]))
(def data (input-to-bin (filereader/input "day20")))

(defn get-bit [coords coll run-num] (or (get-in coll coords) (if (odd? run-num) 0 1)))
(defn get-bits [coords coll run-num] (reduce #(str %1 (get-bit %2 coll run-num)) "" coords))
(defn neighbors [[y x]] (for [a [(dec y) y (inc y)] b [(dec x) x (inc x)]] [a b]))
(defn parseBits [string] (Integer/parseInt string 2))
(defn get-val-from-pos [coords coll run-num] (parseBits (get-bits (neighbors coords) coll run-num)))
;;(get-val-from-pos [2 2] (second testdata)) -> 34
(defn lookup-val [lookup-table val] (get lookup-table val))
;;(lookup-val (first testdata) (get-val-from-pos [2 2] (second testdata))) -> 1

(defn paint-row [row] (reduce #(str %1 (if (= 0 %2) \. \#)) "" row))
(defn paint [coll] (vec (map paint-row coll)))
(defn printpaint [coll] (map #(format "%s \n" %) (paint coll)))

(defn add-padding [run-num coll]
  (let [row-len (count (first coll)) new-vec (vec (take row-len (repeat run-num)))]
    (as-> coll m
         (into [new-vec] m)
         (conj m new-vec)
          (map #(into [run-num] %) m)
          (map #(conj % run-num) m)
          (vec m))))
(defn add-paddings [n coll run-num]
  (last (take (inc n) (iterate #(add-padding (if (odd? run-num) 0 1) %) coll))))

(defn create-empty-output [coll]
  "Add empty rows and columns around"
  (vec (take (count coll) (repeat
           (vec (take (count (first coll)) (repeat 0)))))))

(defn one-pass [lookup-table input-img run-num]
  (let [input-img (add-paddings 3 input-img run-num)]
   (loop [[y x] [0 0] output-img (create-empty-output input-img)]
     (if (nil? (get input-img y))
       ;;(do (println (printpaint output-img)) output-img)
       output-img
       (if (nil? (get-in input-img [y x]))
         (recur [(inc y) 0] output-img)
         (recur [y (inc x)]
                (assoc-in
                  output-img
                  [y x]
                  (lookup-val lookup-table (get-val-from-pos [y x] input-img run-num)))))))))

;;(one-pass (first testdata) (second testdata))
(defn multi-pass [n lookup-table input-img]
  ;;(last (take (inc n) (iterate #(one-pass lookup-table % n) input-img)))
  (loop [run-num 1 input-img input-img]
    (if (< n run-num)
      input-img
      (recur (inc run-num) (one-pass lookup-table input-img run-num)))))
(reduce + (flatten (multi-pass 50 (first data) (second data))))

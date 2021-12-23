(ns clj.day13
  (:require [clj.filereader :as read-file]))

(def x ["6,10"
        "0,14"
        "9,10"
        "0,3"
        "10,4"
        "4,11"
        "6,0"
        "6,12"
        "4,1"
        "0,13"
        "10,12"
        "3,4"
        "3,0"
        "8,4"
        "1,10"
        "2,14"
        "8,10"
        "9,0"
        ""
        "fold along y=7"
        "fold along x=5"])
(defn str-to-coords [s]
  (as-> s m
        (clojure.string/split m #",")
        (map #(Integer/parseInt %) m)
        (assoc {} :x (first m) :y (second m))
        ))
(defn parse-fold [s]
  (as-> s m
        (clojure.string/split m #"\s")
        (last m)
        (clojure.string/split m #"=")
        (vector (keyword (first m)) (Integer/parseInt (second m)))
        ))

(defn format-data [coll]
  (as-> coll m
        ((fn [i] [(take i m) (drop (inc i) m)]) (.indexOf m ""))
        (#(list
            (set (map str-to-coords (first %)))
            (map parse-fold (second %))) m)
        ))

(def testdata (format-data x))
(def data (format-data (read-file/input "day13")))

(defn move-point [p k v]
  (assoc p k (- (k p) (* 2 (- (k p) v)))))
(defn fold [points [k v]]
  (let [s (group-by #(< v (k %)) points) beyond-fold (get s true) within-fold (get s false)]
    (set (reduce #(conj %1 (move-point %2 k v)) within-fold beyond-fold))
    ))

(count (fold (first testdata) (first (second testdata))))
(count (fold (first data) (first (second data))))

(defn run [coll]
  (loop [points (first coll) folds (last coll)]
   (if (empty? folds)
     points
     (recur (fold points (first folds)) (rest folds))
     )))
(def points (run data))
(defn get-vals [points] (map #(vec (vals %)) points))
(defn gen-matrix [n] (vec (take 6 (repeat (vec (take n (repeat 11)))))))
(:x (second points))
(loop [matrix (gen-matrix 38) points points]
        (if (empty? points)
          matrix
          (let [point (vector (:y (first points)) (:x (first points)))]
            (recur (assoc-in matrix point 55) (rest points)))
          ))
;; PZEHRAER
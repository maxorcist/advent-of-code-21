(ns clj.day18
  (:require [clojure.data.json :as json]
            [clj.filereader :as read-file]))

(def testdata1 (map json/read-str ["[1,1]"
                                   "[2,2]"
                                   "[3,3]"
                                   "[4,4]"]))

(def data (read-file/json-read "day18"))

(def testdata [[[[0, [5, 8]], [[1, 7], [9, 6]]], [[4, [1, 2]], [[1, 4], 2]]]
               [[[5, [2, 8]], 4], [5, [[9, 9], 0]]]
               [6, [[[6, 2], [5, 6]], [[7, 6], [4, 7]]]]
               [[[6, [0, 7]], [0, 9]], [4, [9, [9, 0]]]]
               [[[7, [6, 4]], [3, [1, 3]]], [[[5, 5], 1], 9]]
               [[6, [[7, 3], [3, 2]]], [[[3, 8], [5, 7]], 4]]
               [[[[5, 4], [7, 7]], 8], [[8, 3], 8]]
               [[9, 3], [[9, 9], [6, [4, 9]]]]
               [[2, [[7, 7], 7]], [[5, 8], [[9, 3], [0, 2]]]]
               [[[[5, 2], 5], [8, [3, 7]]], [[5, [7, 5]], [4, 4]]]])
;;(map #(inc %) (tree-seq vector? seq (first data)))
;;(tree-seq vector? identity (first data))

(defn add-or-same [coll ks1 ks2] (let [k1 (get-in coll ks1) k2 (get-in coll ks2)]
                                   ;;(println "---- k1 " k1 " k2 " k2)
                                   (if k2 (+ k1 k2) k1)))
(defn explode [coll m]
  ;;(println "Explode " (get-in coll (:explode m)))
  (as-> coll coll
        (if (:left m) (assoc-in coll (:left m) (add-or-same coll (:left m) (conj (:explode m) 0))) coll)
        (if (:right m) (assoc-in coll (:right m) (add-or-same coll (:right m) (conj (:explode m) 1))) coll)
        (assoc-in coll (:explode m) 0)))
(defn split [num]
  ;;(println "Split " num)
  [(Math/floorDiv num 2) (Math/round (Math/ceil (/ num 2)))])

(defn traverse ([coll] (traverse coll [] {}))
  ([coll depth result]
   ;;(println coll result)
   ;;(if (or (:split result) (every? result [:explode :left :right]))
   (if (every? result [:explode :left :right])
     result
     (if (not (vector? coll))
       (cond
         (and (< 9 coll) (not (:split result)) (not (:explode result))) (assoc result :split depth)
         (and (not (contains? result :explode))) (assoc result :left depth)
         (and (not (contains? result :right)) (contains? result :explode)) (assoc result :right depth)
         :else result)
       (if (and (< 3 (count depth)) (not (contains? result :explode)))
         (assoc result :explode depth)
         (let [left (first coll) right (second coll)]
           (->> (traverse left (conj depth 0) result)
                (traverse right (conj depth 1)))
           ))))))
(defn cycle-traverse [coll]
  ;;(println coll)
  (as-> (traverse coll) m
        (cond
          (:explode m) (recur (explode coll m))
          (:split m) (recur (update-in coll (:split m) split))
          :else coll
          ;;:else (do (println coll) coll)
          )))

(defn traverse-over-coll [coll]
  (reduce #(cycle-traverse (vector %1 %2)) coll))


(defn magnitude ([n] (if (vector? n) (magnitude (first n) (second n)) n))
  ([x y]
   ;;(println x y)
   (if (or (vector? x) (vector? y))
     [x y]
     (+ (* x 3) (* y 2))
     )))
(def res [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]])

(defn reduce-magnitude ([coll] (reduce-magnitude coll [] coll))
  ([coll depth result]
   ;;(println depth result)
   (if (number? coll)
     result
     (let [left (first coll) right (second coll)]
       (if (and (number? left) (number? right))
         (if (empty? depth) (magnitude left right)
                            (assoc-in result depth (magnitude left right)))
         (->> (reduce-magnitude left (conj depth 0) result)
              (reduce-magnitude right (conj depth 1))))
       ))))
(defn loop-magnitude [coll]
  (loop [coll coll]
    (if (number? coll)
      coll
      (recur (reduce-magnitude coll)))))

;;(traverse-over-coll (vector (first testdata) (second testdata)))
;;(loop-magnitude (traverse-over-coll data))
(let [coll data]
  (apply max (for [x (range (count coll)) y (range (count coll))
         :when (not= x y) :let [take2 (vector (nth coll x) (nth coll y))]]
     (loop-magnitude (cycle-traverse take2)))))
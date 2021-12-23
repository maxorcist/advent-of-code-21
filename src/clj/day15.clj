(ns clj.day15
  (:require [clj.filereader :as read-file]))

(defn to-ints-and-vec [coll]
  (->> coll
       (map #(vec (map (fn [s] (Integer/parseInt (str s))) %)))
       vec
       ))

(def testdata
  (to-ints-and-vec ["1163751742"
                    "1381373672"
                    "2136511328"
                    "3694931569"
                    "7463417111"
                    "1319128137"
                    "1359912421"
                    "3125421639"
                    "1293138521"
                    "2311944581"]))
(def data (to-ints-and-vec (read-file/input "day15")))

(defn neighbors [[rowi i]]
  [[rowi (inc i)] [(inc rowi) i] [rowi (dec i)] [(dec rowi) i]])
(defn finish [coll] [(dec (count coll)) (dec (count (last coll)))])

;; Diagonal forbidden
(loop [coll testdata rowi 0 i 0 result 0]
  (if (nil? (get-in coll [rowi i]))
    nil
    (if (= [rowi i] (finish coll))
      [result coll]
      (recur
        (assoc-in coll [rowi i] nil)
        (inc rowi)
        (inc i)
        (+ result (get-in coll [(inc rowi) (inc i)]))))))

(defn sort-cheap-neighbors [coll pos neighbors]
  (sort-by #(get-in coll %) neighbors))
(defn filter-neighbors [coll neighbors]
  (filter #(not (nil? (get-in coll %))) neighbors))
(defn one-way-neighbors [[rowi i]]
  [[(inc rowi) i] [rowi (inc i)]])
(defn weight-neighbor [[y1 x1] [y2 x2] cost]
  (+ cost (apply + (map - [y1 x1] [y2 x2]))))
(defn get-weighted-neighbors [pos coll]
  (sort-by #(weight-neighbor pos % (get-in coll %)) (filter-neighbors coll (neighbors pos))))

(defn quick-route ([coll] (quick-route coll [0 0] [0 0]))
  ([coll best] (quick-route coll [0 0] [0 best]))
  ([coll pos [result best]]
   (println (get-in coll pos) pos result best)
   ;;(println (get-in coll [(dec (first pos)) (dec(second pos))]))
   (if (and (> result best) (not= 0 best))
     [(- result (get-in coll pos)) best]
     (if (= pos (finish coll))
       [(- result (get-in coll pos)) (if (= 0 best) result (min result best))]
       (as-> (take 2 (get-weighted-neighbors pos coll)) m
             (reduce #(quick-route
                        (assoc-in coll pos nil)
                        %2
                        [(+ result (get-in coll %2)) (second %1)]
                        )
                     [result best] m)

             )
     ))))

;;(quick-route data 469)

(defn route-to-finish ([coll finish] (route-to-finish coll finish [0 0] [0 0]))
  ([coll finish pos [result best]]
   (println pos result best)
   (if (and (> result best) (not= 0 best))
     [(- result (get-in coll pos)) best]
     (if (= pos finish)
       [(- result (get-in coll pos)) (if (= 0 best) result (min result best))]
       (as-> (get-weighted-neighbors pos coll) m
             (reduce #(route-to-finish
                        (assoc-in coll pos nil)
                        finish
                        %2
                        [(+ result (get-in coll %2)) (second %1)]
                        )
                     [result best] m)

             )
       ))))

(defn heuristic [pos]
  (apply + pos))
(defn pop-queue [queue] (reduce #(if (< (get queue %1) (get queue %2)) %1 %2) (keys queue)))
(defn pop-queue-heuristic [queue]
  (reduce #(if (< (- (get queue %1) (heuristic %1)) (- (get queue %2) (heuristic %2))) %1 %2) (keys queue)))
(defn get-unvisited-neighbors [pos coll visited]
  (filter #(not (contains? visited %)) (filter-neighbors coll (neighbors pos))))
(defn add-neighbor-to-queue [pos coll currcost queue]
  (let [prev (get queue pos) cost (+ currcost (get-in coll pos)) newcost (if (nil? prev) cost (min prev cost))]
    (assoc queue pos newcost)))

(defn dijkstra [coll]
  (let [finish [(dec (count coll)) (dec (count (first coll)))]]
   (loop [current [0 0] visited #{} queue {[0 0] 0} cost 0]
     ;;(println current)
     (if (empty? queue)
       "End node not connected to start"
       (if (= current finish)
         cost
         (let [queue (reduce #(add-neighbor-to-queue %2 coll cost %1) queue (get-unvisited-neighbors current coll visited))
               next (pop-queue-heuristic queue)]
           (recur next (conj visited current) (dissoc queue current) (get queue next)))
         )
       ))))

;;(dijkstra data)
(defn inc-or-1 [num] (if (= num 9) 1 (inc num)))
(defn inc-or-1-all [coll] (vec (map (fn [row] (vec (map inc-or-1 row))) coll)))

(defn multiply-coll [coll] (reduce into [] (take 5 (iterate #(inc-or-1-all %) coll))))
(defn multiply-row [row] (reduce into [] (take 5 (iterate #(map inc-or-1 %) row))))

(def multi-testdata (multiply-coll (map multiply-row testdata)))
;;(time (dijkstra multi-testdata)) ;; time 119, 60 without println
;; 140 with heuristic

(def multi-data (multiply-coll (map multiply-row data)))
;;(time (dijkstra multi-data)) ;; time 25020 without println
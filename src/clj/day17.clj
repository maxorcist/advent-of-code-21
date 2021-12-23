(ns clj.day17)

(def testdata [(range 20 31) (range -10 -4)])
(def data [(range 185 222) (range -122 -73)])

(defn decay-vel [[x y]] [(if (pos? x) (dec x) (if (= 0 x) 0 (inc x)))
              (dec y)])
(defn change-pos [vel pos] (vec (map + vel pos)))
(defn is-beyond? [[x y] target-range]
  (or (> x (apply max (first target-range)))
      (< y (apply min (second target-range)))))
(defn on-target? [[x y] target-range]
  (and (contains? (set (first target-range)) x)
       (contains? (set (second target-range)) y)))

(defn launch [[x y] target]
  (loop [vel [x y] position [0 0] height 0]
   ;;(println vel position)
   (if (is-beyond? position target)
     nil ;;"Is beyond target "
     (if (on-target? position target)
       [[x y] position];;"On target, pal"
       (recur (decay-vel vel)
              (change-pos vel position)
              (if (< height (second position)) (second position) height))))))

(launch [30 -10] testdata)
;;(launch [20 121] data)
;;(sort-by #(first %) (for [y (range 121 200)
;;       :let [x 20 y (launch [x y] data)]
;;       :when (not (nil? y))]
;;   y)) ;; [7381 [20 121] [210 -122]]
(count (for [x (range 222) y (range -122 122)
       :let [y (launch [x y] data)]
       :when (not (nil? y))]
   y)) ;; 2897

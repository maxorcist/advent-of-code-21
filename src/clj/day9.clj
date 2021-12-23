(ns clj.day9
  (:require [clj.filereader :as read-file]))

(defn map-to-int-seqs [coll-of-strings]
  (->> coll-of-strings
       (map (fn [ss] (vec (map #(Integer/parseInt (str %)) ss))))
       vec
       ))
(def testdata (map-to-int-seqs ["2199943210"
                                "3987894921"
                                "9856789892"
                                "8767896789"
                                "9899965678"]))
(def data (map-to-int-seqs (read-file/input "day9")))

(defn nil-or-nine [func] (if-let [x func] x 9))
(defn get-from-top [coll rowi i]
  (nil-or-nine (get (get coll (dec rowi)) i)))
(defn get-from-bot [coll rowi i]
  (nil-or-nine (get (get coll (inc rowi)) i)))
(defn get-left [row i] (nil-or-nine (get row (dec i))))
(defn get-right [row i] (nil-or-nine (get row (inc i))))

(defn is-lowest? [rowi i coll]
  (let [row (get coll rowi) val (get row i)]
    (and (not (nil? val))
         (< val (get-left row i))
         (< val (get-right row i))
         (< val (get-from-top coll rowi i))
         (< val (get-from-bot coll rowi i))
         ))
  )

(defn find-lowpoints [coll]
      (loop [rowi 0 i 0 result 0]
        (let [row (get coll rowi) val (get row i)]
          (if val
            (recur
              rowi
              (inc i)
              (if (is-lowest? rowi i coll) (+ 1 val result) result))
            (if (nil? row)
              result
              (recur
                (inc rowi)
                0
                result
                ))))))

;;(find-lowpoints data)

(defn nine-or-nil [[rowi i] coll] (#(or (nil? %) (= 9 %)) (get-in coll [rowi i])))
(defn check-self-and-neighbors [[rowi i] [result coll]]
  (if (nine-or-nil [rowi i] coll)
    [result coll]
    (let [result (inc result) coll (assoc-in coll [rowi i] 9)]
      ;;(println result coll)
      (->> [result coll]
           (check-self-and-neighbors [(dec rowi) i]) ;; up
           (check-self-and-neighbors [rowi (dec i)]) ;; left
           (check-self-and-neighbors [(inc rowi) i]) ;; down
           (check-self-and-neighbors [rowi (inc i)]) ;; right
           ))))

(check-self-and-neighbors [0 8] [0 testdata])
(defn run [coll]
  (loop [rowi 0 i 0 results [] coll coll]
   (if (nil? (get coll rowi))
     results
     (if (not (nine-or-nil [rowi i] coll))
       (let [[result coll] (check-self-and-neighbors [rowi i] [0 coll])]
         (recur rowi (inc i) (conj results result) coll))
       (if (= 9 (get-in coll [rowi i]))
         (recur rowi (inc i) results coll)
         (recur (inc rowi) 0 results coll)
         )
       )
     )))
(->> data
    run
     sort
     reverse
     (take 3)
     (apply *)
    )
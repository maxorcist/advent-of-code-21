(ns clj.day19
  (:require [clj.filereader :as filereader]))

(defn split-n-int [coll]
  (->> (partition-by #(empty? %) coll)
       (filter #(not= 1 (count %)))
       (map #(drop 1 %))
       (map (partial map #(clojure.string/split % #",")))
       (map (partial map (partial map #(Integer/parseInt %))))
       (map #(vec %))))

(def testdata1 (split-n-int
                 ["--- scanner 0 ---"
                  "0,2"
                  "4,1"
                  "3,3"
                  ""
                  "--- scanner 1 ---"
                  "-1,-1"
                  "-5,0"
                  "-2,1"]))
(def testdata2 (split-n-int (filereader/input "day19example")))
(def data (split-n-int (filereader/input "day19")))

(defn distance [p1 p2]
  (Math/sqrt (apply + (map #(* % %) (map - p2 p1)))))
(defn invert [num] (* -1 num))
(defn invert-p [coords] (map invert coords))
(defn distances-from [coll index]
  (set (map #(distance (nth coll index) %) coll)))
(defn diff [p1 p2]
  ;;(println "diff: " p1 p2 (map - p1 p2))
  (map - p1 p2))
(defn zero-nodes [diff coll] (map #(map + diff %) coll))
(def position-combintations (for [x (range 3) y (range 3) z (range 3)
                                  :when (and (not= x y) (not= x z) (not= y z))] [x y z]))
(defn map-pos-combinations [coords] (map #(vector (nth coords (first %)) (nth coords (second %)) (nth coords (last %))) position-combintations))

(defn transpose-all
  [colls]
  (lazy-seq
    (let [ss (keep seq colls)]
      (when (seq ss)
        (cons (map first ss) (transpose-all (map rest ss)))))))
(defn mapmap [f collwithcolls] (map (partial map f) collwithcolls))

(defn flip [p] (conj (vec (reverse (take 2 p))) (last p)))
(defn fliplast [p] (conj (reverse (drop 1 p)) (first p)))
(defn flipreverse [p] (-> p reverse flip))
(defn reverseflip [p] (-> p flip reverse))

(defn flip-n-find-rotation [coll0 diff0 coll1 diff1]
  (let [zerod0 (set (zero-nodes diff0 coll0)) zerod1 (set (zero-nodes diff1 coll1))]
    (println zerod0 zerod1)
    (first (for [x [1 -1] y [1 -1] z [1 -1] f [identity reverse flip fliplast flipreverse reverseflip]
                 :let [m (map #(f (map * [x y z] %)) zerod1)
                       ;;kd (println (type f) [x y z] m)
                       ]
                 :when (< 11 (count
                               (clojure.set/intersection
                                 (set zerod0)
                                 (set m)
                                 )))
                 ]
             (do (println "result: " [x y z] f m) (vector [x y z] f m))))
    ))

(defn scanner-location [p1 [p2 r]] "coords of shared beacon by both scanners"
  (println "Scanner location of: " p1 p2 r)
  [(map + p1 (invert-p p2)) (map #(map + p1 %) r)])
(defn rotate [p [flip rotefn r]]
  (println "rotate: " p flip rotefn "and" r)
  [(rotefn (map * flip p)) r])
(defn transform [p diff]
  (println "transform: " p diff)
  (map + p diff))

(defn run-2-scanners [coll0 coll1]
  (let [s0 coll0 s1 coll1]
    (loop [n 0 m 0 ds1 (distances-from s1 n)]
      ;;(println n ds1)
      ;;(println "m: " m " n: " n)
      (if (nil? (get s0 m))
        ['() '() "No match"]
        (let [ds0 (distances-from s0 m) matches (count (clojure.set/intersection ds0 ds1))]
          (println matches)
          (if (> matches 11)
            (let [p0 (get s0 m) p1 (get s1 n)]
              (scanner-location p0 (rotate p1 (flip-n-find-rotation s0 (invert-p p0) s1 (invert-p p1)))))
            (if-let [n (and (get s1 (inc n)) (inc n))]
              (recur n m (distances-from s1 n))
              (recur 0 (inc m) (distances-from s1 0)))))))))


(defn run-all [colls]
  (loop [universe (set (first colls)) scanners #{'(0 0 0)} rest-colls (rest colls)]
    (if (empty? rest-colls)
      [rest-colls (count scanners) scanners (count universe) universe]
      (let [new-result (run-2-scanners (vec universe) (first rest-colls))
            new-scanner (first new-result) new-beacons (second new-result)]
        (if (and (empty? new-beacons) (not= 1 (count rest-colls)))
          (recur universe scanners (conj (vec (rest rest-colls)) (first rest-colls)))
          (recur (clojure.set/union (set universe) (set new-beacons))
                 (conj scanners new-scanner) (rest rest-colls)))
        ))))
(def result-of-testdata2 (run-all testdata2))
(def result-of-data (run-all data))

(get result-of-testdata2 2)
(get result-of-data 2)

(defn manhattan-distance [p1 p2] (apply + (map #(Math/abs %) (map - p1 p2))))

(let [points (vec (get result-of-data 2))]
  (println points (get points 0))
  (loop [n 0 best 0]
    (if (nil? (get points n))
      best
     (let [result (apply max (map #(manhattan-distance (get points n) %) points))]
       (recur (inc n) (if (< best result) result best
                        ))))))


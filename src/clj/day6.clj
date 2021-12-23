(ns clj.day6
  (:require [clj.filereader :as read-file]))

(def testdata [3 4 3 1 2])
(def data
  (-> (read-file/input "day6")
      first
      (clojure.string/split #",")
      (#(map (fn [s] (Integer/parseInt s)) %))
      vec
      ))
(defn map-from-data [data]
  (loop [k 0 fishmap {}]
    (if (> k 8)
      fishmap
      (recur
        (inc k)
        (assoc fishmap (keyword (str k)) (count (filter #(= k %) data)))))))
(def testdatamap (map-from-data testdata))
(def datamap (map-from-data data))

(defn simulate-fish [data days]
  (loop [day 0 fish data newfish []]
    (let [fish (concat fish newfish)]
      (if (>= day days)
        (count fish)
        (recur
          (inc day)
          (map #(if (= % 0) 6 (dec %)) fish)
          (reduce #(if (= 0 %2) (conj %1 8) %1) [] fish)
          )))))

(defn nextday [map]
  (->> (reduce
         #(let [k (keyword (str %2))]
            (assoc %1 (keyword (str (dec %2))) (k %1)))
         map (range 0 9))
       (#(assoc % :6 (+ (:6 %) (:-1 %))))
       (#(assoc % :8 (:-1 %)))
       ))
(defn cycle-days [map days]
  (loop [day 0 map map]
    (if (>= day days)
      ((comp #(apply + %) vals) (select-keys map [:0 :1 :2 :3 :4 :5 :6 :7 :8]))
      (recur
        (inc day)
        (nextday map)
        )
      ))
  )
(time (simulate-fish data 80))
(time (cycle-days datamap 256))


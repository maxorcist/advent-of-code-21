(ns clj.day1
  (:require [clj.filereader :as read-file]))

(defn make-int [numbers] (map #(Integer/parseInt %) numbers))

(def testdata (make-int ["199" "200" "208" "210" "200" "207" "240" "269" "260" "263"]))
(def data (make-int (read-file/input "day1")))

(defn run [result val d]
  (if (empty? d)
    result
    (run (if (< val (first d)) (inc result) result) (first d) (rest d))
    ))
;;(run 0 (first testdata) (rest testdata))
;;(run 0 (first data) (rest data))

(defn threesum [data] (+ (first data) (second data) (nth data 2)))

(defn run2 [result val data]
  (if (< (count data) 3)
    result
    (run2
      (if (< val (apply + (take 3 data))) (inc result) result)
      (apply + (take 3 data))
      (rest data)
      )
    ))

;;(run2 0 (apply + (take 3 testdata)) (rest testdata))
(run2 0 (apply + (take 3 data)) (rest data))

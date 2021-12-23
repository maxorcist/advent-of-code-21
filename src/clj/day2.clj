(ns clj.day2
  (:require [clj.filereader :as read-file]))

;;(defn forward [x] #(+ x %))
;;(defn down [y] #(+ y %))
;;(defn up [y] #(+ y %))
(def testdata [
               "forward 5"
               "down 5"
               "forward 8"
               "up 3"
               "down 8"
               "forward 2"
               ])
(def data (read-file/input "day2"))
(defn split-n-int [v]
  (#(vector (first %) (Integer/parseInt (second %))) (clojure.string/split v #" ")))

(defn run
  ([arg-list] (run 0 0 arg-list))
  ([x y arg-list]
   (if (empty? arg-list)
     (* x y)
     (let [arg (split-n-int (first arg-list))]
       (recur
           (if (= "forward" (first arg)) (+ x (second arg)) x)
           (condp = (first arg)
             "down" (+ y (second arg))
             "up" (- y (second arg))
             "forward" y)
           (rest arg-list)))
     )
   )
  )
(defn run2
  ([arg-list] (run2 0 0 0 arg-list))
  ([aim x y arg-list]
   (println aim x y (first arg-list))
   (if (empty? arg-list)
     (* x y)
     (let [arg (split-n-int (first arg-list))]
       (recur
         (condp = (first arg)
           "down" (+ aim (second arg))
           "up" (- aim (second arg))
           "forward" aim)
         (if (= "forward" (first arg)) (+ x (second arg)) x)
         (if (= "forward" (first arg)) (+ y (* aim (second arg))) y)
         (rest arg-list)))
     )
   )
  )

(run2 data)
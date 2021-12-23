(ns clj.day4
  (:require [clj.filereader :as read-file]))
(def x [[41 55 44 2 39]
  [38 14 19 72 64]
  [75 95 35 6 47]
  [70 7 1 29 86]
  [83 79 90 96 82]])
(def testnumbers '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))
(def testboards [[[22 13 17 11 0]
                  [8 2 23 4 24]
                  [21 9 14 16 7]
                  [6 10 3 18 5]
                  [1 12 20 15 19]]

                 [[3 15 0 2 22]
                  [9 18 13 17 5]
                  [19 8 7 25 23]
                  [20 11 10 24 4]
                  [14 21 16 12 6]]

                 [[14 21 17 24 4]
                  [10 16 15 9 19]
                  [18 8 23 26 20]
                  [22 11 13 6 5]
                  [2 0 12 3 7]]])
(def data (read-file/input "day4"))
(def numbers (map #(Integer/parseInt %) (clojure.string/split (first data) #",")))
(def boards(->> (read-file/input "day4")
      rest
      (map #(clojure.string/split % #"\s"))
      (map #(filter not-empty %))
      (filter not-empty)
      (map #(vec (map (fn [str] (Integer/parseInt str)) %)))
      (#(loop [result [] coll %]
          (if (empty? coll)
            result
            (recur (conj result (vec (take 5 coll))) (drop 5 coll)))))
      ))

(defn index-of [val coll] (first (keep-indexed #(when (= val %2) %1) coll)))
(defn find-number-in-board [val board]
  (loop [rowindex 0 numindex nil coll board]
    (if (nil? numindex)
      (if (empty? coll)
        nil
        (recur (inc rowindex) (index-of val (first coll)) (rest coll)))
      [(dec rowindex) numindex])))
(defn mark-value [[rowi vali] board] (update-in board [rowi vali] (fn [_] nil)))
(defn check-row-bingo [row] (every? nil? row))
(defn check-col-bingo [i board] (every? nil? (map #(get % i) board)))
(defn check-bingo [[row i] board]
  (some true? [(check-row-bingo (nth board row)) (check-col-bingo i board)]))
(defn sum-rows [board]
  (->> board
       (flatten)
       (filter number?)
       (reduce +)))

(defn eval-single-board [board numbers]
  (loop [bingo nil pulled-nums [] remaining-nums numbers board board]
    (if (or (empty? remaining-nums) (not (nil? bingo)))
      [(when bingo (* (peek pulled-nums) (sum-rows board))) pulled-nums]
      (let [drawn-num-at (find-number-in-board (first remaining-nums) board)]
        (let [board (if (nil? drawn-num-at) board (mark-value drawn-num-at board))]
          (recur
            (if (nil? drawn-num-at) nil (check-bingo drawn-num-at board))
            (conj pulled-nums (first remaining-nums))
            (rest remaining-nums)
            board))
        ))
    ))

(defn eval-board-list [boards numbers]
  (loop [best nil numbers numbers boards boards]
    (if (empty? boards)
      best
      (let [result (eval-single-board (first boards) numbers)]
         (recur
           (if (nil? best)
             result
             (if (or (nil? (first result)) (< (count (second best)) (count (second result)))) best result))
           (second result)
           (rest boards)
           )))
    ))
(defn eval-loser-board [boards numbers]
  (loop [worst nil boards boards]
    (if (empty? boards)
      [(first worst) (count (second worst))]
      (let [result (eval-single-board (first boards) numbers)]
        (println (count (second result)) result)
        (recur
          (if (nil? worst)
            result
            (if (or (nil? (first result)) (> (count (second worst)) (count (second result)))) worst result))
          (rest boards)
          )))
    ))
(eval-loser-board testboards testnumbers)
(ns clj.day21)

(def determ-die (cycle (range 1 101)))
;;(take 3(repeatedly #(take 3 (determ-die))))
(defn move [from steps]
  (let [to (mod (+ from steps) 10)]
    (if (= to 0) 10 to)))

;;(loop [n 0 m determ-die]
;;  (println n)
;;  (if (> n 1000)
;;    n
;;    (recur (+ n (apply + (take 3 m))) (drop 3 m))))

(defn practice-run [pos1 pos2]
  (loop [current-player 1 pos1 pos1 pos2 pos2 score1 0 score2 0 die determ-die rolls 0]
   (println score1 score2 (take 3 die) [pos1 pos2])
   (if (or (>= score1 1000) (>= score2 1000))
     {:score1 score1 :score2 score2 :rolls (* 3 rolls)}
     (let [throw (apply + (take 3 die))]
       (if (= current-player 1)
         (recur 2 (move pos1 throw) pos2 (+ score1 (move pos1 throw)) score2 (drop 3 die) (inc rolls))
         (recur 1 pos1 (move pos2 throw) score1 (+ score2 (move pos2 throw)) (drop 3 die) (inc rolls)))))))

;;(def testrun (practice-run 4 8))
;;(* (min (:score1 testrun) (:score2 testrun)) (:rolls testrun))
;;(def run1 (practice-run 9 6))
;;(* (min (:score1 run1) (:score2 run1)) (:rolls run1))

;; [player pos1 pos2 score1 score2]
(defn outcomes [pos] (map #(move pos %) (for [d1 (range 1 4) d2 (range 1 4) d3 (range 1 4)] (+ d1 d2 d3))))
(def universes {[1 1 0 1 0] 1})
(defn get-n-set [u p v] (if-let [r (get u p)] (assoc u p (+ r v)) (conj u [p v])))
(defn set-outcomes [u p player v]
  (let [outcomes (outcomes (if (= player 1) (get p 1) (get p 3)))]
    (map #(get-n-set ) outcomes)))
(get-n-set universes [0 1 0 1 0] 15)
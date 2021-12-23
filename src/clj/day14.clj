(ns clj.day14
  (:require [clj.filereader :as read-file]))

(defn parse-instruction [m str]
  (as-> str s
        (clojure.string/split s #" -> ")
        (assoc m (keyword (first s)) (second s))))
(defn get-key [char-list] (keyword (apply str char-list)))
(defn keyify-string [s]
  (loop [result [] s s]
    (if (>= 1 (count s))
      result
      (recur (conj result (get-key (take 2 s))) (rest s)))))
(defn inc-or-one [m k]
  (assoc m k (if (k m) (inc (k m)) 1)))
(defn score-charlist [m char-list]
  (reduce #(inc-or-one %1 (keyword (str %2))) m char-list))
(defn cleanup [data]
  (as-> data m
        (vector (str (first (first m))) (keyify-string (first m)) (drop 2 m))
        (vector (first m) (second m) (reduce parse-instruction {} (last m)))))
(defn score-beginning-pairs [ks]
  (apply assoc {} (interleave ks (repeat 1))))

(def testdata (cleanup ["NNCB"
                        ""
                        "CH -> B"
                        "HH -> N"
                        "CB -> H"
                        "NH -> C"
                        "HB -> C"
                        "HC -> B"
                        "HN -> C"
                        "NN -> C"
                        "BH -> H"
                        "NC -> B"
                        "NB -> B"
                        "BN -> B"
                        "BB -> N"
                        "BC -> B"
                        "CC -> N"
                        "CN -> C"]))
(def data (cleanup (read-file/input "day14")))

(defn single-pass [data]
  (let [s (reduce conj [] (first data)) instructions (second data)]
    (loop [to [] from s]
      (if (> 2 (count from))
        (vector (reduce conj to from) instructions)
        (let [k (get-key (take 2 from)) v (first (k instructions))]
          (recur (conj to (first from) v) (rest from)))))))

(defn run-passes [n data]
  (loop [n n data data]
    (if (> 1 n)
      (first data)
      (recur (dec n) (single-pass data)))))

(defn run-and-count [n data]
  (reduce #(inc-or-one %1 (keyword (str %2))) {} (run-passes n data)))

;;(as-> (run-and-count 10 testdata) m
;;      (vals m)
;;      (- (apply max m) (apply min m))
;;      ) ;; blows memory

(defn get-vals [ks instr]
  (map #(% instr) ks))
(defn split-n-match [k m]
  (let [v (k m) kname (name k)]
    [(keyword (str (first kname) v)) (keyword (str v (second kname)))]
    ))
(defn split-n-match-list [ks instructions]
  (flatten (map #(split-n-match % instructions) ks)))

(defn run2 [times result pairs instr]
  (loop [n times result result pairs pairs]
    (if (>= 0 n)
      result
      (recur
        (dec n)
        (score-charlist result (get-vals pairs instr))
        (split-n-match-list pairs instr)
        ))))

(defn get-val-or-0 [m k] (if-let [v (k m)] v
                                           0
                                           ;;(bigint 0)
                                           ))
;;(defn split-3-way [m k]
;;  (let [v (k m) kname (name k)]
;;    [v (keyword (str (first kname) v)) (keyword (str v (second kname)))]))
(defn merge-maps [m new]
  (reduce (fn [m kv] (let [k (first kv) v (second kv)] (assoc m k (+ (get-val-or-0 k m) v)))) m new))
(defn split-n-score [k instr result newmap]
  (let [times (get-val-or-0 result k) [a b] (split-n-match k instr)]
    ;;(if (< 0 times) (println k times a b))
    (assoc newmap k (- (+ times (get-val-or-0 newmap k)) times)
                  a (+ (get-val-or-0 newmap a) times)
                  b (+ (get-val-or-0 newmap b) times))
    ))
(defn split-n-score-sidesave [result instr]
  (let [ks (keys result) newmap {}]
    (reduce #(split-n-score %2 instr result %1) newmap ks)))

(defn get-letter-score [kv]
  (vector (keyword (str (second (name (first kv))))) (second kv)))
(defn filter-pairs-into-letter-score [pairs]
  (as-> pairs m
        (map get-letter-score m)
        (reduce
          #(assoc %1 (first %2) (+ (get-val-or-0 %1 (first %2)) (second %2)))
          {} m)
        ))

(defn run3 [times n1 pairs instr]
  (loop [times times result (reduce inc-or-one {} pairs)]
    (if (> 1 times)
      (update (filter-pairs-into-letter-score result) (keyword n1) inc)
      (recur
        (dec times)
        (split-n-score-sidesave result instr)
        ))))

(as-> (run3 40 (first testdata) (second data) (last data)) m
      (vals m)
      (- (apply max m) (apply min m))
      )

(ns clj.day10
  (:require [clj.filereader :as read-file]))

(def x "[<>({}){}[([])<>]]")
(def y "{([(<{}[<>[]}>{[]{[(<()>")
(def z "<{([{{}}[<[[[<>{}]]]>[]]")
(def testdata (clojure.string/split-lines "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"))
(def data (read-file/input "day10"))

(defn match-pairs [a b]
  (cond
    (= a \() (= b \))
    (= a \[) (= b \])
    (= a \{) (= b \})
    (= a \<) (= b \>)
    :else false))
(defn is-open? [b]
  (not (nil? (some #{\( \[ \{ \<} (str b)))))
(defn score-error [b]
  (condp = b
   \) 3
   \] 57
   \} 1197
   \> 25137
    ))
(defn run-string [s]
  (loop [stack '() strings s]
   (if (empty? strings)
     0
     (if (match-pairs (first stack) (first strings))
       (recur (rest stack) (rest strings))
       (if (is-open? (first strings))
         (recur (conj stack (first strings)) (rest strings))
         (score-error (first strings))
         )))))
(reduce #(+ %1 (run-string %2)) 0 data)

(defn get-match [a]
  (cond
    (= a \() \)
    (= a \[) \]
    (= a \{) \}
    (= a \<) \>
    :else false))
(defn get-match-scores [a]
  (cond
    (= a \() 1
    (= a \[) 2
    (= a \{) 3
    (= a \<) 4
    :else 0))
(defn mult-by-5-and-add [score add]
  (+ add (* 5 score))
  )

(defn get-open-chunks [s]
  (loop [stack '() strings s]
    (if (empty? strings)
      (reduce #(mult-by-5-and-add %1 (get-match-scores %2)) 0 stack)
      (if (match-pairs (first stack) (first strings))
        (recur (rest stack) (rest strings))
        (if (is-open? (first strings))
          (recur (conj stack (first strings)) (rest strings))
          0
          )))))
(map #(get-open-chunks %) testdata)
(->> data
     (map #(get-open-chunks %))
     (filter pos?)
     sort
     (#(nth % (quot (count %) 2)))
     )
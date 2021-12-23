(ns clj.day8
  (:require [clj.filereader :as read-file]))

(def x '([["be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb" "fabcd" "edb"]
         ["fdgacbe" "cefdb" "cefbgd" "gcbe"]]))
(def testdata [["acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"] ["cdfeb" "fcadb" "cdfeb" "cdbaf"]])
(def data (->> (read-file/input "day8")
               (map (fn [line]
                      (-> line
                          (clojure.string/split #" ")
                          (#(vector (vec (take 10 %)) (vec (drop 11 %))))
                          )))
               ))

;; 1:2, 4:4, 7:3, 8:7
;; [0,6,9]:6, [2,3,5]:5

(defn isknown [c] (some #{c} [2 3 4 7]))
(defn getkey [c]
  (condp = c
    2 :1
    4 :4
    3 :7
    7 :8
    :other))
(defn str-to-binary [s] (reduce
                          #(bit-or (condp = %2
                                     \a 2r0000001
                                     \b 2r0000010
                                     \c 2r0000100
                                     \d 2r0001000
                                     \e 2r0010000
                                     \f 2r0100000
                                     \g 2r1000000) %1) 0 s))
(defn strings-to-binary [coll]
  (map str-to-binary coll))

(defn only [coll]
  (when-not (seq (rest coll))
    (when (seq coll)
      (first coll)
      )))

(defn getknown
  ([strings] (getknown strings str))
  ([strings str-eval-func]
   (loop [set {} strings strings other []]
     (if (empty? strings)
       (assoc set :other other)
       (let [s (first strings) c (count s)]
         (recur
           (if (isknown c) (assoc set (getkey c) (str-eval-func s)) set)
           (rest strings)
           (if (isknown c) other (conj other s))
           ))))))

(getknown (first testdata))
;; -> {:1 "ab"}
(getknown (first testdata) str-to-binary)
;; -> {:1 18}
(defn compare-bits [known coll]
  (reduce #(bit-and %1 %2) known coll))
;; 3 [3 5 7] -> 1
(defn str-has-bit [bit s]
  (= (min s bit) (bit-and bit s))
  )
(defn filter-of-length [length coll]
  (filter #(= length (count %)) coll)
  )
(defn remove-binary-from-strings [b strings]
  (filter #(not= (str-to-binary %) b) strings)
  )




;; First star
;;(defn countknown [strings] (reduce #(if (number? %2) (inc %1) %1) 0 (map #(isknown (count %)) strings)))
;;(reduce #(+ %1 (countknown %2)) 0 (map #(second %) data))

(defn map-binaries [coll]
  (as-> coll m
        (getknown m str-to-binary)
        ;;(assoc m :a (bit-xor (:7 m) (:1 m)))                ;; :a
        (assoc m :f (compare-bits (:1 m) (strings-to-binary (filter-of-length 6 (:other m)))))
        (assoc m :c (bit-xor (:1 m) (:f m)))
        (assoc m :6 (only (filter #(not (str-has-bit (:c m) %)) (strings-to-binary (filter-of-length 6 (:other m))))))
        (assoc m :other (remove-binary-from-strings (:6 m) (:other m)))
        (assoc m :9 (only (filter #(str-has-bit (:4 m) %) (strings-to-binary (filter-of-length 6 (:other m))))))
        (assoc m :other (remove-binary-from-strings (:9 m) (:other m)))
        (assoc m :0 (only (strings-to-binary (filter-of-length 6 (:other m))))
                 :other (filter-of-length 5 (:other m)))
        (assoc m :d (bit-xor (:0 m) (:8 m)))
        (assoc m :b (bit-xor (:1 m) (:d m) (:4 m)))
        (assoc m :5 (only (filter #(str-has-bit (:b m) %) (strings-to-binary (:other m)))))
        (assoc m :other (remove-binary-from-strings (:5 m) (:other m)))
        (assoc m :3 (only (filter #(str-has-bit (:f m) %) (strings-to-binary (:other m)))))
        (assoc m :2 (only (filter #(not (str-has-bit (:f m) %)) (strings-to-binary (:other m)))))
        (select-keys m [:0 :1 :2 :3 :4 :5 :6 :7 :8 :9])
        (clojure.set/map-invert m)
        ))
(defn decrypt [m coll]
  (Integer/parseInt
    (apply str (map #(name (get m (str-to-binary %))) coll)))
  )
(reduce
  #(+ %1 (decrypt (map-binaries (first %2)) (second %2)))
        0 data)

(ns clj.day16
  (:require [clj.filereader :as read-file]))
;; 0x0010 -> 16
;; 16r10  -> 16
;;(Integer/parseInt "FF" 16)
;;(Integer/toBinaryString 2021)
;;(Long/toBinaryString 16r38006F45291200)
(def ex1 "8A004A801A8002F478")
(def ex2 "D2FE28")
(def data "6053231004C12DC26D00526BEE728D2C013AC7795ACA756F93B524D8000AAC8FF80B3A7A4016F6802D35C7C94C8AC97AD81D30024C00D1003C80AD050029C00E20240580853401E98C00D50038400D401518C00C7003880376300290023000060D800D09B9D03E7F546930052C016000422234208CC000854778CF0EA7C9C802ACE005FE4EBE1B99EA4C8A2A804D26730E25AA8B23CBDE7C855808057C9C87718DFEED9A008880391520BC280004260C44C8E460086802600087C548430A4401B8C91AE3749CF9CEFF0A8C0041498F180532A9728813A012261367931FF43E9040191F002A539D7A9CEBFCF7B3DE36CA56BC506005EE6393A0ACAA990030B3E29348734BC200D980390960BC723007614C618DC600D4268AD168C0268ED2CB72E09341040181D802B285937A739ACCEFFE9F4B6D30802DC94803D80292B5389DFEB2A440081CE0FCE951005AD800D04BF26B32FC9AFCF8D280592D65B9CE67DCEF20C530E13B7F67F8FB140D200E6673BA45C0086262FBB084F5BF381918017221E402474EF86280333100622FC37844200DC6A8950650005C8273133A300465A7AEC08B00103925392575007E63310592EA747830052801C99C9CB215397F3ACF97CFE41C802DBD004244C67B189E3BC4584E2013C1F91B0BCD60AA1690060360094F6A70B7FC7D34A52CBAE011CB6A17509F8DF61F3B4ED46A683E6BD258100667EA4B1A6211006AD367D600ACBD61FD10CBD61FD129003D9600B4608C931D54700AA6E2932D3CBB45399A49E66E641274AE4040039B8BD2C933137F95A4A76CFBAE122704026E700662200D4358530D4401F8AD0722DCEC3124E92B639CC5AF413300700010D8F30FE1B80021506A33C3F1007A314348DC0002EC4D9CF36280213938F648925BDE134803CB9BD6BF3BFD83C0149E859EA6614A8C")

(def version 2r1110)
(def literal-val 2r00010000)
(def length-type-1 2r00000010)
(defn parse16char [char]
  (Integer/parseInt (str char) 16))
(defn parse16toBin [char] (clojure.string/replace (format "%4s" (Integer/toBinaryString (parse16char char))) #"\s" "0"))
;;(defn parse-input [string] (reduce #(str %1 (parse16toBin %2)) "" string))
(defn parse-input [string] (mapcat parse16toBin string))
(defn parse-bin [str] (Integer/parseInt str 2))
(defn take-n-str [num coll] (apply str (take num coll)))

;; take 2
;; Take first
(defn get-version [char] (bit-and version (parse16char char)))
;; Take first and second
(defn is-literal? [str-of-2] (< 0 (bit-and literal-val (Integer/parseInt str-of-2 16))))
(defn is-length-type-1? [str-of-2] (= length-type-1 (bit-and length-type-1 (Integer/parseInt str-of-2 16))))
;; if length type 0 ->15 bits for rest of length, + 0s > add 14 bits (+2)
;; if length type 1 ->11 bits for num of packets, + 0s > add 10 bits (+2)
;; 8 + 14 + 2 = 24 (6chars) remove 7 from top, 2 from bot, shift 2 right
;; 8 + 10 + 2 = 20 (5chars) remove 7 from top, 2 from bot, shift 2 right
(defn get-subseq-length [str-of-6] (let [r 16r01fffc] (bit-shift-right (bit-and r str-of-6) 2)))
(defn get-subseq-packets [str-of-5] (let [r 16r01ffc] (bit-shift-right (bit-and r str-of-5) 2)))

(defn remainder-to-4 [num] (+ num (- 4 (mod num 4)))) ;; 14 -> 16
(defn remainder-to-next-4 [num] (- 4 (mod num 4))) ;; 14 -> 2
;; length is easy.
;; drop current + remainder from str and continue

(defn run [result packsize step cache rest]
  (println step cache)
  (if (empty? rest)
    [result packsize rest]
    (condp = step
      nil (recur result (+ 3 packsize) 'version (take-n-str 3 rest) (drop 3 rest))
      'version (recur (+ result (parse-bin cache)) (+ 3 packsize) 'type (take-n-str 3 rest) (drop 3 rest))
      'type (if (= cache "100")
              (recur result (+ 5 packsize) 'literal (take-n-str 5 rest) (drop 5 rest))
              (recur result (+ 1 packsize) 'ltype (take-n-str 1 rest) (drop 1 rest)))
      'type (condp = (parse-bin cache)
              0 + ;; "sum"
              1 * ;; "product"
              2 min ;; "minimum"
              3 max ;; "maximum"
              4 (recur result (+ 5 packsize) 'literal (take-n-str 5 rest) (drop 5 rest));;#(%) "literal"
              5 #(if (> %1 %2) 1 0) ;; "greater than"
              6 #(if (< %1 %2) 1 0) ;; "less than"
              7 #(if (= %1 %2) 1 0) ;; "equal to"
              "operator out of range")
      'literal (if (= \1 (first cache))
                 (recur result (+ 5 packsize) 'literal (str (take-n-str 5 rest) (drop 1 cache)) (drop 5 rest))
                 [(+ result (parse-bin (drop 1 cache))) packsize rest])
      'ltype (let [ltype1 (= cache "1")]
                  (recur result
                         (if ltype1 (+ 11 packsize) (+ 15 packsize))
                         (if ltype1 'packets 'length)
                         (if ltype1 (take-n-str 11 rest) (take-n-str 15 rest))
                         (if ltype1 (drop 11 rest) (drop 15 rest))))
      'length (recur result packsize nil "" rest)
      ;;'length ((fn [[result packsize innerrest]] [result packsize (str innerrest rest)])
      ;;         (run  "" (take (parse-bin cache) rest)))
      'packets (reduce (fn [acc _] (run (first acc) (second acc) nil "" (last acc))) [result packsize rest] (range (parse-bin cache)))
      "No known step"))
  )

(apply str (parse-input "C200B40A82"))
(loop [result 0 packetsize 0 rest (parse-input "C200B40A82")]
  (if (or (empty? rest) (every? #(= \0 %) rest))
    result
   (let [[result packetsize rest] (run result packetsize nil "" rest)]
     ;;(recur result 0 (drop (remainder-to-next-4 packetsize) rest))
     (recur result packetsize rest)
     ))
  )

(reduce #(< %1 %2) [2 4])
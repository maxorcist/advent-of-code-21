(ns clj.day12)

(def x ["start-A"
        "start-b"
        "A-c"
        "A-b"
        "b-d"
        "A-end"
        "b-end"])
(def y ["fs-end"
        "he-DX"
        "fs-he"
        "start-DX"
        "pj-DX"
        "end-zg"
        "zg-sl"
        "zg-pj"
        "pj-he"
        "RW-he"
        "fs-DX"
        "pj-RW"
        "zg-RW"
        "start-pj"
        "he-WI"
        "zg-he"
        "pj-fs"
        "start-RW"])
(def input ["CV-mk"
            "gm-IK"
            "sk-gm"
            "ca-sk"
            "sx-mk"
            "gm-start"
            "sx-ca"
            "kt-sk"
            "ca-VS"
            "kt-ml"
            "kt-ca"
            "mk-IK"
            "end-sx"
            "end-sk"
            "gy-sx"
            "end-ca"
            "ca-ml"
            "gm-CV"
            "sx-kt"
            "start-CV"
            "IK-start"
            "CV-kt"
            "ml-mk"
            "ml-CV"
            "ml-gm"
            "ml-IK"])
(defn map-connections [input]
  (->> input
      (map #(clojure.string/split % #"-"))
      (reduce #(let [a (keyword (first %2)) b (keyword (second %2))]
                 (assoc %1
                   a (conj (a %1) b)
                   b (conj (b %1) a))) {})
      ))
(def testdatax (map-connections x))
(def testdatay (map-connections y))
(def data (map-connections input))
(defn is-small-cave? [k]
  (not (nil? (re-matches #"[a-z]+" (name k)))))

(defn remove-visited-small-and-self [visited togo]
  (let [self (first visited) small (filter is-small-cave? visited)]
    (remove (set (conj small self)) togo)))

(defn remove-visited-small-but-one-and-self [visited togo]
  (let [basic '((first visited) :start) small (filter is-small-cave? visited)]
    (let [removables (if (= (count small) (count (set small))) (set basic) (set (concat small basic)))]
     (remove removables togo))))
(->> ['(:start :a :A :b :A :start :g) '(:c :C :d :a :b :g :start)]
     ;;(#(remove-visited-small-and-self (first %)(second %)))
     (#(remove-visited-small-but-one-and-self (first %)(second %)))
     )

"whoami, where can I go (minus sender), visits list"
"visits list (first is me), list of where to go (not sender)"
"list of where to go (minus sender, minus small from visits)"
(defn find-routes ([map] (find-routes map '(:start) (:start map) '()))
  ([map visited togo completed]
   (if (= :end (first visited))
     (conj completed (reverse visited))
     (reduce #(find-routes map (conj visited %2) (%2 map) %1) completed (remove-visited-small-and-self visited togo))
     )))
(count (find-routes testdatax))

(defn find-routes2 ([map] (find-routes2 map '(:start) (:start map) '()))
  ([map visited togo completed]
   (if (= :end (first visited))
     (conj completed (reverse visited))
     (reduce #(find-routes2 map (conj visited %2) (%2 map) %1) completed (remove-visited-small-but-one-and-self visited togo))
     )))
(count (find-routes2 data))
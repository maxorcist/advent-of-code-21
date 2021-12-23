(ns clj.filereader
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]))

(defn read-file [file] (slurp (io/resource file)))
(defn split-by-newline [input] (clojure.string/split input #"\n"))

(defn input [file] (split-by-newline (read-file file)))
(defn json-read [filename] (map json/read-str (input filename)))

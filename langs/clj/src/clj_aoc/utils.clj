(ns clj-aoc.utils
  (:require [clojure.string :as str]))

(defn get-input
  "Reads the input file and returns a list of strings. Requires the year as yy and day as dd formats e.g. (get-input 15 1)"
  [year day]
  (slurp (str "../../inputs/" year "day" day)))

(defn get-input-lines
  "Splits the input at newline chars"
  [input]
  (str/split input #"\r?\n"))

;; TODO function to grab the input from the website
(ns clj-aoc.utils
  (:require [clojure.string :as str]))

(defn get-input
  "Reads the input file and returns a list of strings. Requires the year as yy and day as dd formats e.g. (get-input 15 1)"
  [year day]
  (slurp (str "../../inputs/" year "day" day)))

(defn split-lines
  "Splits the input at newline chars"
  [input]
  (str/split input #"\r?\n"))

;;;; Deprecated.
(defn ^:deprecated get-input-lines [x]
  (split-lines x))

;; TODO function to grab the input from the website

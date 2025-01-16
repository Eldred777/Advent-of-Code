(ns clj-aoc.utils)

(defn get-input
  "Reads the input file and returns a list of strings. Requires the year as yy and day as dd formats e.g. (get-input 15 1)"
  [year day]
  (slurp (str "../../inputs/" year "day" day)))

;; TODO function to grab the input from the website
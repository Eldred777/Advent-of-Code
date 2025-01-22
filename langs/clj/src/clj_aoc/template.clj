(ns clj-aoc.template
  (:require [clj-aoc.utils :as utils]))

(defn part1 [input]
  'part-1)

(defn part2 [input]
  'part-2)

(defn solve []
  (let [input (utils/get-input 15 1)]
    (println (str "Part 1:" (part1 input)))
    (println (str "Part 2:" (part2 input)))))

(solve)

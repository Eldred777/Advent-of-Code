(ns clj-aoc.y15.d1
  (:require [clj-aoc.utils :as utils]))

(defn part1 [input]
  (apply max
         (reductions +
                     (map (fn [c] (if (= c (first "(")) 1 -1))
                          input))))

(defn part2 [input]
  (let [cumsum (reductions + (map #(if (= % (first "(")) 1 -1) input))]
    (+ 1 ; fence post counting   
       (first (keep-indexed #(when (= %2 -1) %1) cumsum)))))

(defn solve []
  (let [input (utils/get-input 15 1)]
    (println (format "Part 1: %s" (part1 input)))
    (println (format "Part 2: %s" (part2 input)))))

(solve)

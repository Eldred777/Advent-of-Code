(ns y15.d1
  (:require [utils])
  )

(defn part1 [input]
  (apply max
         (reductions +
                     (map (fn [c] (if (= c (first "(")) 1 -1))
                          input))))

(+ 1 2)

(println (part1 (utils/get-input 15 1)))

(defn part2 [input]
  2)

(defn solve []
  (let [input (utils/get-input 15 1)]
    (println (format "Part 1: %s" (part1 input)))
    (println (format "Part 2: %s" (part2 input)))))

(solve)
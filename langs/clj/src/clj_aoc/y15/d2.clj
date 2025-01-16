(ns clj-aoc.y15.d2
  (:require [clj-aoc.utils :as utils]
            [clojure.string :as str]))

(defn process-dimension
  "Processes `dimension-str` to a list of dimensions."
  [dimension-str]
  (as-> dimension-str s
    (str/split s #"x")
    (map #(Integer/parseInt %) s)))

(defn process-input [input]
  (as-> input i
    (utils/get-input-lines i)
    (map process-dimension i)))

(defn face-areas [dimension-list]
  (let [l (nth dimension-list 0)
        w (nth dimension-list 1)
        h (nth dimension-list 2)]
    (list (* l w)
          (* w h)
          (* l h))))

(defn part1 [dims]
  (let [areas (map face-areas dims)]
    (+
     (->> areas
          (map #(apply min %))
          (reduce +))
     (->> areas
          (map #(reduce + %))
          (reduce +)
          (* 2)))))

(defn vol [dimensions]
  (apply * dimensions))

(defn least-perimeter [dimensions]
  (let [l (nth dimensions 0)
        w (nth dimensions 1)
        h (nth dimensions 2)]
    (apply min (list (* 2 (+ l w))
                     (* 2 (+ w h))
                     (* 2 (+ l h))))))

(defn part2 [dims]
  (reduce + (map  +
                  (map vol dims)
                  (map least-perimeter dims))))

(defn solve []
  (let [input (utils/get-input 15 2)
        dims (process-input input)]
    (println (str "Part 1: " (part1 dims)))
    (println (str "Part 2: " (part2 dims)))))

(solve)
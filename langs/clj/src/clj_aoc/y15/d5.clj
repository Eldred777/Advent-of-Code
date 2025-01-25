(ns clj-aoc.y15.d5
  (:require [clj-aoc.utils :as utils]))

(defn test-regexes [regexes s]
  (every? identity (map #(re-find % s) regexes)))

(comment
  (test-regexes '(#"a" #"b") "abc")
  (test-regexes '(#"a" #"b" #"d") "abc")
  (test-regexes '(#"([a-z])[a-z]\1") "aba"))

(defn pt1-nice? [s]
  (test-regexes '(#"([aeiou]).*([aeiou]).*([aeiou])"
                  #"([a-z])\1"
                  #"^((?!ab|cd|pq|xy).)*$")
                s))

(comment
  (pt1-nice? "ugknbfddgicrmopn") ; nice
  (pt1-nice? "aaa") ; nice 
  (pt1-nice? "jchzalrnumimnmhp") ; naughty
  (pt1-nice? "haegwjzuvuyypxyu") ; naughty
  (re-find #"^((?!ab|cd|pq|xy).)*$" "haegwjzuvuyypxyu")
  (pt1-nice? "dvszwmarrgswjxmb") ; naughty 
  )

(defn pt2-nice? [s]
  (test-regexes '(#"(.)(.).*?\1\2"
                  #"(.).\1")
                s))

(comment
  (map #(re-find #"(.)(.).*?\1\2" %)
       '("qjhvhtzxzqqjkmpb"))
  (pt2-nice? "qjhvhtzxzqqjkmpb")
  (pt2-nice? "uurcxstgmygtbstg")
  (pt2-nice? "ieodomkazucvgmuy"))

(defn part1 [lines]
  (->> lines
       (map pt1-nice?)
       (map #(if % 1 0))
       (apply +)))

(defn part2 [lines]
  (->> lines
       (map pt2-nice?)
       (map #(if % 1 0))
       (apply +)))

(defn solve []
  (let [input (utils/get-input 15 5)
        lines (utils/split-lines input)]
    (println (str "Part 1:" (part1 lines)))
    (println (str "Part 2:" (part2 lines)))))

(comment
  (solve)
  (-> (utils/get-input 15 5)
      utils/split-lines
      part1))
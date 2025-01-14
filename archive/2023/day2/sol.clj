(ns sol
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

;; part 1 

(def max-balls  {:red 12 :green 13 :blue 14})
;; "In format [R G B]"

(defn get-game-id [game-string]
  (-> game-string
      (str/split #"(?:[: ])")
      second
      Integer/parseInt))

(defn parse-state-build-hash-map [current-hash-map states-vec]
  (if (empty? states-vec)
    current-hash-map ;; return 
    (let [x (first states-vec)]
      (parse-state-build-hash-map
       (assoc current-hash-map (keyword (second x))
              (Integer/parseInt (first x)))
       (rest states-vec)))))

(defn parse-state [state-string]
  (as-> state-string s ;; "5 green, 3 red"
    (str/split s #", ") ;; ["5 green", "3 red"]
    (map #(str/split % #" ") s) ;; [["5", "green"], ["3", "red"]]
    (parse-state-build-hash-map {} s)))

(defn parse-game [game-string]
  (as-> game-string s
    (str/split s #"(?:[:;] )")
    (drop 1 s) ;; drop game identifier
    (map parse-state s)))

(defn valid-state? [state]
  (and
   (<= (get state :red 0)
       (get max-balls :red))
   (<= (get state :green 0)
       (get max-balls :green))
   (<= (get state :blue 0)
       (get max-balls :blue))))

(defn valid-game? [game]
  (reduce #(and %1 %2)
          (map valid-state? game)))

(defn valid-game-string? [game-string]
  (-> game-string
      parse-game
      valid-game?))

;; part 2 

(defn min-number-of-cubes-for-key [game key]
  (reduce #(max %1 (get %2 key 0))
          0
          game))

(defn min-number-of-cubes [game]
  {:red   (min-number-of-cubes-for-key game :red)
   :blue  (min-number-of-cubes-for-key game :blue)
   :green (min-number-of-cubes-for-key game :green)})

(defn set-power [cubes]
  (* (get cubes :red)
     (get cubes :blue)
     (get cubes :green)))

(defn part1 []
  (when-not (.exists (io/file "../day2/input"))
    (throw (AssertionError. "File not found")))
  (let [rdr (slurp "../day2/input")]
    (->> rdr
         str/split-lines
         (filter valid-game-string?)
         (map get-game-id)
         (reduce +)
         println)))

(defn part2 []
  (when-not (.exists (io/file "../day2/input"))
    (throw (AssertionError. "File not found")))
  (let [rdr (slurp "../day2/input")]
    (->> rdr
         str/split-lines
         (map parse-game)
         (map min-number-of-cubes)
         (map set-power)
         (reduce +)
         println)))

(part1)
(part2)

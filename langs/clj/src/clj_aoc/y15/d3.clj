(ns clj-aoc.y15.d3
  (:require [clj-aoc.utils :as utils]))

(defn char-to-direction [character]
  (case character
    \> '(1 0)
    \< '(-1 0)
    \v '(0 -1)
    \^ '(0 1)
    '(0 0)))


(defn process-directions [input]
  (map char-to-direction input))

(defn add-2d [x y]
  (list (+ (first x)
           (first y))
        (+ (second x)
           (second y))))

(defn get-pos [map]
  (get map :pos))

(defn get-pos2 [map]
  (get map :pos2))

(defn update-pos [map pos]
  (assoc map :pos pos))

(defn init-map []
  {:pos '(0 0)
   '(0 0) 1})

(defn init-map2 []
  {:pos '(0 0)
   :pos2 '(0 0) ; used for part 2
   '(0 0) 2})


(defn map-add [travel-map direction]
  (let [pos (get-pos travel-map)
        new-pos (add-2d pos direction)
        count-at-new-pos (get travel-map new-pos 0)]
    (-> travel-map
        (assoc new-pos (+ count-at-new-pos 1))
        (update-pos new-pos))))

(defn travel [map directions]
  (reduce map-add map directions))

(defn finalise-map [map]
  (-> map
      (dissoc :pos)
      (dissoc :pos2)))

(defn count-houses [travel-map]
  (-> travel-map
      (finalise-map)
      (count)))

(defn part1 [input]
  (count-houses (travel (init-map)
                        (process-directions input))))

(defn swap-pos [map]
  (let [pos (get-pos map)
        pos2 (get-pos2 map)]
    (-> map
        (assoc :pos pos2)
        (assoc :pos2 pos))))

(defn part2-helper
  "`maps` is a collection of 2 maps."
  [map directions]
  (if (empty? directions)
    ; base case 
    map
    ; main logic 
    (let [direction (first directions)
          new-map (map-add map direction)]
      (part2-helper (swap-pos new-map) (rest directions)))))

(defn part2
  "Relies on the fact there is only one robo-santa"
  [input]
  (->> input
       (process-directions)
       (part2-helper (init-map2))
       (count-houses)))

(part2-helper (init-map2) (process-directions "^v^v"))
(->> "^>v<"
     (process-directions)
     (part2-helper (init-map2)))
    ;;  (count-houses)

(part2 "^v^v^v^v^v")

(defn solve []
  (let [input (utils/get-input 15 3)]
    (println "Part 1:" (part1 input))
    (println "Part 2:" (part2 input))))

(solve) ; StackOverflowError

(part2 "^>v<")

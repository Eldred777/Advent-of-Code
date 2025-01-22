(ns clj-aoc.y15.d4
  (:import java.math.BigInteger))

(defn compute-hash [s]
  (let [digest (doto (java.security.MessageDigest/getInstance "md5")
                 (.reset)
                 (.update (.getBytes s "UTF-8")))
        hash (.digest digest)]
    (->> hash
         (java.math.BigInteger. 1)
         (format "%032x"))))

(defn part1 [input]
  (loop [i 0]
    (if (as-> i i
          (str input i)
          (compute-hash i)
          (.startsWith i "00000"))
      i
      (recur (inc i)))))

(defn part2 [input]
  (loop [i 0]
    (if (as-> i i
          (str input i)
          (compute-hash i)
          (.startsWith i "000000"))
      i
      (recur (inc i)))))

(defn solve []
  (let [input "ckczppom"]
    (println (str "Part 1:" (part1 input)))
    (println (str "Part 2:" (part2 input)))))

(solve)

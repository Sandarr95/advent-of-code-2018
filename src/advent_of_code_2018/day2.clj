(ns advent-of-code-2018.day2
  (:require [clojure.edn :as edn]))

(defonce input (edn/read-string (slurp "resources/day2.edn")))

(defn part1 [input]
  (let [fqss (map (comp vals frequencies) input)]
    (apply * (reduce
              (fn [[twos threes] fqs]
                (vector
                 (if (some (partial = 2) fqs) (inc twos) twos)
                 (if (some (partial = 3) fqs) (inc threes) threes)))
              [0 0] fqss))))

(defn diff-chs [grouped-chars]
  (reduce
   (fn [uneq [v1 v2]]
     (if (not= v1 v2)
       (inc uneq)
       uneq))
   0 grouped-chars))

(defn part2 [input]
  (let [charss (map vec input)]
    (reduce
     (fn [_ chs]
       (if-let
        [val (reduce
              (fn [_ cmps]
                (let [grouped (map vector chs cmps)
                      diff-n (diff-chs grouped)]
                  (if (= diff-n 1)
                    (reduced (apply str (map first (filter #(= (first %) (second %)) grouped)))))))
              nil charss)]
         (reduced val)))
     nil charss)))

(defn -main []
  (println (part1 input))
  (println (part2 input)))
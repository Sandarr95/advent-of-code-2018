(ns advent-of-code-2018.day1
  (:require [clojure.edn :as edn]))

(defonce input (edn/read-string (slurp "resources/day1.edn")))

(defn part1 [input]
  (reduce + input))

(defn part2 [input]
  (let [init {:fq 0 :fqs #{0}}]
    (reduce
      (fn [{:keys [fq fqs]} offset]
        (let [new-fq (+ fq offset)]
          ;(println (str "First " new-fq ": " (contains? fqs new-fq)))
          (if (contains? fqs new-fq)
            (reduced new-fq)
            {:fq new-fq :fqs (conj fqs new-fq)})))
      init (cycle input))))

(defn -main []
  (println (part1 input))
  (println (part2 input)))
(ns advent-of-code-2018.day3
  (:require [clojure.edn :as edn]
            [clojure.set :as set]))

(defonce input (edn/read-string (slurp "resources/day3.edn")))

(defn mark-spot [fabric x y id]
  (update fabric x
    #(update %1 y
      (fn [claims]
        (conj claims id)))))

(defn get-all-coords [[id {:keys [x y dx dy]}]]
  (for [x (range x (+ x dx)) y (range y (+ y dy))]
    {:id id :coord (vector x y)}))

(defn part1 [input]
  (let [maxx (reduce #(max %1 (+ (:x %2) (:dx %2))) 0 (vals input))
        maxy (reduce #(max %1 (+ (:y %2) (:dy %2))) 0 (vals input))
        fabric (vec (repeat maxx (vec (repeat maxy #{}))))
        coord-ids (mapcat get-all-coords input)
        claimed (reduce (fn [f {:keys [id coord]}] (mark-spot f (first coord) (second coord) id)) fabric coord-ids)]
    (count (filter (comp #(> % 1) count) (mapcat identity claimed)))))

(defn part2 [input]
  (let [maxx (reduce #(max %1 (+ (:x %2) (:dx %2))) 0 (vals input))
        maxy (reduce #(max %1 (+ (:y %2) (:dy %2))) 0 (vals input))
        fabric (vec (repeat maxx (vec (repeat maxy #{}))))
        coord-ids (mapcat get-all-coords input)
        claimed (reduce (fn [f {:keys [id coord]}] (mark-spot f (first coord) (second coord) id)) fabric coord-ids)
        claim-ids (into #{} (keys input))
        non-overlapping (reduce #(if (> (count %2) 1) (set/difference %1 %2) %1) claim-ids (mapcat identity claimed))]
  (name (first non-overlapping))))

(defn -main []
  (println (part1 input))
  (println (part2 input)))
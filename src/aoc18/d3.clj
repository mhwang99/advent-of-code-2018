(ns aoc18.d3
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))

(defn q1 [ll]
  (let [points (reduce (fn [ret [_ sx sy lx ly]]
                         (concat
                           ret
                           (for [x (range sx (+ sx lx))
                                 y (range sy (+ sy ly))]
                             [x y])))
                       [] ll)]
    (->> points frequencies (filter #(> (second %) 1)) count)))

(defn q2 [ll]
  (let [claims (into {} (mapv (fn [[id sx sy lx ly]]
                                [id (for [x (range sx (+ sx lx))
                                          y (range sy (+ sy ly))]
                                      [x y])])
                              ll))
        points (apply concat (vals claims))
        multi (->> points frequencies (filter #(> (second %) 1)) keys)]
    (->> claims
         (remove (fn [[_ points]]
                   (some (set points) multi)))
         (mapv first))))

(def in (get-line-num))
(q1 in)
(q2 in)

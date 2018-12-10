(ns aoc18.d10
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))

(def in (get-line-num))

(defn print-board [ps]
  (let [ys (mapv second ps)
        miny (apply min ys)
        maxy (apply max ys)]
    (when (<= (- maxy miny) 10)
      (let [xs (mapv first ps)
            minx (apply min xs)
            maxx (apply max xs)
            board (delay
                    (reduce (fn [board [x y]]
                              (assoc-in board [(- y miny) (- x minx)] \#))
                            (vec (repeat (- maxy miny -1)
                                (vec (repeat (- maxx minx -1) \space)))) ps))
            board2 (reduce (fn [board [x y]]
                             (assoc-in board [(- x minx) (- y miny)] \#))
                           (vec (repeat (- maxx minx -1)
                                (vec (repeat (- maxy miny -1) \space)))) ps)]
        (when (some (fn [l]  (every? #(= % \#) l)) board2)
          (doseq [xs @board]
            (println (apply str xs)))
          true)))))


(defn q0 [l]
  (let [[ps vs] (reduce (fn [[ps vs] [a b c d]]
                          [(conj ps [a b]) (conj vs [c d])])
                        [[] []] l)]
    (loop [n 0
           ps ps]
      (if (>= n 100000)
        nil
        (if-not (print-board ps)
          (recur (inc n)
                 (mapv (fn [[x y] [vx vy]]
                         [(+ x vx) (+ y vy)])
                       ps vs))
          n)))))

(q0 in)

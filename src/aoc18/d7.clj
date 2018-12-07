(ns aoc18.d7
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))

(defn q1
  [[client-map parent-map]]
  (loop [done []
         queue (set (remove (set (keys client-map)) (keys parent-map)))]
    (if (empty? queue)
      (apply str done)
      (let [c (char (apply min (mapv int queue)))
            done (conj done c)
            queue (disj queue c)
            new-queue (reduce (fn [ps p]
                                (if (every? (set done) (get client-map p))
                                  (conj ps p) ps))
                              [] (get parent-map c))]
          (recur done (apply conj queue new-queue))))))

(defn q2
  [[client-map parent-map] worker-cnt time-take]
  (loop [done []
         n 0
         working {}
         queue (set (remove (set (keys client-map)) (keys parent-map)))]
    (if (and (empty? queue)
             (empty? working))
      (dec n)
      (let [work-done (keys (filter (fn [[_ tm]] (= tm 0)) working))
            working (apply dissoc working work-done)
            done (if (seq work-done) (apply conj done work-done) done)
            queue (if (empty? work-done) queue
                    (->> (mapcat #(get parent-map %) work-done)
                         (reduce (fn [ps p]
                                   (if (every? (set done) (get client-map p))
                                     (conj ps p) ps)) [])
                         (apply conj queue)))
            working-new (take (- worker-cnt (count working)) (sort queue))
            working (->> working-new
                         (mapv (fn [c] [c (+ time-take (- (int c) (int \A) -1))]))
                         (apply conj working)
                         (mapv (fn [[c tm]] [c (dec tm)]))
                         (into {}))
            queue (apply disj queue working-new)]
        (recur done (inc n) working queue)))))

(def in
  (->> (get-line-split " ")
       (mapv (fn [l] [(first (nth l 1)) (first (nth l 7))]))
       (reduce (fn [[client-map parent-map] [k v]]
                 [(update client-map v conj k)
                  (update parent-map k conj v)])
               [{} {}])))
(q1 in)
(q2 in 6 60)

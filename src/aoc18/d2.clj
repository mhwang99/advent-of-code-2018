(ns aoc18.d2
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))

(defn q1 [l]
  (let [[a b] (->> l
                   (mapv (fn [s]
                           (let [cs (-> s frequencies vals set)]
                             [(if (cs 2) 1 0)
                              (if (cs 3) 1 0)])))
                   (reduce (fn [[aa bb] [a b]] [(+ aa a) (+ bb b)])))]
    (* a b)))

(defn get-same
  [a b]
  (->> (mapv (fn [a b] (when (= a b) a)) a b)
       (filter identity)))

(defn q2 [l]
  (let [cnt (count (first l))]
    (loop [li l]
      (let [s (first li)
            ret (loop [lj (rest li)]
                  (when-not (empty? lj)
                    (let [ret (get-same s (first lj))]
                      (if (= (count ret) (dec cnt))
                        ret
                        (recur (rest lj))))))]
        (if ret
          (apply str ret)
          (recur (rest li)))))))

(def in (get-split))
(q1 in)
(q2 in)

(ns aoc18.d1
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))

(defn q1 [ll]
  (apply + ll))

(defn q2 [ll]
  (loop [s #{0}
         n 0
         l ll]
    (let [n (+ n (first l))]
      (if (s n)
        n
        (recur (conj s n) n
               (if (seq (rest l))
                    (rest l)
                    ll))))))

(def in (get-num))
(q1 in)
(q2 in)


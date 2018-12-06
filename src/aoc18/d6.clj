(ns aoc18.d6
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))


(defn dif [a b] (if (> a b) (- a b) (- b a)))
(defn dif2 [[ax ay] [bx by]] (+ (dif ax bx) (dif ay by)))

(defn get-nth [l i]
  [(vec (concat (take i l) (drop (inc i) l))) (nth l i)])

(defn limited?
  [l [ix iy]]
  (and (some (fn [[x y]] (and (< x ix) (<= (dif y iy) (dif x ix)))) l)
       (some (fn [[x y]] (and (> x ix) (<= (dif y iy) (dif x ix)))) l)
       (some (fn [[x y]] (and (< y iy) (<= (dif x ix) (dif y iy)))) l)
       (some (fn [[x y]] (and (> y iy) (<= (dif x ix) (dif y iy)))) l)))

(defn get-grid-count
  [l [ix iy]]
  (loop [s #{}
         ps [[[ix iy] 0]]
         ret 0]
    (let [[ps n s] (reduce (fn [[ps n s] [[ix iy] len]]
                             (if (and (not (s [ix iy]))
                                      (not-any? #(<= (dif2 % [ix iy]) len) l))
                               [(conj ps
                                      [[(inc ix) iy] (inc len)]
                                      [[(dec ix) iy] (inc len)]
                                      [[ix (inc iy)] (inc len)]
                                      [[ix (dec iy)] (inc len)])
                                (inc n)
                                (conj s [ix iy])]
                               [ps n (conj s [ix iy])]))
                           [[] 0 s] ps)]
      (if (= n 0)
        (do  ret)
        (recur s ps (+ ret n))))))

(defn q1 [ll]
  (->> (range (count ll))
       (mapv (fn [i]
               (let [[l p] (get-nth ll i)]
                 (if-not (limited? l p)
                   0
                   (get-grid-count l p)))))
       (apply max)))

(def in (get-line-num))
(def sample [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])
(q1 in)

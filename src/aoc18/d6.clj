(ns aoc18.d6
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))


(defn dif
  [a b]
  (if (number? a)
    (if (> a b) (- a b) (- b a))
    (let [[ax ay] a
          [bx by] b]
      (+ (dif ax bx) (dif ay by)))))

(defn get-nth [l i]
  [(vec (concat (take i l) (drop (inc i) l))) (nth l i)])

(defn limited?
  [l [ix iy]]
  (and (some (fn [[x y]] (and (< x ix) (<= (dif y iy) (dif x ix)))) l)
       (some (fn [[x y]] (and (> x ix) (<= (dif y iy) (dif x ix)))) l)
       (some (fn [[x y]] (and (< y iy) (<= (dif x ix) (dif y iy)))) l)
       (some (fn [[x y]] (and (> y iy) (<= (dif x ix) (dif y iy)))) l)))

(defn get-grid-count
  [[ix iy] fn-check]
  (loop [s #{}
         ps #{[ix iy]}
         ret 0
         len 0]
    (let [[ps n s] (reduce (fn [[ps n s] [ix iy]]
                             (if (and (not (s [ix iy]))
                                      (fn-check [ix iy] len))
                               [(conj ps
                                      [(inc ix) iy] [ix (inc iy)]
                                      [(dec ix) iy] [ix (dec iy)])
                                (inc n)
                                (conj s [ix iy])]
                               [ps n (conj s [ix iy])]))
                           [[] 0 s] ps)]
      (if (= n 0)
        ret
        (recur s ps (+ ret n) (inc len))))))

(defn q1 [ll]
  (->> (range (count ll))
       (mapv (fn [i]
               (let [[l p] (get-nth ll i)]
                 (if-not (limited? l p) 0
                   (get-grid-count p (fn [p len]
                                     (not-any? #(<= (dif % p) len) l)))))))
       (apply max)))

(defn q2 [ll mx]
  (let [ax (->> (mapv first ll) (apply +) (#(/ % (count ll))) int)
        ay (->> (mapv last ll) (apply +) (#(/ % (count ll))) int)]
    (get-grid-count [ax ay] (fn [p _]
                              (< (apply + (mapv #(dif % p) ll)) mx)))))

(def in (get-line-num))
(q1 in)
(q2 in 10000)

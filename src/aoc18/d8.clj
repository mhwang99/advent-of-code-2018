(ns aoc18.d8
  (:require [aoc18.core :as aoc :refer :all]))

(defn dec-first [l]
  (let [[v & l] l]
    (conj l (dec v))))

(defn q1 [l]
  (loop [cl (list (first l))
         ml (list (second l))
         l (drop 2 l)
         ret 0]
    (if (empty? l)
      ret
      (if (= (first cl) 0)
        (recur (rest cl)
               (rest ml)
               (drop (first ml) l)
               (+ ret (apply + (take (first ml) l))))
        (recur (conj (dec-first cl) (first l))
               (conj ml (second l))
               (drop 2 l)
               ret)))))

(defn get-val [vl is]
  (if (empty? vl)
    (apply + is)
    (let [vl (vec (reverse vl))]
      (reduce (fn [ret i]
                (+ ret (get vl (dec i) 0)))
              0 is))))

(defn q2 [l]
  (loop [cl (list (first l))
         ml (list (second l))
         l (drop 2 l)
         vl '(() ())]
    (if (empty? l)
      vl
      (if (= (first cl) 0)
        (recur (rest cl)
               (rest ml)
               (drop (first ml) l)
               (let [[cv pv & vl] vl
                     is (take (first ml) l)]
                 (conj vl (conj pv (get-val cv is)))))
        (recur (conj (dec-first cl) (first l))
               (conj ml (second l))
               (drop 2 l)
               (conj vl '()))))))

(def in (get-num))
(q1 in)
(q2 in)

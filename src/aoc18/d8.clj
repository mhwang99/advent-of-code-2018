(ns aoc18.d8
  (:require [aoc18.core :refer :all]))

(defn q0 [f l]
  (loop [cl (list (first l))
         ml (list (second l))
         l (drop 2 l)
         vl '(() ())]
    (if (empty? l)
      (ffirst vl)
      (if (= (first cl) 0)
        (recur (rest cl)
               (rest ml)
               (drop (first ml) l)
               (let [[cv pv & vl] vl
                     is (take (first ml) l)]
                 (conj vl (conj pv (f cv is)))))
        (recur (-> (let [[v & cl] cl]
                     (conj cl (dec v)))
                   (conj (first l)))
               (conj ml (second l))
               (drop 2 l)
               (conj vl '()))))))

(def q1 (partial q0 (fn [vl is]
                      (+ (reduce + vl) (reduce + is)))))

(def q2 (partial q0 (fn [vl is]
                      (if (empty? vl)
                        (apply + is)
                        (let [vl (vec (reverse vl))]
                          (reduce (fn [ret i]
                                    (+ ret (get vl (dec i) 0)))
                                  0 is))))))

(q1 (get-num))
(q2 (get-num))

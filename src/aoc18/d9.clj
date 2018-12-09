(ns aoc18.d9
  (:require [aoc18.core :as aoc :refer :all]))

(defn prv [a] (-> a deref (nth 1)))
(defn nxt [a] (-> a deref (nth 2)))
(defn vol [a] (-> a deref first))

(defn q0 [[e m]]
  (let [l (vector (atom [0 nil nil])
                  (atom [2 nil nil])
                  (atom [1 nil nil]))
        _ (do (swap! (nth l 0) assoc 1 (nth l 2) 2 (nth l 1))
              (swap! (nth l 1) assoc 1 (nth l 0) 2 (nth l 2))
              (swap! (nth l 2) assoc 1 (nth l 1) 2 (nth l 0)))
        score (mapv #(atom %) (repeat e 0))]
    (loop [c (get l 1)
           n 2
           idx 2]
      (if (>= n m)
        (apply max (map deref score))
        (if (= 0 (mod (inc n) 23))
          (let [c (-> c   prv prv prv   prv prv prv   prv)]
            (swap! (get score idx) #(+ % 1 (vol c) n))
            (swap! (-> c prv) assoc 2 (-> c nxt))
            (swap! (-> c nxt) assoc 1 (-> c prv))
            (recur (nxt c)  (inc n) (mod (inc idx) e)))
          (let [nc (atom [(inc n) (-> c nxt) (-> c nxt nxt)])]
            (swap! (-> nc prv) assoc 2 nc)
            (swap! (-> nc nxt) assoc 1 nc)
            (recur nc (inc n) (mod (inc idx) e))))))))

(defn q1 [l] (q0 l))
(defn q2 [l] (-> l
                 (update 1 #(* 100 %))
                 q0))
#_(time (q1 (get-num)))
#_(time (q2 (get-num)))

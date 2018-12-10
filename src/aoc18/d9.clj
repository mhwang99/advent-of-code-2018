(ns aoc18.d9
  (:require [aoc18.core :as aoc :refer :all]))

(defprotocol INode
  (prv [this])
  (nxt [this])
  (set-prev [this node])
  (set-next [this node]))

(deftype Node [value ^{:volatile-mutable true} prev-node ^{:volatile-mutable true} next-node]
  INode
  (prv [_] prev-node)
  (nxt [_] next-node)
  (set-prev [_ node] (set! prev-node node))
  (set-next [_ node] (set! next-node node)))

(defn q0 [[e m]]
  (let [c (Node. 0 nil nil)
        score (zipmap (range e) (repeat 0))]
    (set-prev c c)
    (set-next c c)
    (loop [c c
           n 0
           score score]
      (if (>= n m)
        (apply max (vals score))
        (if (= 0 (mod (inc n) 23))
          (let [c (-> c   prv prv prv   prv prv prv   prv)]
            (set-next (-> c prv) (-> c nxt))
            (set-prev (-> c nxt) (-> c prv))
            (recur (nxt c) (inc n)
                   (update score (mod n e) #(+ % (.value c) (inc n)))))
          (let [nc (Node. (inc n) (-> c nxt) (-> c nxt nxt))]
            (set-next (-> nc prv) nc)
            (set-prev (-> nc nxt) nc)
            (recur nc (inc n) score)))))))

(defn q1 [l] (q0 l))
(defn q2 [l] (q0 (update l 1 #(* 100 %))))

#_(time (q1 (get-num)))
#_(time (q2 (get-num)))

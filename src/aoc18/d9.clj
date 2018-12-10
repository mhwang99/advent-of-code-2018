(ns aoc18.d9
  (:require [aoc18.core :as aoc :refer :all]))

(defprotocol INode
  (prv [this])
  (nxt [this])
  (vol [this])
  (set-pn [this p n])
  (set-prev [this node])
  (set-next [this node]))

(defn setv [l idx f]
  (swap! (get l idx) f))

(deftype Node [value ^{:volatile-mutable true} prev-node ^{:volatile-mutable true} next-node]
  INode
  (prv [_] prev-node)
  (nxt [_] next-node)
  (vol [_] value)
  (set-pn [_ p n] (set! prev-node p) (set! next-node n))
  (set-prev [_ node] (set! prev-node node))
  (set-next [_ node] (set! next-node node)))

(defn q0 [[e m]]
  (let [a (Node. 0 nil nil)
        b (Node. 2 nil nil)
        c (Node. 1 nil nil)
        _ (do (set-pn a c b)
              (set-pn b a c)
              (set-pn c b a))
        score (mapv #(atom %) (repeat e 0))]
    (loop [c b
           n 2
           idx 2]
      (if (>= n m)
        (apply max (map deref score))
        (if (= 0 (mod (inc n) 23))
          (let [c (-> c   prv prv prv   prv prv prv   prv)]
            (setv score idx #(+ % 1 (vol c) n))
            (set-next (-> c prv) (-> c nxt))
            (set-prev (-> c nxt) (-> c prv))
            (recur (nxt c)  (inc n) (mod (inc idx) e)))
          (let [nc (Node. (inc n) (-> c nxt) (-> c nxt nxt))]
            (set-next (-> nc prv) nc)
            (set-prev (-> nc nxt) nc)
            (recur nc (inc n) (mod (inc idx) e))))))))

(defn q1 [l] (q0 l))
(defn q2 [l] (-> l
                 (update 1 #(* 100 %))
                 q0))
#_(time (q1 (get-num)))
#_(time (q2 (get-num)))

(ns aoc18.d13
  (:require [aoc18.core :refer :all]))

(defn get-rules-carts [l]
  (loop [yl l
         y 0
         [rule cart] [{} []]]
    (if (empty? yl)
      [rule cart]
      (recur (rest yl) (inc y)
             (loop [xl (first yl)
                    x 0
                    [rule cart] [rule cart]]
               (if (empty? xl)
                 [rule cart]
                 (recur (rest xl) (inc x)
                        (case (first xl)
                          \v [rule (conj cart [[x y] :d (cycle [:l :s :r])])]
                          \> [rule (conj cart [[x y] :r (cycle [:l :s :r])])]
                          \^ [rule (conj cart [[x y] :u (cycle [:l :s :r])])]
                          \< [rule (conj cart [[x y] :l (cycle [:l :s :r])])]
                          \\ [(assoc rule [x y]
                                     (fn [d turns]
                                       [(-> {:d :r :u :l :r :d :l :u} d)
                                        turns]))
                              cart]
                          \/ [(assoc rule [x y]
                                     (fn [d turns]
                                       [(-> {:d :l :u :r :r :u :l :d} d)
                                        turns]))
                              cart]
                          \+ [(assoc rule [x y]
                                     (fn [d turns]
                                       (let [turn (first turns)]
                                         [(-> {:l {:d :r :r :u :u :l :l :d}
                                               :s {:d :d :r :r :u :u :l :l}
                                               :r {:d :l :l :u :u :r :r :d}} turn d)
                                          (rest turns)])))
                              cart]
                          [rule cart]))))))))

(defn go-next
  [[x y] d]
  (case d
    :r [(inc x) y]
    :l [(dec x) y]
    :d [x (inc y)]
    [x (dec y)]))

(defn q0
  [f l]
  (let [[rules carts] (get-rules-carts l)]
    (loop [carts carts]
      (let [l (loop [ps (set (map first carts))
                     nps #{}
                     crashed []
                     ncarts []
                     carts carts]
                (let [[[p d turns] & carts] carts
                      ps (disj ps p)
                      np (go-next p d)
                      [d turns] ((get rules np vector) d turns)
                      [carts ncarts crashed] (cond
                                               (ps np) [(remove (fn [[p]] (= p np)) carts)
                                                        ncarts
                                                        (conj crashed np)]
                                               (nps np) [carts
                                                         (remove (fn [[p]] (= p np)) ncarts)
                                                         (conj crashed np)]
                                               :else [carts (conj ncarts [np d turns]) crashed])
                      nps (conj nps np)]
                  (if (empty? carts)
                    [ncarts crashed]
                    (recur ps nps crashed ncarts carts))))]
        (if-let [ret (f l)]
          ret
          (recur (sort-by (fn [[[x y]]] [y x]) (first l))))))))

(def q1 (partial q0 (fn [[_ crashed]] (when-let [[x y] (first crashed)]
                                        (str x "," y)))))

(def q2 (partial q0 (fn [[remain _]] (when (< (count remain) 2)
                                       (let [[x y] (ffirst remain)]
                                         (str x "," y))))))

(def in (get-split))
(time (q1 in)) ;83,121
(time (q2 in)) ;102,144

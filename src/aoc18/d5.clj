(ns aoc18.d5
  (:require [clojure.string :as s]
            [aoc18.core :as aoc :refer :all]))

;a-z 97-122
;A-Z 65-90

(defn char+ [c i] (-> (int c) (+ i) char))

(defn q1 [l]
  (loop [l l]
    (let [nl (loop [nl []
                    l l]
               (if (empty? l)
                 nl
                 (let [[a b] l]
                   (cond
                     (= b nil) (conj nl a)
                     (or (= a (char+ b 32))
                         (= (char+ a 32) b)
                         (= a (char+ b -32))
                         (= (char+ a -32) b)) (recur nl (drop 2 l))
                     :else (recur (conj nl a) (rest l))))))]
      (if (< (count nl) (count l))
        (recur nl)
        l))))

(defn q2 [l]
  (->> (mapv (fn [ic]
               (filterv #(and (not= (char ic) %)
                              (not= (char (+ ic 32)) %)) l))
             (range 65 91))
       (reduce (fn [mn sl]
                 (min mn (count (q1 sl))))
               (count l))))

#_(let [s (q1 (get-res))]
    (println "q1:" (count s))
    (println "q2:" (q2 s)))

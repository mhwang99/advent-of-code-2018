(ns aoc18.core
  (:require [clojure.string :as s]))

(defn get-res []
  (->> (s/split (str *ns*) #"\.")
       second
       (#(str "resources/" % ".txt"))
       slurp drop-last (apply str)))

(defn get-split
  ([]
   (get-split (get-res)))
  ([s]
   (s/split s #",|\n")))

(defn get-num
  ([]
   (get-num (get-res)))
  ([s]
   (->> s
        (re-seq #"\d+")
        (mapv #(Integer. %)))))

(defn get-line-num
  ([]
   (get-line-num (get-res)))
  ([s]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (->> (re-seq #"\d+" l)
                     (mapv #(Integer. %))))))))


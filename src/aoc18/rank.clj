(ns aoc18.rank
  (:require [clojure.string :as s]
            [aoc18.core :refer :all]
            [cheshire.core :as json]
            [camel-snake-kebab.core :as camel]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [com.hypirion.clj-xchart :as c])
  (:import [java.util Date]))

;view-source:https://adventofcode.com/2018/leaderboard/private/view/110816.json
(def default-lbooard 110816)

(defn get-file
  ([]
   (get-file default-lbooard))
  ([lboard]
   (->> (str "resources/" lboard ".json")
        slurp json/parse-string
        (transform-keys camel/->kebab-case-keyword))))

(defn get-users
  ([]
   (get-users default-lbooard))
  ([lboard]
   (->> (get-file lboard)
        :members
        (mapv #(-> % second :name)))))

(defn convert-info
  ([]
   (convert-info default-lbooard))
  ([lboard]
   (->> (get-file lboard)
        :members
        (reduce
          (fn [ret [_ {user :name
                       cdl :completion-day-level}]]
            (reduce
              (fn [ret [day qs]]
                (reduce
                  (fn [ret [qno {:keys [get-star-ts]}]]
                    (conj ret [(* (atoi get-star-ts) 1000)
                               user
                               (str (name day) "-" (name qno))]))
                  ret qs))
              ret cdl))
          [])
        (sort-by first))))

(defn time-spliced-score
  ([]
   (time-spliced-score default-lbooard))
  ([lboard]
   (time-spliced-score lboard 4))
  ([lboard hours]
   (let [time-gap (* 60 60 hours 1000)
         users (get-users lboard)
         user-count (count users)
         dec-1st-00 1543640400000]
     (loop [ret []
            l (convert-info lboard)
            t (+ dec-1st-00 time-gap)
            r? false
            q-score (reduce (fn [m d]
                              (assoc m
                                     (str d "-1") user-count
                                     (str d "-2") user-count))
                            {} (range 1 26))
            u-score (into {} (mapv #(vector % 0) users))]
       (if (empty? l)
         ret
         (let [[timestamp user qid] (first l)]
           (if (> timestamp t)
             (let [ret (cond-> ret
                         r?  (conj [t u-score]))]
               (recur ret l (+ t time-gap) false q-score u-score))
             (recur ret (rest l) t true
                    (update q-score qid dec)
                    (update u-score user #(+ (or % 0) (get q-score qid)))))))))))

(defn print-times
  ([]
   (show-times default-lbooard))
  ([lboard]
   (doseq [[timestamp user qid] (convert-info lboard)]
     (println (format "%s , %4s , %s" (Date. timestamp) qid user)))))

(defn show-time-score
  ([]
   (show-time-score default-lbooard))
  ([lboard]
   (let [stats (time-spliced-score lboard)
         users (sort (->> stats first second keys))
         times (mapv (fn [[timestamp]]
                      (java.util.Date. timestamp)) stats)
         chart-data (reduce (fn [ret user]
                              (conj ret [user
                                         {:x times
                                          :y (for [[_ u-score] stats]
                                               (get u-score user))}]))
                            [] users)]
     (c/view
       (c/xy-chart
         chart-data
         {:date-pattern "dd hh:mm"})))))

(defn show-time-score-percent
  ([]
   (show-time-score-percent default-lbooard))
  ([lboard]
   (let [stats (time-spliced-score lboard)
         users (sort (->> stats first second keys))
         times (mapv (fn [[timestamp]]
                      (java.util.Date. timestamp)) stats)
         top-scores (->> stats
                         (mapv (fn [[tm scores]]
                                 [tm (-> scores vals sort last)]))
                         (into {}))
         chart-data (reduce (fn [ret user]
                              (conj ret [user
                                         {:x times
                                          :y (for [[tm u-score] stats]
                                               (-> (get u-score user)
                                                   (/ (get top-scores tm))
                                                   (* 100)))}]))
                            [] users)]
     (c/view
       (c/xy-chart
         chart-data
         {:date-pattern "dd hh:mm"})))))

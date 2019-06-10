(ns cluster-tools
  (:require [com.climate.claypoole :as cp]
            [taoensso.timbre :as log]
            [uncomplicate.neanderthal.core :as thal]
            [uncomplicate.neanderthal.native :as thal-native]
            [uncomplicate.neanderthal.real :as thal-real]
            [uncomplicate.commons.core :as uncomplicate]))

(defn update-cluster
  [clusters cluster sample merge-fn]
  (let [clusters (disj clusters cluster)
        cluster (merge-fn cluster sample)]
    (conj clusters cluster)))

(defn conj-cluster
  [clusters sample merge-fn]
  (conj clusters (merge-fn nil sample)))

(defn nearest-sample-cluster-pair
  [samples clusters {:keys [cluster-thresh factory matrix-fn]}]
  (when (and (seq samples) (seq clusters))
    (uncomplicate/with-release [s1-1 (matrix-fn factory clusters samples)
                                s1 (thal/trans s1-1)
                                s2 (matrix-fn factory clusters)]
      #_(println (thal/mrows s1) (thal/ncols s1) (thal/mrows s2) (thal/ncols s2))
      (uncomplicate/with-release [score-m (thal/mm s1 s2)]
        (reduce
          (fn [best [i sample]]
            (reduce
              (fn [{:keys [score] :as best} [j cluster]]
                (let [new-score (thal-real/entry score-m i j)]
                  (if (< score new-score)
                    {:cluster cluster :sample sample :score score}
                    best)))
              best
              (map-indexed vector clusters)))
          {:score cluster-thresh}
          (map-indexed vector samples))))))

(defn single-pass-cluster
  "Occurs in O(N^2*M) time"
  [samples clusters {:keys [cluster-merge-fn] :as params}]
  (loop [samples samples
         clusters clusters]
    (let [best (nearest-sample-cluster-pair samples clusters params)]
      (cond (:cluster best) (recur (disj samples (:sample best)) (update-cluster clusters (:cluster best) (:sample best) cluster-merge-fn))
            (seq samples) (recur (set (rest samples)) (conj-cluster clusters (first samples) cluster-merge-fn))
            :else clusters))))
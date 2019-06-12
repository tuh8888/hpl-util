(ns cluster-tools
  (:require [linear-algebra :as linear-algebra]))

(defn update-cluster
  [clusters cluster sample merge-fn]
  (let [clusters (disj clusters cluster)
        cluster (merge-fn cluster sample)]
    (conj clusters cluster)))

(defn conj-cluster
  [clusters sample merge-fn]
  (conj clusters (merge-fn nil sample)))

(defn nearest-sample-cluster-pair
  [samples clusters {:keys [cluster-thresh] :as params}]
  (when (and (seq samples) (seq clusters))
    (let [best (linear-algebra/find-best-match params samples clusters)]
      (when (< cluster-thresh (:score best))
        best))))

(defn single-pass-cluster
  "Occurs in O(N^2*M) time"
  [{:keys [cluster-merge-fn] :as params} samples clusters]
  (loop [samples samples
         clusters clusters]
    (let [{:keys [sample match]} (nearest-sample-cluster-pair samples clusters params)]
      (cond match (let [samples (disj samples sample)
                        clusters (update-cluster clusters match sample cluster-merge-fn)]
                    (recur samples clusters))
            (seq samples) (let [sample (first samples)
                                samples (disj samples sample)
                                clusters (conj-cluster clusters sample cluster-merge-fn)]
                            (recur samples clusters))
            :else clusters))))
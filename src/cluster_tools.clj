(ns cluster-tools)

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
    (->> clusters
         (math/find-best-match params samples)
         (filter (fn [{:keys [score]}] (< cluster-thresh score))))))

(defn single-pass-cluster
  "Occurs in O(N^2*M) time"
  [samples clusters {:keys [cluster-merge-fn] :as params}]
  (loop [samples samples
         clusters clusters]
    (let [best (nearest-sample-cluster-pair samples clusters params)]
      (cond (:cluster best) (recur (disj samples (:sample best)) (update-cluster clusters (:cluster best) (:sample best) cluster-merge-fn))
            (seq samples) (recur (set (rest samples)) (conj-cluster clusters (first samples) cluster-merge-fn))
            :else clusters))))
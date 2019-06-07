(ns cluster-tools)

(defn nearest-cluster
  [sample clusters best {:keys [cluster-match-fn]}]
  (first
    (reduce
      (fn [{:keys [cluster score] :as best}  new-cluster]
        (if-let [score (cluster-match-fn cluster sample score)]
          {:score score :cluster new-cluster}
          best))
      best clusters)))

(defn update-cluster
  [clusters cluster sample merge-fn]
  (let [clusters (disj clusters cluster)
        cluster (merge-fn cluster sample)]
    (conj clusters cluster)))

(defn conj-cluster
  [clusters sample merge-fn]
  (conj clusters (merge-fn nil sample)))

(defn nearest-sample-cluster-pair
  [samples clusters params]
  (reduce
    (fn [best sample]
      (nearest-cluster sample clusters best params))
    {} samples))

(defn single-pass-cluster
  "Occurs in O(N^2*M) time"
  [samples clusters {:keys [cluster-merge-fn] :as params}]
  (loop [clusters clusters
         samples samples]
    (if-let [best (nearest-sample-cluster-pair samples clusters params)]
      (recur (disj samples (:sample best)) (update-cluster clusters (:cluster best) (:sample best) cluster-merge-fn))
      (recur (disj samples (first samples)) (conj-cluster clusters (first samples) cluster-merge-fn)))))
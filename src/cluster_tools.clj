(ns cluster-tools
  (:require [linear-algebra :as linear-algebra]))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn update-cluster
  [clusters cluster pos sample merge-fn]
  (let [clusters (vec-remove clusters pos)
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
  [{:keys [cluster-merge-fn vector-fn] :as params} samples clusters]
  (let [clusters (vec clusters)
        samples (vec samples)]

    (loop [samples (vec samples)
           sample-vectors (->> samples
                               (map vector-fn)
                               (map #(linear-algebra/unit-vec params %))
                               (vec))
           clusters (vec clusters)]
      (let [cluster-vectors (->> clusters
                                 (map vector-fn)
                                 (map #(linear-algebra/unit-vec params %)))
            {:keys [i j]} (nearest-sample-cluster-pair sample-vectors cluster-vectors params)]

        (let [sample (get samples i)
              cluster (get clusters j)]
          (cond cluster (let [samples (vec-remove samples i)
                              sample-vectors (vec-remove sample-vectors i)
                              clusters (update-cluster clusters cluster j sample cluster-merge-fn)]
                          (recur samples sample-vectors clusters))
                (seq samples) (let [sample (first samples)
                                    samples (vec-remove samples 0)
                                    sample-vectors (vec-remove sample-vectors 0)
                                    clusters (conj-cluster clusters sample cluster-merge-fn)]
                                (recur samples sample-vectors clusters))
                :else clusters))))))
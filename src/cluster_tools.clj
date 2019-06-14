(ns cluster-tools
  (:require [linear-algebra :as linear-algebra]
            [taoensso.timbre :as log]))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn update-cluster
  [{:keys [cluster-merge-fn] :as params} clusters cluster pos sample]
  (let [clusters (vec-remove clusters pos)
        cluster  (cluster-merge-fn params cluster sample)]
    (conj clusters cluster)))

(defn conj-cluster
  [{:keys [cluster-merge-fn] :as params} clusters sample]
  (conj clusters (cluster-merge-fn params nil sample)))

(defn nearest-sample-cluster-pair
  [{:keys [cluster-thresh factory]} clusters samples]
  (when (and (seq samples) (seq clusters))
    (let [best (linear-algebra/find-best-match factory samples clusters)]
      (when (< cluster-thresh (:score best))
        best))))

(defn single-pass-cluster
  "Occurs in O(N^2*M) time"
  [{:keys [vector-fn factory] :as params} samples clusters]
  (let [clusters (vec clusters)
        samples  (vec samples)]
    (loop [samples        (vec samples)
           sample-vectors (->> samples
                               (map vector-fn)
                               #_(pmap #(linear-algebra/unit-vec factory %))
                               (vec))
           clusters       (vec clusters)]
      (let [cluster-vectors (->> clusters
                                 (map vector-fn)
                                 #_(pmap #(linear-algebra/unit-vec factory %)))
            {:keys [i j]} (nearest-sample-cluster-pair params cluster-vectors sample-vectors)]
        (let [sample  (get samples i)
              cluster (get clusters j)]
          (cond cluster (let [samples        (vec-remove samples i)
                              sample-vectors (vec-remove sample-vectors i)
                              clusters       (update-cluster params clusters cluster j sample)]
                          (recur samples sample-vectors clusters))
                (seq samples) (let [sample         (first samples)
                                    samples        (vec-remove samples 0)
                                    sample-vectors (vec-remove sample-vectors 0)
                                    clusters       (conj-cluster params clusters sample)]
                                (recur samples sample-vectors clusters))
                :else clusters))))))
(ns coll)

(defn remove-nth
  [v n]
  (-> v
      (subvec 0 n)
      (concat (subvec v (inc n)))
      (vec)))

(defn toggle-contains-set
  [coll x]
  (if (contains? coll x)
    (disj coll x)
    (conj (or coll #{}) x)))

(defn toggle-contains-vector
  [coll x]
  (if (some #(= x %) coll)
    (->> coll
         (remove #(= x %))
         (vec))
    (conj (or coll []) x)))

(ns util)

(defn map-kv
  [f m]
  (zipmap (keys m)
          (map f (vals m))))

(defn parse-int
  [x]
  (try (Integer/parseInt x)
       (catch NumberFormatException _
         x)))

(defn find-matches
  [coll1 coll2 match-fn]
  (filter
    (fn [s1]
      (some
        (fn [s2]
          (match-fn s1 s2))
        coll2))
    coll1))
(ns util)

(defn map-kv
  [f m]
  (zipmap (keys m)
          (map f (vals m))))

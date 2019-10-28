(ns graph)

(defn undirected-graph
  [g]
  (apply ubergraph.core/multigraph
         (map #(vector (loom.graph/src %)
                       (loom.graph/dest %))
              (loom.graph/edges g))))
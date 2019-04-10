(defn read-deps-edn [aliases-to-include]
  (let [{:keys [paths deps aliases]} (-> "deps.edn" slurp clojure.edn/read-string)
        deps (->> (select-keys aliases aliases-to-include)
                  vals
                  (mapcat :extra-deps)
                  (into deps)
                  (reduce
                    (fn [deps [artifact info]]
                      (if-let [version (:mvn/version info)]
                        (conj deps
                              (transduce cat conj [artifact version]
                                         (select-keys info [:scope :exclusions])))
                        deps))
                    []))]
    {:dependencies   deps
     :source-paths   (set paths)
     :resource-paths (set paths)}))

(let [{:keys [source-paths resource-paths dependencies]} (read-deps-edn [])]
  (set-env!
    :source-paths source-paths
    :resource-paths resource-paths
    :dependencies dependencies #_(into '[[adzerk/boot-cljs "2.1.4" :scope "test"]
                                         [adzerk/boot-reload "0.5.2" :scope "test"]]
                                       dependencies)))

(require
  ;'[adzerk.boot-cljs :refer [cljs]]
  ;'[adzerk.boot-reload :refer [reload]]
  '[core :refer [-main]])

(task-options!
  pom {:project 'hpl/util
       :version "0.0.0-SNAPSHOT"})

(deftask run []
         (comp
           (-main)
           ;(with-pass-thru _ (-main))
           ;(watch)
           ;(reload :asset-path "public")
           ;(cljs :optimizations :none)
           ;(target)
           ))

(deftask build []
         (comp
           ;(cljs :optimizations :advanced)
           (aot) (pom) (uber) (jar) (target)))
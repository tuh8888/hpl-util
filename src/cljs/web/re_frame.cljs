(ns web.re-frame
  (:require [re-frame.core :refer [subscribe dispatch
                                   ->interceptor
                                   get-coeffect assoc-coeffect]]
            [coll :refer [remove-nth]]))

(def <sub (comp deref subscribe))
(def >evt dispatch)

(defn path-nth
  ([]
   (path-nth 0))
  ([i]
   (let [db-store-key         :re-frame-path/db-store
         first-path-store-key (keyword *ns* (str (name ::first-path-store)
                                                 (random-uuid)))]
     (->interceptor
       :id :viz-id-path
       :before (fn [context]
                 (let [original-db (get-coeffect context :db)
                       viz-id      (get-in context [:coeffects :event i])
                       new-db      (get original-db viz-id)]
                   (-> context
                       (update-in [:coeffects :event] remove-nth i)
                       (update db-store-key conj original-db)
                       (assoc first-path-store-key viz-id)
                       (assoc-coeffect :db new-db))))))))

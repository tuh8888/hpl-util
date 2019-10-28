(ns web.browser)

(defn win-inner-w
  []
  js/window.innerWidth)

(defn win-inner-h
  []
  js/window.innerHeight)

(defn get-element-by-id
  [id]
  (js/document.getElementById id))

(defn unify-scroll
  "Unifies the scrolling of two elements"
  [master-id follower-id]
  (let [follower-elem (get-element-by-id follower-id)
        master-elem   (get-element-by-id master-id)]
    (set! (.-scrollTop follower-elem) (.-scrollTop master-elem))))

(defn key?
  [e key]
  (= (.-charCode e) (key key-map)))

(defn target-value
  "Returns value found in text field"
  [d]
  (-> d (.-target) (.-value)))

(defn selection-start
  "Returns selection start."
  [d]
  (-> d (.-target) (.-selectionStart)))

(defn selection-end
  "Returns selection start."
  [d]
  (-> d (.-target) (.-selectionEnd)))

(defn set-ons
  "Utility function for adding a bunch of ons."
  [node ons]
  (reduce (fn [node [on on-fn]]
            (.on node (name on) on-fn))
          node ons))

(defn len-text
  "Calculates the length of a span of text."
  [text]
  (let [ruler (get-element-by-id "ruler")]
    (set! (.-innerHTML ruler) text)
    (.-offsetWidth ruler)))
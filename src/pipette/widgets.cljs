(ns pipette.widgets
  (:require [reagent.core :as reagent]
            [reagent.dom :as rdom]))

(defn- default-validate [s x]
  (when (nil? x)
    (str s " is not a valid input")))

(def fmt
  (reagent/create-class
   {:display-name "fmt-input"

    :component-did-update
    (fn [this old-argv]
      (let [{:keys [value print read validate]}
            (-> (reagent/argv this)
                (second))
            element (rdom/dom-node this)]
        (when-not (and
                   (= js/document.activeElement element)
                   ;; this may be a little slow, but it's easy
                   (= (read (.-value element)) value))
          (set! (.-value element) (print value)))))

    :reagent-render
    (fn [{:keys [value print read on-change validate]
          :or {validate default-validate}
          :as atts}]
      [:input
       (-> atts
           (dissoc :value :print :read :validate)
           (assoc :type :text
                  :default-value (print value)
                  :on-change
                  (and on-change
                       (fn [e]
                         (let [s (-> e .-target .-value)
                               v (read s)
                               err (validate s v)]
                           (when (and on-change (not err))
                             (on-change v))
                           )))
                  :on-blur
                  (fn [e]
                    (set!
                     (-> e .-target .-value)
                     (print value))
                    )))
       ])}))


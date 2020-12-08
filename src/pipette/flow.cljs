(ns pipette.flow
  "A bit like re-frame, except you don't have to register everything as
  a name in a global database, so you can have more than one at a time."
  (:require [goog.async.nextTick]
            [reagent.ratom :refer [IDisposable]]
            [reagent.core]
            [clojure.walk :as walk]))

(defprotocol IView
  (view
    [t f]
    [t f x]
    [t f x y]
    [t f x y z]
    "Return a view into this under f, which is a reagent reaction (i.e. you deref it.)
The result is IView so can itself be view-ed. Since the view is of (f this ...), the view
can itself depend on other views by asking for them")
  
  (view*
    [t f]
    [t f x]
    [t f x y]
    [t f x y z]
    "Like view, except the function is called with @this, so cannot ask for more views")

  (subscribe [t v]
    "Like view or view*, except the arguments are determined from the meta on v, and the result is dereffed"))

(defprotocol IHandler
  "A reference type for doing a reactive type web thing"
  (fire! [this event])
  (fire!! [this event])
  (flush! [this]))

(declare cache-view!)

(deftype View [name reaction ^:mutable views]
  IView
  (view  [t f]          (cache-view! t false f))
  (view  [t f x]        (cache-view! t false f x))
  (view  [t f x y]      (cache-view! t false f x y))
  (view  [t f x y z]    (cache-view! t false f x y z))

  (view*  [t f]         (cache-view! t true f))
  (view*  [t f x]       (cache-view! t true f x))
  (view*  [t f x y]     (cache-view! t true f x y))
  (view*  [t f x y z]   (cache-view! t true f x y z))

  (subscribe [t f] @(cache-view! t :sub f))
  
  IDeref
  (-deref [_] (deref reaction))
  
  IWatchable
  (-add-watch [this key cb] (add-watch reaction key cb) this)
  (-remove-watch [this key] (remove-watch reaction key) this)

  IDisposable
  (reagent.ratom/dispose! [this] (reagent.ratom/dispose! reaction))
  (reagent.ratom/add-on-dispose! [this f] (reagent.ratom/add-on-dispose! reaction f))

  Object
  (toString [this] (str "View on: " name)))

(defn- cache-view! [view deref? f & args]
  (let [key [deref f args]
        value (get (.-views view) key)]
    (if value
      value

      (let [reaction
            (let [[x y z] args]
              (case (count args)
                0
                (case deref?
                  true #(f (deref view))
                  false #(f view)
                  :sub
                  (let [m (meta f)]
                    (if (contains? m ::args)
                      (let [m  (::args m)
                            a  (map
                                #(if (or (seq? %) (vector? %))
                                   (apply cache-view! view :sub %)
                                   (cache-view! view :sub %))
                                m)]
                        (case (count a)
                          0 f
                          1 (let [[a] a] #(f @a))
                          2 (let [[a b] a] #(f @a @b))
                          3 (let [[a b c] a] #(f @a @b @c))
                          4 (let [[a b c d] a] #(f @a @b @c @d))
                          5 (let [[a b c d e] a] #(f @a @b @c @d @e))
                          #(apply f (map deref a))))
                      
                      (cache-view! view true f))))
                
                1
                (case deref?
                  (:sub true) #(f (deref view) x)
                  false #(f view x))
                2
                (case deref?
                  ("sub" true) #(f (deref view) x y)
                  false #(f view x y))
                3
                (case deref?
                  (:sub true) #(f (deref view) x y z)
                  false #(f view x y z))
                
                (case deref?
                  (:sub true) #(apply f (deref view) args)
                  false #(apply f view args))))
            
            reaction (if (fn? reaction)
                       (reagent.ratom/make-reaction reaction)
                       reaction)
            subview (View.
                     (str f args)
                     reaction {})
            ]
        
        (reagent.ratom/add-on-dispose!
         reaction
         (fn [_]
           (set! (.-views view)
                 (dissoc (.-views view) key))))
        
        (set! (.-views view) (assoc (.-views view) key subview))
        subview)
      )))

(deftype Root [state effects handler v ^:mutable events]
  IView
  (view   [t f]          (view v f))
  (view   [t f x]        (view v f x))
  (view   [t f x y]      (view v f x y))
  (view   [t f x y z]    (view v f x y z))
  
  (view*  [t f]          (view* v f))
  (view*  [t f x]        (view* v f x))
  (view*  [t f x y]      (view* v f x y))
  (view*  [t f x y z]    (view* v f x y z))

  (subscribe [t f]       (subscribe v f))

  IDeref
  (-deref [_] (deref state))

  IWatchable
  (-add-watch [this key cb] (add-watch state key cb) this)
  (-remove-watch [this key] (remove-watch state key) this)
  
  IHandler
  (fire! [t event]
    (when (empty? events)
      (goog.async.nextTick #(flush! t)))
    (set! events (conj events event)))

  (fire!! [t event]
    (set! events (conj events event))
    (flush! t))
  
  (flush! [t]
    (let [to-run events]
      (set! events #queue [])
      (let [[new-state fx]
            (loop [state @state
                   to-run to-run
                   fx []]
              (if (empty? to-run)
                [state fx]
                (let [out (handler state (first to-run))]
                  (if (contains? out ::effects)
                    (recur
                     (::state out state)
                     (rest to-run)
                     (conj fx (::effects out)))
                    
                    (recur out (rest to-run) fx))
                  )))
            ]
        ;; trigger side-effects. we group them by keys, in case some
        ;; of them are debouncable. this is up to the effect handling
        ;; function

        (let [groups
              (apply merge-with conj
                     (zipmap (mapcat keys fx)
                             (repeat []))
                     fx)]
          (doseq [[e es] groups]
            (if-let [ef (get effects e)]
              (ef t es)
              (println "Unhandled effect" e))))
        
        ;; update state (since we have no contention issues in js we
        ;; don't use swap!)
        (reset! state new-state)))))

(defn apply-handler [state [f & args]]
  (apply f state args))

(defn create-root [{:keys [state handler effects]
                    :or {state (reagent.core/atom nil)
                         effects {}
                         handler apply-handler
                         }}]
  (Root.
   state
   effects
   handler
   (View. "ROOT" state {})
   #queue []))


(defn- event? [e]
  (boolean
   (or (instance? js/Event e)
       (and (instance? js/Object e)
            (.-nativeEvent e)))))

(deftype Callback [root func e]
  IFn
  (-invoke [this]
    (func
     root
     (walk/prewalk-replace {:% nil} e)))
  
  (-invoke [this el]
    (func
     root
     (walk/prewalk-replace
      {:% (if (event? el)
            (-> el .-target .-value)
            el)}
      e)))

  IEquiv
  (-equiv [x y]
    (and (instance? Callback y)
         (= e (.-e y))
         (= func (.-func y))
         (= root (.-root y)))))

(defn callback [root event & {:keys [immediate]}]
  (Callback. root
             (if immediate fire!! fire!)
             event))

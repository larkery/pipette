(ns pipette.flow)

(defmacro defsub [name args & body]
  (let [args (partition-all 2 args)]
    `(def ~name
       ^{::args ~(mapv second args)}
       (fn ~(mapv first args)
         ~@body))))

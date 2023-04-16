(ns utils)

(defmacro def-locals []
  `(do ~@(for [sym (keys &env)]
           `(def ~sym ~sym))))

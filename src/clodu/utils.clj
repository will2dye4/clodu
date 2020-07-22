(ns clodu.utils
  (:import (java.io Writer)))

(def enumerate (partial map-indexed vector))

(def sum (partial reduce +))

(defn map-vals
  ([f m]
    (map-vals f {} m))
  ([f init m]
    (reduce-kv #(assoc %1 %2 (f %3)) init m)))

(defn make-printable
  ([type] (make-printable type symbol))
  ([type coerce]
    (defmethod print-method type [object ^Writer writer]
      (print-method (coerce (str object)) writer))
    (defmethod print-dup type [object ^Writer writer]
      (print-ctor object (fn [o w] (print-dup (vals o) w)) writer))
    (defmethod clojure.pprint/simple-dispatch type [object]
      (.write ^Writer *out* (str object)))))

(defn value-comparator [m]
  (fn [key1 key2]
    (compare (get m key1) (get m key2))))

(defn sorted-map-by-value [m]
  (into (sorted-map-by (value-comparator m)) m))

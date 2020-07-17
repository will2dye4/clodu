(ns clodu.core
  (:require [clojure.pprint]
            [clojure.string :as str]
            [clodu.cards :refer [->Card joker new-deck ranks suits]])
  (:gen-class))

(def deck (new-deck))

(def shuffled-deck (shuffle deck))

(defn c [rank-suit]
  {:pre [(string? rank-suit) (or (.equalsIgnoreCase "joker" rank-suit) (< 1 (count rank-suit) 4))]}
  (if (.equalsIgnoreCase "joker" rank-suit)
    joker
    (let [[r s] (if (= 2 (count rank-suit))
                  (str/split (str/upper-case rank-suit) #"")
                  [(.substring rank-suit 0 2) (.substring rank-suit 2)])
          rank (->> ranks
                    vals
                    (filter #(= r (:symbol %)))
                    first)
          suit (suits ({"C" :clubs "D" :diamonds "H" :hearts "S" :spades} s))]
      (when (and rank suit)
        (->Card rank suit)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

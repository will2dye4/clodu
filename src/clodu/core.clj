(ns clodu.core
  (:require [clojure.pprint]
            [clojure.string :as str]
            [clodu.cards :refer [->Card joker new-deck ranks suits]]
            [clodu.cli :refer [new-cli-game]]
            [clodu.game :refer [play]])
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

(defn cs [& rank-suits] (map c rank-suits))

(defn -main
  "Play an interactive game of shanghai at the command line."
  [& args]
  (play (new-cli-game)))

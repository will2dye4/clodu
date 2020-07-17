(ns clodu.game
  (:require [clodu.constants :refer [max-players min-players]]
            [clodu.utils :refer [map-vals]]))

;; Dye family house rules
(def ^:const default-rules
  {:allow-any-number-of-jokers? false   ;; must have >= 1/2 natural cards
   :allow-buying? true                  ;; can 'buy' cards out of turn
   :allow-calling-shanghai? false       ;; if true, allows preventing others from drawing your discard
   :allow-initial-wild-upcard? false    ;; reshuffle deck if initial upcard is wild
   :allow-replacing-jokers? true        ;; substitute naturals for jokers once you are down
   :buying-penalty 1                    ;; number of penalty cards to draw when buying
   :max-buys-per-hand 3                 ;; other variations use allow 4 buys for hands 9-10
   :require-down-and-out :last-hand     ;; or :always, or :never
   :wild-cards #{:2 :joker}})           ;; ranks to consider 'wild'

;; Dye family house rules
(def ^:const default-min-cards {:sets 3 :runs 4})

(defn counts->contracts [counts]
  (mapv #(reduce-kv (fn [m k v] (assoc m k [v (default-min-cards k)])) {} %) counts))

;; standard version of the Dye family house rules
(def ^:const default-contracts
  (counts->contracts [{:sets 2}
                      {:sets 1 :runs 1}
                      {:runs 2}
                      {:sets 3}
                      {:sets 2 :runs 1}
                      {:sets 1 :runs 2}
                      {:runs 3}
                      {:sets 2 :runs 2}]))

;; 'easy' version of the Dye family house rules
(def ^:const easy-mode-contracts
  (counts->contracts [{:sets 1}
                      {:runs 1}
                      {:sets 2}
                      {:sets 1 :runs 1}
                      {:runs 2}
                      {:sets 3}
                      {:sets 2 :runs 1}
                      {:sets 2 :runs 2}]))

(defn num-decks [num-players]
  {:pre [(<= min-players num-players max-players)]}
  (int (Math/ceil (/ num-players 2))))

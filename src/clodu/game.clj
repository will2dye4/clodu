(ns clodu.game
  (:require [clodu.constants :refer [max-players min-players]]
            [clodu.utils :refer [map-vals]]))

;; Dye family house rules
(def ^:const default-rules
  {:allow-any-number-of-wild-cards? false   ;; must have >= 1/2 natural cards
   :allow-buying? true                      ;; can 'buy' cards out of turn
   :allow-calling-shanghai? false           ;; if true, allows preventing others from drawing your discard
   :allow-initial-wild-upcard? false        ;; reshuffle deck if initial upcard is wild
   :allow-replacing-jokers? true            ;; substitute naturals for jokers once you are down
   :buying-penalty 1                        ;; number of penalty cards to draw when buying
   :max-buys-per-hand 3                     ;; other variations use allow 4 buys for hands 9-10
   :require-down-and-out :last-hand         ;; or :always, or :never
   :wild-cards #{:2 :joker}})               ;; ranks to consider 'wild'

(defn num-decks [num-players]
  {:pre [(<= min-players num-players max-players)]}
  (int (Math/ceil (/ num-players 2))))

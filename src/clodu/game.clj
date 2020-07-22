(ns clodu.game
  (:require [clodu.cards :refer [add draw-hands new-decks]]
            [clodu.constants :refer [max-players min-players]]
            [clodu.contract :refer [default-contracts]]
            [clodu.player :refer [new-player]]
            [clodu.utils :refer [map-vals sum]]))

(def ^:const default-wild-cards #{:2 :joker})

;; Dye family house rules
(def ^:private default-ranks->points {#{:3 :4 :5 :6 :7 :8 :9} 5
                                      #{:10 :jack :queen :king} 10
                                      #{:ace} 15
                                      default-wild-cards 25})

(def ->point-values (partial reduce-kv #(apply assoc %1 (interleave %2 (repeat %3))) {}))

(def ^:const default-point-values (->point-values default-ranks->points))

;; Dye family house rules
(def ^:const default-rules
  {:allow-buying? true                      ;; can 'buy' cards out of turn
   :allow-calling-shanghai? false           ;; if true, allows preventing others from drawing your discard
   :allow-initial-wild-upcard? false        ;; reshuffle deck if initial upcard is wild
   :allow-replacing-jokers? true            ;; substitute naturals for jokers once you are down
   :buying-penalty 1                        ;; number of penalty cards to draw when buying
   :cards-per-hand 11                       ;; number of cards dealt to each player for each hand
   :max-buys-per-hand 3                     ;; other variations use allow 4 buys for hands 9-10
   :point-values default-point-values       ;; point values to use for scoring
   :require-down-and-out :last-hand         ;; or :always, or :never
   :wild-cards default-wild-cards})         ;; ranks to consider 'wild'

(defn num-decks [num-players]
  {:pre [(<= min-players num-players max-players)]}
  (int (Math/ceil (/ num-players 2))))

(defrecord Hand [contract discards])

(defn new-hand [contract] (Hand. contract []))

(defrecord Game [players rules contracts deck hands action-fn])

(def default-num-players 4)

(defn default-action-fn [state] nil)  ;; TODO

(defn ->player-map [players]
  (->> players
       (map #(vector (:id %) %))
       (into (sorted-map))))

(defn new-game [& {:keys [players rules contracts num-players action-fn]
                   :or {players nil, rules default-rules, contracts default-contracts,
                        num-players default-num-players, action-fn default-action-fn}}]
  {:pre [(let [n (if players (count players) num-players)] (> n 1))]}
  (let [players (->player-map (or players (map new-player (map inc (range num-players)))))
        deck (shuffle (new-decks (num-decks (count players))))]
    (Game. players rules contracts deck [] action-fn)))

(defn current-contract [game] (get-in game [:current-hand :contract]))

(defn players [game] (vals (:players game)))

(defn update-player [game player f & args]
  (apply update-in game [:players (:id player)] f args))

(defn update-players [game players]
  (assoc game :players (if (sequential? players)
                         (->player-map players)
                         players)))

(defn event [event-type & attrs]
  (apply conj [event-type] attrs))

(defn register-event-handler [game handler]
  (update game :handlers conj handler))

(defn emit [game event]
  (when-let [handlers (:handlers game)]
    (doseq [handler handlers] (handler event)))
  game)

(defn deal-hand [game contract]
  (let [[hands deck] (draw-hands (count (players game)) (get-in game [:rules :cards-per-hand]) (shuffle (:deck game)))
        players (map #(assoc %1 :hand %2) (players game) hands)]
    (-> game
        (assoc :current-hand (new-hand contract))
        (assoc :deck deck)
        (update-players players))))

(defn round-of-play [game] game)  ;; TODO

(defn discard [game card])  ;; TODO

(defn index-relative-to-dealer [relative-index game]
  ;; use (count (:hands game)) as a counter that increments after each hand
  (mod (+ (count (:hands game)) relative-index) (count (players game))))

(defn player-id-at-index-relative-to-dealer [relative-index game]
  (:id (nth (players game) (index-relative-to-dealer relative-index game))))

(def dealer-id (partial player-id-at-index-relative-to-dealer 0))

(defn is-dealer? [game player] (= (:id player) (dealer-id game)))

(defn available-actions [game player])  ;; TODO

(defn action-fn-state [game player]
  {:player player
   :contract (current-contract game)
   :allowed-actions (available-actions game player)})

(defn score-hand
  ([hand] (score-hand hand default-point-values))
  ([hand point-values] (sum (map (comp point-values :key :rank) hand))))

(defn tally-scores [game]
  (let [point-values (get-in game [:rules :point-values])
        update-score (fn [player] (update player :score + (score-hand (:hand player) point-values)))]
    (update-players game (map update-score (players game)))))

;; TODO - add player melds
(defn recycle-cards [game]
  (let [{:keys [current-hand]} game
        players (players game)
        hands (->> players
                   (map :hand)
                   (remove nil?)
                   flatten)
        cards (concat hands (:discards current-hand))]
    (-> game
        (update :deck add cards)
        (update-players (map #(dissoc %1 :hand) players)))))

(defn conclude-hand [game]
  (-> game
      tally-scores
      recycle-cards
      (#(update % :hands conj (:current-hand %)))
      (dissoc :current-hand)
      (#(emit % (event :end-hand %)))))

(defn play-hand [game contract]
  (-> game
      (deal-hand contract)
      round-of-play
      conclude-hand))

(defn play [game]
  (loop [game game
         contracts (:contracts game)]
    (if-let [contract (first contracts)]
      (recur (play-hand game contract) (rest contracts))
      game)))

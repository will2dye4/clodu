(ns clodu.game
  (:require [clodu.cards :refer [add draw draw-hands new-decks sort-cards]]
            [clodu.constants :refer [max-players min-players]]
            [clodu.contract :refer [default-contracts is-run? is-set?]]
            [clodu.player :refer [new-player]]
            [clodu.utils :refer [enumerate map-vals rotate sum]]))

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

(defn new-hand [contract] (Hand. contract '()))

(defrecord Game [players rules contracts deck hands draw-choice-fn action-fn])

(def default-num-players 4)

(defn default-draw-choice-fn [player upcard] :deck)  ;; TODO

(defn default-action-fn [state] [{:action :discard, :card (:new-card state)}])  ;; TODO

(defn ->player-map [players]
  (->> players
       (map #(vector (:id %) %))
       (into (sorted-map))))

(defn new-game [& {:keys [players rules contracts num-players draw-choice-fn action-fn]
                   :or {players nil, rules default-rules, contracts default-contracts,
                        num-players default-num-players, draw-choice-fn default-draw-choice-fn,
                        action-fn default-action-fn}}]
  {:pre [(let [n (if players (count players) num-players)] (> n 1))]}
  (let [players (->player-map (or players (map new-player (map inc (range num-players)))))
        deck (shuffle (new-decks (num-decks (count players))))]
    (Game. players rules contracts deck [] draw-choice-fn action-fn)))

(defn current-contract [game] (get-in game [:current-hand :contract]))

(defn players [game] (vals (:players game)))

(defn update-player
  ([game player]
    (assoc-in game [:players (:id player)] player))
  ([game player f & args]
    (update-player game (apply update player f args))))

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

;; TODO - draw an upcard
(defn deal-hand [game contract]
  (let [[hands deck] (draw-hands (count (players game)) (get-in game [:rules :cards-per-hand]) (shuffle (:deck game)))
        players (map #(assoc %1 :hand %2) (players game) hands)]
    (-> game
        (assoc :current-hand (new-hand contract))
        (assoc :deck deck)
        (update-players players)
        (#(emit % (event :begin-hand %))))))

(defn discard
  ([game player-id card] (discard game player-id card false))
  ([game player-id card call-shanghai?]
   (if (and call-shanghai? (not (get-in game [:rules :allow-calling-shanghai?])))
     (throw (IllegalArgumentException. "Game rules do not permit calling shanghai!"))
     (let [game (-> game
                    (update-in [:current-hand :discards] conj card)
                    (update-in [:players player-id :hand] #(vec (remove #{card} %))))]
       (if call-shanghai?
         game
         (assoc-in game [:current-hand :upcard] card))))))

(defn validate-melds [game melds]
  (loop [contract (current-contract game)]
    (if-let [[contract-type [num-melds num-cards-per-meld]] (first contract)]
      (let [contract-type-melds (melds contract-type)
            validate-fn (partial (if (= :runs contract-type) is-run? is-set?) (get-in game [:rules :wild-cards]))
            validate-meld (fn [cards] (and (>= (count cards) num-cards-per-meld) (validate-fn cards)))
            error (fn [& msg] (throw (IllegalStateException. ^String (apply str msg))))]
        (cond
          (not= num-melds (count contract-type-melds)) (error "Must have " num-melds " " (name contract-type) " to meld!")
          (not-every? validate-meld contract-type-melds) (error "One or more " (name contract-type) " is invalid!")
          :else (recur (rest contract))))
      melds)))

(defn meld [game player-id melds]
  (let [melds (validate-melds game melds)
        meld-cards (set (flatten (vals melds)))
        player (-> (get-in game [:players player-id])
                   (assoc :melds melds)
                   (update :hand #(vec (remove meld-cards %))))]  ;; TODO fix - this will remove ALL cards of the same rank/suit
    (update-player game player)))

;; TODO - ensure it's a real down and out?
(defn down-and-out [game player-id melds final-card]
  (-> game
      (meld player-id melds)
      (discard player-id final-card)))

(defn play-cards [game player-id plays] game)  ;; TODO

(defn index-relative-to-dealer [relative-index game]
  ;; use (count (:hands game)) as a counter that increments after each hand
  (mod (+ (count (:hands game)) relative-index) (count (players game))))

(def left-of-dealer-index (partial index-relative-to-dealer 1))

(defn draw-card [game player-id]
  (let [{:keys [current-hand deck draw-choice-fn]} game
        upcard (:upcard current-hand)
        player (get-in game [:players player-id])
        draw-choice (if (nil? upcard) :deck (draw-choice-fn player upcard))
        [drawn-card game] (if (= draw-choice :deck)
                            (let [[top-card deck] (draw deck)] [top-card (assoc game :deck deck)])
                            [upcard (update game :current-hand #(-> % (dissoc :upcard) (update :discards (partial drop 1))))])]
    [drawn-card (update-in game [:players player-id :hand] #(sort-cards (conj % drawn-card)))]))

(defn action-fn-state [game player-id new-card]
  {:player (get-in game [:players player-id])
   :contract (current-contract game)
   :rules (:rules game)
   :new-card new-card
   :played-melds (map-vals :melds (:players game))})

(defn validate-actions [actions]
  (let [action-type-indices (fn [type] (seq (map first (filter #(#{type} (:action (second %))) (enumerate actions)))))
        discard-indices (action-type-indices :discard)
        down-and-out-indices (action-type-indices :down-and-out)
        total (+ (count discard-indices) (count down-and-out-indices))
        error (fn [^String msg] (throw (IllegalStateException. msg)))]
    (cond
      (not= total 1) (error "Actions must include exactly one discard or down-and-out!")
      (and discard-indices (not= (dec (count actions)) (first discard-indices))) (error "Discard must be the last action!")
      (and down-and-out-indices (not= (count actions) 1)) (error "Down-and-out must be the only action!")
      :else actions)))

(defn player-turn [game player-id]
  (let [[drawn-card game] (draw-card game player-id)  ;; TODO - emit event before this (so hand can be shown)?
        _ (emit game (event :player-to-act game (get-in game [:players player-id])))
        actions (validate-actions ((:action-fn game) (action-fn-state game player-id drawn-card)))]
    (loop [game game
           actions actions]
      (if-let [{:keys [action card melds plays call-shanghai?] :as action-map} (first actions)]
        (let [game (case action
                     :discard (discard game player-id card call-shanghai?)
                     :down-and-out (down-and-out game player-id melds card)
                     :meld (meld game player-id melds)
                     :play-cards (play-cards game player-id plays)
                     (throw (IllegalStateException. (str "Unknown action: " (pr-str action)))))]
          (emit game (event :player-action (get-in game [:players player-id]) action-map))
          (recur game (rest actions)))
        game))))

(defn round-of-play [game]
  (let [player-ids (map :id (rotate (left-of-dealer-index game) (players game)))]
    (loop [game game
           player-ids (cycle player-ids)]
      (let [player-id (first player-ids)
            game (player-turn game player-id)]
        (if (empty? (get-in game [:players player-id :hand]))
          (emit game (event :win (get-in game [:players player-id])))
          (recur game (rest player-ids)))))))  ;; TODO - offer other players a chance to buy

(defn score-hand
  ([hand] (score-hand hand default-point-values))
  ([hand point-values] (sum (map (comp point-values :key :rank) hand))))

(defn tally-scores [game]
  (let [point-values (get-in game [:rules :point-values])
        update-score (fn [player] (update player :score + (score-hand (:hand player) point-values)))]
    (update-players game (map update-score (players game)))))

(defn recycle-cards [game]
  (let [{:keys [current-hand]} game
        players (players game)
        hands (->> players
                   (map :hand)
                   (remove nil?)
                   flatten)
        melds (->> players
                   (map :melds)
                   (remove nil?)
                   (map vals)
                   flatten)
        cards (concat hands melds (:discards current-hand))]
    (-> game
        (update :deck add cards)
        (update-players (map #(dissoc % :hand :melds) players)))))

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

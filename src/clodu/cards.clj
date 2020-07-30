(ns clodu.cards
  (:require [clojure.set :as set]
            [clodu.constants :refer [num-jokers-per-deck]]
            [clodu.utils :refer [enumerate make-printable sorted-map-by-value]]))

(defrecord Suit [value symbol]
  Comparable
    (compareTo [_ other] (compare value (:value other)))
  Object
    (toString [_] symbol))

(def suits (sorted-map-by-value {:spades (Suit. 4 "♠︎")
                                 :hearts (Suit. 3 "♥︎")
                                 :diamonds (Suit. 2 "♦︎")
                                 :clubs (Suit. 1 "♣")}))

(defrecord Rank [key value symbol name]
  Comparable
    (compareTo [_ other] (compare value (:value other)))
  Object
    (toString [_] symbol))

(def ^:private rank-data [[:2 "2" "deuce"]
                          [:3 "3" "three"]
                          [:4 "4" "four"]
                          [:5 "5" "five"]
                          [:6 "6" "six"]
                          [:7 "7" "seven"]
                          [:8 "8" "eight"]
                          [:9 "9" "nine"]
                          [:10 "10" "ten"]
                          [:jack "J" "jack"]
                          [:queen "Q" "queen"]
                          [:king "K" "king"]
                          [:ace "A" "ace"]])

(def ranks (sorted-map-by-value
             (into {} (for [[i [key symbol name]] (enumerate rank-data)]
                        [key (Rank. key (+ i 2) symbol name)]))))

(def ^:private joker-rank (Rank. :joker (inc (:value (ranks :ace))) "<?>" "joker"))

(defrecord Card [rank suit]
  Comparable
    (compareTo [_ other] (compare [rank suit] [(:rank other) (:suit other)]))
  Object
    (toString [_] (str rank (or suit ""))))

(make-printable Card)

(def ^:const joker (Card. joker-rank nil))

(defn is-joker? [card] (= joker card))

(defn is-wild-card? [wild-cards card]
  {:pre [(set? wild-cards)]}
  (boolean (wild-cards (:key (:rank card)))))

(def is-natural-card? (complement is-wild-card?))

(defn excluding [unwanted cards]
  (->> unwanted
       set
       (set/difference (set cards))))

(defn excluding-wild-cards [wild-cards cards]
  {:pre [(set? wild-cards) (seqable? cards)]}
  (filter (partial is-natural-card? wild-cards) cards))

(defn sort-cards [cards] (vec (sort cards)))

(defn card [rank suit] (Card. (ranks rank) (suits suit)))

(defn new-deck []
  (let [deck (for [suit (vals suits) rank (vals ranks)] (Card. rank suit))]
    (vec (concat deck (repeat num-jokers-per-deck joker)))))

(defn new-decks [n] (vec (flatten (repeatedly n new-deck))))

(defn draw
  ([deck] [(first deck) (vec (rest deck))])
  ([n deck] (draw n deck true))
  ([n deck sort?]
   (let [cards (take n deck)
         cards (if sort? (sort-cards cards) (vec cards))
         deck (vec (drop n deck))]
     [cards deck])))

(defn draw-hands [n cards-per-hand deck]
  (let [num-cards (* cards-per-hand n)
        [top-cards deck] (draw num-cards deck false)
        hands (mapv sort-cards (partition cards-per-hand top-cards))]
    [hands deck]))

(defn add [deck cards]
  (if (sequential? cards)
    (apply conj deck cards)
    (conj deck cards)))

(ns clodu.cards
  (:require [clodu.constants :refer [num-jokers-per-deck]]
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

(def ^:private joker-rank (inc (:value (ranks :ace))))

(declare is-joker?)

(defrecord Card [rank suit]
  Comparable
    (compareTo [card other]
      (let [rank (if (is-joker? card) joker-rank (:value rank))
            other-rank (if (is-joker? other) joker-rank (:value (:rank other)))]
        (compare [rank suit] [other-rank (:suit other)])))
  Object
    (toString [card] (if (is-joker? card) "<?>" (str rank suit))))

(make-printable Card)

(def ^:const joker (Card. :joker nil))

(defn is-joker? [card] (= joker card))

(defn sort-cards [cards] (-> cards sort reverse vec))

(defn card [rank suit] (Card. (ranks rank) (suits suit)))

(defn new-deck []
  (let [deck (for [suit (vals suits) rank (vals ranks)] (Card. rank suit))]
    (vec (concat deck (repeat num-jokers-per-deck joker)))))

(defn new-decks [n] (flatten (repeatedly n new-deck)))

(defn draw
  ([deck] [(first deck) (vec (rest deck))])
  ([n deck] (draw n deck true))
  ([n deck sort?]
   (let [cards (take n deck)
         cards (if sort? (sort-cards cards) (vec cards))
         deck (vec (drop n deck))]
     [cards deck])))

(defn add [deck cards]
  (if (sequential? cards)
    (apply conj deck cards)
    (conj deck cards)))

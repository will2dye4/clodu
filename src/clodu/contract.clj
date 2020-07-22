(ns clodu.contract
  (:require [clodu.cards :refer [excluding-wild-cards is-natural-card? is-wild-card? ranks]]))

;; Dye family house rules
(def ^:const default-min-cards {:sets 3 :runs 4})

(defn counts->contracts
  ([counts]
    (counts->contracts counts default-min-cards))
  ([counts min-cards]
    (mapv #(reduce-kv (fn [cs k v] (assoc cs k [v (min-cards k)])) {} %) counts)))

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

(defn at-least-half-natural? [cards natural-cards]
  (>= (count natural-cards) (Math/ceil (/ (count cards) 2))))

(defn is-set? [wild-cards cards]
  (let [all-wild? (every? (partial is-wild-card? wild-cards) cards)  ;; handle set of natural deuces
        natural-cards (excluding-wild-cards (if all-wild? #{:joker} wild-cards) cards)
        distinct-ranks (set (map :rank natural-cards))]
    (and (= (count distinct-ranks) 1)
         (at-least-half-natural? cards natural-cards))))

;; Assumption: cards are in ascending order (with wilds in the correct positions).
(defn in-consecutive-order? [wild-cards cards]
  (let [{:keys [rank] :as first-natural-card} (first (filter (partial is-natural-card? wild-cards) cards))
        ace-rank (ranks :ace)
        first-natural-rank (if (= rank ace-rank) 1 (:value rank))
        initial-rank (- first-natural-rank (.indexOf (vec cards) first-natural-card))]
    (if (< initial-rank 1)
      false
      (loop [effective-rank initial-rank
             cards cards]
        (if-let [card (first cards)]
          (if (or (> effective-rank (:value ace-rank))
                  (and (is-natural-card? wild-cards card)
                       (if (= effective-rank 1)
                         (not= ace-rank (:rank card))
                         (not= effective-rank (get-in card [:rank :value])))))
            false
            (recur (inc effective-rank) (rest cards)))
          true)))))

;; TODO - handle run with two wilds and a natural deuce
(defn is-run? [wild-cards cards]
  (let [natural-cards (excluding-wild-cards wild-cards cards)
        distinct-suits (set (map :suit natural-cards))]
    (and (= (count distinct-suits) 1)
         (at-least-half-natural? cards natural-cards)
         (in-consecutive-order? wild-cards cards))))

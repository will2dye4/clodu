(ns clodu.contract
  (:require [clodu.cards :refer [excluding-wild-cards is-wild-card?]]))

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

(defn is-set? [cards rules]
  (let [{:keys [allow-any-number-of-wild-cards? wild-cards]} rules
        all-wild? (every? (partial is-wild-card? wild-cards) cards)  ;; handle set of natural deuces
        natural-cards (excluding-wild-cards (if all-wild? #{:joker} wild-cards) cards)
        distinct-ranks (set (map :rank natural-cards))]
    (and (= (count distinct-ranks) 1)
         (or allow-any-number-of-wild-cards?
             (>= (count natural-cards) (Math/ceil (/ (count cards) 2)))))))

(defn is-run? [cards rules] false)  ;; TODO

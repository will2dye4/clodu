(ns clodu.cli
  (:require [clojure.string :as str]
            [clodu.cards :refer [new-deck]]
            [clodu.contract :refer [is-run? is-set?]]
            [clodu.game :refer :all]
            [clodu.utils :refer [input]]))

(defn pluralize-contract-type [contract-type num-melds]
  (let [name (name contract-type)]
    (if (= num-melds 1)
      (.substring name 0 (dec (count name)))
      name)))

(defn contract->str [contract]
  (->> contract
       (map (fn [[type [num-melds num-cards]]] (format "%d %s of %d" num-melds (pluralize-contract-type type num-melds) num-cards)))
       (str/join " and ")))

(defn flatten-melds [melds] (apply str (interpose "  " melds)))

(defn melds->str [melds] (flatten-melds (remove empty? (map flatten-melds (vals melds)))))

(defn show-board [game]
  (when-let [current-hand (:current-hand game)]
    (let [{:keys [contract upcard]} current-hand]
      (println "Contract:" (contract->str contract))
      (println "Upcard:" (or upcard "None"))
      (doseq [player (players game) :let [{:keys [melds name]} player]]
        (println (str name ":") (if melds (melds->str melds) "None (yet)")))
      (println))))

(defn show-player-info [game player]
  (let [{:keys [show-all-player-info?]} game
        {hand :hand name :name player-id :id} player
        show-full-info? (or show-all-player-info? (= player-id 1))
        player-hand (str (if show-full-info? hand (vec (repeat (count hand) 'XX))))]
    (println (format "%-10s\t%s" name player-hand))))

(defn show-standings [game]
  (println "\n================ Standings ================")
  (doseq [{:keys [name score wins]} (players game)
          :let [chips (format "%,5d" score)
                wins (format "%,d %s" wins (if (= 1 wins) "win" "wins"))]]
    (println (format "%s\t%s\t%s" name chips wins)))
  (println))

(defn indefinite-article [card] (if (#{:8 :ace} (:key (:rank card))) "an" "a"))

(defmulti cli-event-handler (fn [[event-type & attrs]] event-type))

(defmethod cli-event-handler :begin-hand
  [[_ game]]
  (println "=============" (str "Hand #" (inc (count (:hands game)))) "=============")
  (show-board game))

(defmethod cli-event-handler :player-action
  [[_ player action]]
  (let [{:keys [action card melds call-shanghai?]} action
        action (case action
                 :discard (format "discards %s %s%s" (indefinite-article card) card (if call-shanghai? " and calls shanghai" ""))
                 :down-and-out (str "goes down and out with " (melds->str melds))
                 :meld (str "melds with " (melds->str melds))
                 :play-cards "plays cards on other players' melds")]  ;; TODO
    (println (format "--> %s %s" (:name player) action))))

(defmethod cli-event-handler :end-hand
  [[_ game]] (show-standings game))

(defmethod cli-event-handler :player-to-act
  [[_ game player]] (show-player-info game player))

(defmethod cli-event-handler :win
  [[_ winner]]
  (println "\n============= Hand Finished =============")
  (println (:name winner) "wins"))

(defmethod cli-event-handler :default [_])  ;; do nothing

(defn card-strs-equal? [& strs]
  (apply = (map #(str/replace % "\uFE0E" "") strs)))

(defn str->card [card-str]
  (->> (new-deck)
       (filter #(card-strs-equal? card-str (str %)))
       first))

(def ^:const card-re #"((?i:10|[23456789JQKA])[♠♥♦♣]|<\?>)")

(defn parse-cards [card-str]
  (loop [matcher (re-matcher card-re (str/upper-case card-str))
         cards []]
    (if-let [match (re-find matcher)]
      (recur matcher (conj cards (str->card (first match))))
      cards)))

(defn sanitized-input [prompt] (str/lower-case (str/trim (input prompt))))

(defn cli-draw-choice-fn [player upcard]
  (let [prompt (str (:name player) " may draw from the deck or take the current upcard (" upcard "). Enter 'deck' or 'upcard': ")]
    (loop []
      (let [choice (sanitized-input prompt)]
        (if (#{"deck" "upcard"} choice)
          (keyword choice)
          (do (println "Invalid choice!") (recur)))))))

(defn parse-discard-action [player rules args]
  (if-not (#{1 2} (count args))
    (println "Invalid discard action! (wrong number of arguments)")
    (let [card (first args)
          shanghai? (or (second args) "")]
      (if-not (#{"" "shanghai"} shanghai?)
        (println "Invalid discard action! (invalid shanghai argument)")
        (let [shanghai? (= shanghai? "shanghai")]
          (if (and shanghai? (not (:allow-calling-shanghai? rules)))
            (println "Invalid discard action! (calling shanghai not allowed)")
            (let [cards (parse-cards card)]
              (if (not= (count cards) 1)
                (println "Invalid discard action! (invalid card argument)")
                (let [card (first cards)]
                  (if-not ((set (:hand player)) card)
                    (println "Invalid discard action! (card not in player's hand)")
                    {:action :discard, :card card, :call-shanghai? shanghai?}))))))))))

(defn parse-meld-args [wild-cards args]
  (loop [parsed-melds {:sets [] :runs []}
         all-melds (map parse-cards args)]
    (if-let [meld (first all-melds)]
      (let [all-melds (rest all-melds)
            parsed-melds (cond
                           (is-set? wild-cards meld) (update parsed-melds :sets conj meld)
                           (is-run? wild-cards meld) (update parsed-melds :runs conj meld)
                           :else parsed-melds)]
        (recur parsed-melds all-melds))
      parsed-melds)))

(defn has-all-cards? [hand cards]
  (let [distinct-cards (set cards)
        hand-cards (filter distinct-cards hand)]
    (>= (count hand-cards) (count cards))))

(defn parse-meld-action [player current-contract rules args]
  (let [{:keys [hand]} player
        {:keys [wild-cards]} rules
        melds (parse-meld-args wild-cards args)]
    (if-not (has-all-cards? hand (flatten (vals melds)))
      (println "Invalid meld action! (cards not in player's hand)")
      (loop [contract current-contract]
        (if-let [[contract-type [num-melds num-cards-per-meld]] (first contract)]
          (let [contract-type-melds (melds contract-type)]
            (cond
              (not= num-melds (count contract-type-melds)) (println (format "Invalid meld action! (must have %d %s)" num-melds (name contract-type)))
              (not-every? #(>= (count %) num-cards-per-meld) contract-type-melds) (println (format "Invalid meld action! (invalid %s)" (name contract-type)))
              :else (recur (rest contract))))
          {:action :meld, :melds melds})))))

(defn available-actions [player prev-actions]
  (let [{:keys [melds]} player
        prev-actions (set (map :action prev-actions))]
    (concat ["discard"] (if (and (not melds) (not (prev-actions :meld))) ["down-and-out" "meld"] ["play"]))))

;; TODO - parse remaining actions
(defn cli-action-fn [state]
  (let [{:keys [contract new-card player rules]} state
        {:keys [name]} player
        prompt (str name " may %s: ")]
    (println name "drew" new-card)
    (loop [actions []]
      (let [allowed-actions (available-actions player actions)
            args (str/split (sanitized-input (format prompt (str/join ", " allowed-actions))) #"\s+")
            verb (first args)]
        (cond
          (not ((set allowed-actions) verb)) (do (println "Invalid action!") (recur actions))
          (= verb "discard") (if-let [action (parse-discard-action player rules (rest args))]
                               (conj actions action)
                               (recur actions))
          (= verb "meld") (if-let [action (parse-meld-action player contract rules (rest args))]
                            (recur (conj actions action))
                            (recur actions))
          :else (do (println "Action not supported yet!") (recur actions)))))))

(def ^:private auto-draw-choice-fn default-draw-choice-fn)

(def ^:private auto-action-fn default-action-fn)

(defn single-player-draw-choice-fn [player upcard]
  (let [f (if (= 1 (:id player)) cli-draw-choice-fn auto-draw-choice-fn)]
    (f player upcard)))

(defn single-player-action-fn [state]
  (let [f (if (= 1 (:id (:player state))) cli-action-fn auto-action-fn)]
    (f state)))

(defn default-cli-opts [mode] {:show-all-player-info? (not= mode :single-player)})

(defn new-cli-game
  ([] (new-cli-game :interactive))
  ([mode] (new-cli-game mode default-num-players))
  ([mode num-players]
    (let [[draw-choice-fn action-fn] (case mode
                                       :auto [auto-draw-choice-fn auto-action-fn]
                                       :interactive [cli-draw-choice-fn cli-action-fn]
                                       :single-player [single-player-draw-choice-fn single-player-action-fn]
                                       :cheat [single-player-draw-choice-fn single-player-action-fn]
                                       (throw (IllegalArgumentException. (str "Unknown mode: " (pr-str mode)))))]
      (-> (new-game :draw-choice-fn draw-choice-fn :action-fn action-fn :num-players num-players)
          (merge (default-cli-opts mode))
          (register-event-handler cli-event-handler)))))

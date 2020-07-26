(ns clodu.cli
  (:require [clojure.string :as str]
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

(defn melds->str [melds] (str/join "  " (vals melds)))

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

(defn cli-draw-choice-fn [player upcard] :deck)  ;; TODO

(def ^:private action-names (sorted-map "discard" :discard, "down-and-out" :down-and-out, "meld" :meld, "play" :play-cards))

(defn cli-action-fn [state]
  (let [{:keys [player]} state
        prompt (format "%s may %s: " (:name player) (str/join ", " (keys action-names)))]
    (loop []
      (let [args (str/split (input prompt) #"\s+")
            verb (first args)]
        (cond
          (not (action-names verb)) (do (println "Invalid action!") (recur))
          :else (default-action-fn state))))))  ;; TODO - parse action

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

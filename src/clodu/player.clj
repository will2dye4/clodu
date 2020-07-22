(ns clodu.player)

(defrecord Player [id name])

(defn new-player [id] (Player. id (str "Player " id)))

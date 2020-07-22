(ns clodu.player)

(defrecord Player [id name score])

(defn new-player [id] (Player. id (str "Player " id) 0))

(ns snake.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [clojure.java.io :as io]))

(def ^:const pixels-per-tile 16)

(declare snake-game main-screen score-screen)

(defn game-over []
  (set-screen! snake-game main-screen score-screen))

(defn get-direction [old-direction]
  (let [new-direction
        (cond
          (key-pressed? :dpad-up) [0 pixels-per-tile]
          (key-pressed? :dpad-down) [0 (- pixels-per-tile)]
          (key-pressed? :dpad-left) [(- pixels-per-tile) 0]
          (key-pressed? :dpad-right) [pixels-per-tile 0]
          (key-pressed? :space) [0 0]
          :else old-direction)]
    (if (and
          (= (+ (first new-direction) (first old-direction)) 0)
          (= (+ (second new-direction) (second old-direction)) 0))
      old-direction
      new-direction)))

(defn ensure-in-range [x max]
  (cond
    (>= x max) (- x max)
    (< x 0) (+ max x)
    :else x))

(defn collide? [e1 e2]
  (and (= (:x e1) (:x e2)) (= (:y e1) (:y e2))))

(defn collide-with? [e1 entities]
  (some (partial collide? e1) entities))

(defn handle-collision [entities entity]
  (cond
    (:player? entity)
    (let [player-head (first (:entities entity))
          player-parts (rest (:entities entity))]
      (cond
        (collide-with? player-head (filter :apple? entities))
        (do
          (screen! score-screen :on-update-score :score (count (:entities entity)))
          (assoc entity :eating-apple? true))

        (collide-with? player-head player-parts)
        (do
          (game-over)
          entity)

        :else
        entity))

    (:apple? entity)
    (let [player-parts (apply :entities (filter :player? entities))]
      (if (collide-with? entity player-parts)
        (assoc entity :to-destroy? true)
        entity))

    (:bomb? entity)
    (let [player-parts (apply :entities (filter :player? entities))]
      (if (collide-with? entity [(first player-parts)])
        (game-over)
        entity))

    :else
    entity))

(defn create-snake-texture [pos]
  (let [fixed-pos (map ensure-in-range pos [(game :width) (game :height)])]
    (assoc (texture "snake.png") :x (first fixed-pos) :y (second fixed-pos))))

(defn create-snake []
  (let [body [[400 400]]
        textures (map create-snake-texture body)]
    (assoc (bundle) :entities textures
                    :player? true :last-direction [0 pixels-per-tile])))

(defn get-free-slot [entities]
  (let [x (* (rand-int (quot (game :width) pixels-per-tile)) pixels-per-tile)
         y (* (rand-int (quot (game :height) pixels-per-tile)) pixels-per-tile)
         snake (filter :player? entities)]

    (if (or (collide-with? {:x x :y y} entities) (collide-with? {:x x :y y} snake))
      (recur entities)
      [x y])))

(defn create-bomb [entities]
  (let [pos (get-free-slot entities) x (first pos) y (second pos)]
    (assoc (texture "bomb.png") :x x :y y :bomb? true)))

(defn create-apple [entities]
  (let [pos (get-free-slot entities) x (first pos) y (second pos)]
    (assoc (texture "apple.png") :x x :y y :apple? true)))

(defn move-snake [entity]
  (if (:player? entity)
    (let [grow? (:eating-apple? entity)
          displacement (get-direction (:last-direction entity))
          textures (:entities entity)
          pos (map + displacement [(:x (first textures)) (:y (first textures))])]

      (assoc entity :last-direction displacement
                    :eating-apple? false
                    :entities
                    (cons (create-snake-texture pos)
                          (if grow? textures (butlast textures)))))
    entity))

(defn clean-entity [entities entity]
  (if (:to-destroy? entity)
    (if (:apple? entity)
      (list (create-apple entities) (create-bomb entities))
      nil)
    entity))

(defscreen main-screen
           :on-show
           (fn [screen entities]
             (update! screen :renderer (stage)
                      :camera (orthographic))
             [(create-snake) (repeatedly 1 #(create-apple entities))])

           :on-resize
           (fn [screen entities]
             (height! screen 800))

           :on-render
           (fn [screen entities]
             (clear!)
             (some->> (map (fn [entity]
                             (->> entity
                                  (move-snake)
                                  (handle-collision entities)
                                  (clean-entity entities)))
                           entities)
                      (render! screen)))

           :on-key-down
           (fn [screen entities]
             (cond
              (key-pressed? :R) (game-over)
              (key-pressed? :escape) (game-over))))

(def highscore (atom 0))

(defscreen score-screen
           :on-show
           (fn [screen entities]
             (update! screen :camera (orthographic) :renderer (stage))
             [(assoc (label "0" (color :white))
                :id :score
                :score 0
                :x 15 :y 6)
              (assoc (label (str "HighScore " @highscore) (color :orange))
                  :id :highscore
                  :highscore highscore
                  :x 150 :y 6)])

           :on-update-score
           (fn [screen entities]
             (let [score (:score screen)]
               (->> (for [entity entities]
                      (case (:id entity)
                        :score (doto entity (label! :set-text (str score)))
                        :highscore (if (> score @highscore)
                                     (do
                                       (compare-and-set! highscore @highscore score)
                                       (doto entity (label! :set-text (str "HighScore " @highscore))))
                                     entity)
                        entity)))))

           :on-render
           (fn [screen entities]
             (render! screen entities))

           :on-resize
           (fn [screen entities]
             (height! screen 300)))

(defscreen blank-screen
           :on-render
           (fn [screen entities]
             (clear!))

           :on-key-down
           (fn [screen entities]
             (if (key-pressed? :R)
               (game-over))))


(set-screen-wrapper! (fn [screen screen-fn]
                       (try (screen-fn)
                            (catch Exception e
                              (.printStackTrace e)
                              (set-screen! snake-game blank-screen)))))

(defgame snake-game
         :on-create
         (fn [this]
           (set-screen! this main-screen score-screen)))



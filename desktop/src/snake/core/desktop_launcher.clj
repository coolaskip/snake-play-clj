(ns snake.core.desktop-launcher
  (:require [snake.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication LwjglApplicationConfiguration]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (def config (LwjglApplicationConfiguration.))
  (set! (. config width) 800)
  (set! (. config height) 800)
  (set! (. config title) "snake")
  (set! (. config foregroundFPS) 20)
  (LwjglApplication. snake-game config)
  (Keyboard/enableRepeatEvents true))

(ns app.main
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))

(def app (r/atom {}))

(defn root []
  (fn []
    [:h1 (:greeting @app)]) )

(defn ^:export main! []
  (rdom/render
   [root]
   (js/document.getElementById "app")))

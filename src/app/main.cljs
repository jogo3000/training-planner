(ns app.main
  (:require
   [clojure.string :as str]
   [reagent.core :as r]
   [reagent.dom :as rdom])
  (:import [goog.date Date DateTime Interval]))

(def local-storage-key "training-planner-exercises")
(def canvas-id "week-svg")
(def canvas-width 2100)
(def canvas-height 400)
(def day-width (/ canvas-width 8))

(defn date->last-monday [date]
  (let [day (.getDate date)
        weekday (.getIsoWeekday date)
        year (.getYear date)
        month (.getMonth date)]
    (if (zero? weekday)
      date
      (Date. year month (- day weekday)))))

(defn adjust-days [date days]
  (let [new-date (.clone date)]
    (->> (Interval. Interval/DAYS days)
         (.add new-date))
    new-date))

(defn dec-week [date]
  (adjust-days date -7))

(defn inc-week [date]
  (adjust-days date 7))

(defn adjust-hours [datetime hours]
  (let [new (.clone datetime)]
    (->> (Interval. Interval/HOURS hours)
         (.add new))
    new))

(defn date>= [date1 date2]
  (>= (Date/compare date1 date2) 0))

(defn date<= [date1 date2]
  (<= (Date/compare date1 date2) 0))

(defn serialize-start-time [date]
  (when date (.toIsoString date)))

(defn parse-isodatetime [s]
  (let [[year month date] ((juxt #(.getYear %)
                                 #(.getMonth %)
                                 #(.getDate %))
                           (DateTime/fromIsoString s))]
       (Date. year month date)))

(defn serialize-exercises [exercises]
  (->> (mapv (fn [ex]
               (-> (update ex :id #(.toString %))
                   (update :start-time serialize-start-time)))
             exercises)
       clj->js
       (#(js/JSON.stringify %))))

(defn deserialize-exercises [s]
  (->> (js/JSON.parse s)
       (#(js->clj % :keywordize-keys true))
       (mapv (fn [ex]
               (-> (update ex :id #(uuid %))
                   (update :start-time parse-isodatetime)
                   ((juxt :id identity)))))
       (into {})))

(defn persist-exercises [_ _ _ new-state]
  (->> (:exercises new-state)
       vals
       serialize-exercises
       (.setItem js/window.localStorage local-storage-key)))

(defn load-exercises []
  (->> (.getItem js/window.localStorage local-storage-key)
       deserialize-exercises))

(defonce db
  (let [stored-exercises (load-exercises)
        ratom (r/atom {:start-date (date->last-monday (Date.))
                       :editor ""
                       :drag {:dragging? false}
                       :selected-element nil
                       :exercises stored-exercises})]
    (add-watch ratom :persistence-watcher persist-exercises)
    ratom))

(defmulti handle (fn [event _ & _] event))

(defmethod handle :default [_ db & _] db)

(defn emit [event & data]
  (r/rswap! db #(apply (partial handle event %) data)))

(defmethod handle :clear-selected-exercise [_ db]
  (assoc db :selected-element nil
         :editor ""))

(defn mouse-position [evt]
  (let [ctm (.getScreenCTM (.-target evt))
        ctm-a (.-a ctm)
        ctm-e (.-e ctm)
        ctm-d (.-d ctm)
        ctm-f (.-f ctm)
        mouse-x (.-clientX evt)
        mouse-y (.-clientY evt)]
    {:x (/ (- mouse-x ctm-e) ctm-a)
     :y (/ (- mouse-y ctm-f) ctm-d)}))

(defn copy-exercise [id exercises]
  (let [new-id (random-uuid)
        copy (-> (get exercises id)
                 (assoc :id new-id)
                 (update :z dec))]
    (assoc exercises new-id copy)))

(defn maybe-copy-exercise [db ctrl-down? id]
  (if ctrl-down?
    (update db :exercises (partial copy-exercise id))
    db))

(defmethod handle :start-drag [_ db evt {:keys [id x y description]}]
  (let [mouse-position (mouse-position evt)
        ctrl-down? (.-ctrlKey evt)]
    (-> (maybe-copy-exercise db ctrl-down? id)
        (assoc-in [:drag :dragging?] true)
        (assoc :selected-element id)
        (assoc :editor description)
        (assoc-in [:drag :offset] {:x (- (:x mouse-position) x)
                                   :y (- (:y mouse-position) y)}))))

(defn identify-datetime [start-date {:keys [x y]}]
  (let [weekday (js/Math.floor (/ x day-width))
        hour (if (< y 200) 6 16)
        date (adjust-days start-date weekday)]
    (DateTime. (.getYear date) (.getMonth date) (.getDate date) hour 0)))

(defmethod handle :stop-drag [_ db]
  (assoc db :drag {:dragging? false}))

(defn correct-mouse-position [mouse offset]
  {:x (- (:x mouse) (:x offset))
   :y (- (:y mouse) (:y offset))})

(defn exercises-for-week [monday exercises]
  {:pre [(seq? exercises)]
   :post [(seq? %)]}
  (let [sunday (adjust-days monday 6)]
    (filter (fn [{start-time :start-time}]
              (or (not start-time)
                  (let [start-date (Date. (.getYear start-time) (.getMonth start-time) (.getDate start-time))
                        ex-after-monday (date>= start-date monday)
                        ex-before-sunday (date<= start-date sunday)]
                    (and ex-after-monday ex-before-sunday)))) exercises)))

(defmethod handle :update-editor [_ db evt]
  (let [new-value (-> evt .-target .-value)
        selected-element (:selected-element db)]
    (cond-> (assoc db :editor new-value)
      selected-element (update-in [:exercises selected-element] #(assoc % :description new-value)))))

(defmethod handle :create-exercise [_ db]
  (let [id (random-uuid)]
    (-> (assoc-in db [:exercises id]
                  {:id id
                   :x 0
                   :y 0
                   :z 0
                   :description (:editor db)})
        (assoc :selected-element id))))

(defmethod handle :delete-exercise [_ db]
  (let [selected (:selected-element db)]
    (-> (update db :exercises
                #(dissoc % selected))
        (assoc :selected-element nil
               :editor ""))))

(defmethod handle :previous-week [_ db]
  (update db :start-date dec-week))

(defmethod handle :next-week [_ db]
  (update db :start-date inc-week))

(defmethod handle :mousemove [_ db evt]
  (if-not (get-in db [:drag :dragging?]) db
          (let [offset (get-in db [:drag :offset])
                start-date (:start-date db)
                selected-element (:selected-element db)
                mouse-position (-> (mouse-position evt)
                                                  (correct-mouse-position offset))
                new-datetime (identify-datetime start-date mouse-position)]

            (update-in db [:exercises selected-element]
                       #(merge % mouse-position {:start-time new-datetime})))))

(defn ical-render-summary [description]
  (first (str/split-lines description)))

(defn ical-render-description [description]
  (str/escape description {\newline "\\n"}))

(defn ical-render-exercise [exercise]
  (if-let [start (:start-time exercise)]
    (let [created "20200524T135634Z"
          uid (:id exercise)
          end (adjust-hours start 1)
          description (:description exercise)]
      (str
       "\nBEGIN:VEVENT"
       "\nDTSTAMP:" created
       "\nUID:" uid
       "\nDTSTART:" (.toString start)
       "\nDTEND:" (.toString end)
       "\nSUMMARY:" (ical-render-summary description)
       "\nDESCRIPTION:" (ical-render-description description)
       "\nCATEGORIES:training-plan"
       "\nEND:VEVENT"))))

(defn to-ical [exercises]
  (str "BEGIN:VCALENDAR
VERSION:2.0
X-WR-CALNAME:harjoitukset
PRODID:-//Matti Uusitalo//training-planner//EN
X-WR-TIMEZONE:EEST
X-WR-CALDESC:Viikon harjoitukset
CALSCALE:GREGORIAN"
       (apply str (map ical-render-exercise exercises))
       "\nEND:VCALENDAR
"))

(defn render-date [date]
  (let [weekday (-> (.getIsoWeekday date)
                        (case
                            0 "Maanantai"
                            1 "Tiistai"
                            2 "Keskiviikko"
                            3 "Torstai"
                            4 "Perjantai"
                            5 "Lauantai"
                            6 "Sunnuntai"))]
    (str weekday " " (.getDate date) "." (inc (.getMonth date)) "." (.getYear date))))

(defn render-week-grid [monday]
  (let [date-headers (-> (mapv #(render-date (adjust-days monday %)) (range 7))
                         (conj "Yhteenveto"))]
    (map-indexed  (fn [i title]
                    (let [top 0
                          left (* i day-width)]
                      [:<>
                       [:rect {:on-click #(emit :clear-selected-exercise)
                               :width day-width
                               :height canvas-height
                               :stroke-width 2
                               :stroke "black"
                               :x left
                               :y top
                               :fill "white"}]
                       [:text {:x (+ left 10)
                               :y (+ top 20)}
                        title]]))
                  date-headers)))

(defn render-exercise [selected-exercise {:keys [id x y description] :as exercise}]
  {:pre [(map? exercise)]}
  (let [text-offset-x (+ 5 x)
        text-offset-y (+ 25 y)
        lines (str/split-lines description)]
    [:g {:id id
         :on-mouse-down #(emit :start-drag % exercise)
         :on-mouse-up #(emit :stop-drag)}
     [:rect (merge {:width 250
                    :height 50
                    :x x :y y
                    :fill "green"}
                   (when (= selected-exercise id)
                     {:stroke "black"
                      :stroke-width 3}))]
     (into [:text {:x text-offset-x :y text-offset-y
                   :fill "black"}]
           (map-indexed  (fn [i line]
                           [:tspan {:x text-offset-x
                                    :y (+ text-offset-y (* i 18))} line]) lines))]))

(defn render-exercises [exercises selected-exercise]
  {:pre [(seq? exercises)]
   :post [(seq? %)]}
  (let [in-drawing-order (sort :z exercises)]
    (map (partial render-exercise selected-exercise) in-drawing-order)))

(defn root []
  (fn []
    (let [start-date (:start-date @db)
          monday (date->last-monday start-date)
          selected-exercise (:selected-element @db)
          exercises (vals (:exercises @db))
          editor (:editor @db)]
      [:<>
       [:link {:rel "stylesheet" :href "/css/main.css"}]
       [:div {:class "flex-down"}
        [:div {:class "flex-right"}
         [:button {:class "primary-button"
                   :on-click #(emit :previous-week)}"Edellinen viikko"]
         [:button {:class "primary-button"
                   :on-click #(emit :next-week)} "Seuraava viikko"]]
        (-> [:svg {:id canvas-id
                   :width canvas-width :height canvas-height
                   :on-mouse-move #(emit :mousemove %)
                   :on-mouse-up #(emit :stop-drag)}]
            (into (render-week-grid monday))
            (into (render-exercises (exercises-for-week monday exercises) selected-exercise)))
        [:textarea {:rows 10
                    :cols 80
                    :value editor
                    :on-change #(emit :update-editor %)}]
        [:button {:class "primary-button"
                  :on-click #(emit :create-exercise)}
         "Luo harjoitus"]
        [:button {:class "primary-button"
                  :on-click #(emit :delete-exercise)}
         "Poista harjoitus"]
        ;; FIXME: pois debugit. Voisko tähän saada aidon debuggerin kiinni?
        [:p (with-out-str (cljs.pprint/pprint @db))]
        [:a {:href (str "data:text/plain;charset=utf-8," (js/encodeURIComponent (to-ical exercises)))
             :download "harjoitukset.ics"}
         "Lataa viikon harjoitukset"]]])) )

(defn read-key [evt]
  (.-key evt))

(defn ^:export ^:dev/after-load main! []
  (rdom/render
   [root]
   (js/document.getElementById "app")))

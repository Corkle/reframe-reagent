(ns phonecat-re-frame.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [accountant.core :as accountant]
            [re-frame.core :as re-frame]
            [ajax.core :as ajax])
  (:require-macros [reagent.ratom :refer [reaction]])
  (:import goog.History))

;; -------------------------
;; Re-Frame Data

(defn- handle-search-input-entered
  [db [_ search-input]]
  (assoc-in db [:search-input] search-input))

(defn- handle-order-prop-changed
  [db [_ order-prop]]
  (assoc-in db [:order-prop] order-prop))

(re-frame/register-sub
  :search-input
  (fn [db]
    (reaction (:search-input @db))))

(re-frame/register-sub
  :phones
  (fn [db]
    (reaction (:phones @db))))

(re-frame/register-sub
  :order-prop
  (fn [db]
    (reaction (:order-prop @db))))

(re-frame/register-sub
  :phone-details
  (fn [db]
    (reaction (:phone-details @db))))

(re-frame/register-sub
  :phone-query
  (fn [db [_ phone-id]]
    (let [phone-details-reaction (reaction (:phone-details @db))]
      (reaction ((keyword phone-id) @phone-details-reaction)))))

(re-frame/register-sub
  :selected-image-url
  (fn [db [_ phone-id]]
    (let [phone (re-frame/subscribe [:phone-query phone-id])
          phone-details (re-frame/subscribe [:phone-details])
          images (reaction (:images @phone))]
      (reaction
        (if @phone-details
          (get-in @phone-details [(keyword phone-id) :selected-image-url] (first @images)))))))

(re-frame/register-handler
  :search-input-entered
  handle-search-input-entered)

(re-frame/register-handler
  :order-prop-changed
  handle-order-prop-changed)

(re-frame/register-handler
  :set-image
  (fn [db [_ [phone-id selected-image-url]]]
    (assoc-in db [:phone-details (keyword phone-id) :selected-image-url] selected-image-url)))

(re-frame/register-handler
  :process-phones-response
  (fn [db [_ response]]
    (assoc-in db [:phones] response)))

(re-frame/register-handler
  :process-phones-bad-response
  (fn [db [_ response]]
    (println "Error getting phones")
    (println response)
    db))

(re-frame/register-handler
  :load-phones
  (fn [db _]
    (ajax/GET "phones/phones.json"
              {:handler #(re-frame/dispatch [:process-phones-response %1])
               :error-handler #(re-frame/dispatch [:process-phones-bad-response %1])
               :response-format :json
               :keywords? true})
    db))

(re-frame/register-handler
  :process-phone-detail-response
  (fn [db [_ phone-id response]]
    (assoc-in db [:phone-details (keyword phone-id)] response)))

(re-frame/register-handler
  :process-phone-detail-bad-response
  (fn [db [_ phone-id response]]
    (println "Error getting phone detail for id: " phone-id)
    (println response)
    db))

(re-frame/register-handler
  :load-phone-detail
  (fn [db [_ phone-id]]
    (ajax/GET (str "phones/" phone-id ".json")
              {:handler #(re-frame/dispatch [:process-phone-detail-response phone-id %1])
               :error-handler #(re-frame/dispatch [:process-phone-detail-bad-response phone-id %1])
               :response-format :json
               :keywords? true})
    db))

(re-frame/register-handler
  :initialize-db
  (fn [_ _]
    {:phones []
     :phone-details {}
     :search-input ""
     :order-prop "name"}))

;; (re-frame/register-handler
;;   :debug-state
;;   (fn [db _]
;;     (js/console.log (str "DEBUG: " db))
;;     db))

;; -------------------------
;; Views

(defn search-component
  []
  (let [search-input (re-frame/subscribe [:search-input])])
  (fn []
    [:input {:on-change #(re-frame/dispatch [:search-input-entered (-> % .-target .-value)])}]))

(defn order-prop-component
  []
  (let [order-prop (re-frame/subscribe [:order-prop])]
  (fn []
    [:div "Sort by: "
     [:select {:value @order-prop :on-change #(re-frame/dispatch [:order-prop-changed (-> % .-target .-value)])}
      [:option {:value "name"} "Alphabetical"]
      [:option {:value "age"} "Newest"]]])))

(defn phone-component
  [phone]
  [:li {:class "thumbnail phone-listing"}
   [:a {:href (str "#/phones/" (:id phone))
        :class "thumb"}
    [:img {:src (:imageUrl phone)}]]
   [:a {:href (str "#/phones/" (:id phone))} (:name phone)]
   [:p (:snippet phone)]])

(defn- matches-query?
  [search-input phone]
  (if (= "" search-input)
    true
    (boolean (or
               (re-find (re-pattern search-input) (:name phone))
               (re-find (re-pattern search-input) (:snippet phone))))))

(defn phones-component
  []
  (let [phones (re-frame/subscribe [:phones])
        search-input (re-frame/subscribe [:search-input])
        order-prop (re-frame/subscribe [:order-prop])]
    (fn []
      [:ul {:class "phones"}
       (for [phone (->> @phones
                        (filter (partial matches-query? @search-input))
                        (sort-by (keyword @order-prop))
                        )]
         ^{:key (:name phone)} [phone-component phone])
       ])))

(defn home-page []
  [:div
   [search-component]
   [order-prop-component]
   [phones-component]]
  )

(defn current-page []
  [:div [(session/get :current-page) (session/get :params)]])

;; -------------------------
;; Phone details views

(defn phone-info-template
  [section-title attributes-map]
  [:li
   [:span section-title]
   [:dl
    (map (fn [attribute]
           ^{:key (:name attribute)} [:div
                                      [:dt (:name attribute)]
                                      [:dd (condp = (:value attribute)
                                                   true "\u2713"
                                                   false "\u2718"
                                                   (:value attribute))]])
         attributes-map)]])

(defn thumbnails
  [phone]
  [:ul {:class "phone-thumbs"}
   (for [image (:images @phone)]
     ^{:key image} [:li [:img {:src image
                               :class "phone"
                               :on-click #(re-frame/dispatch [:set-image [(:id @phone) image]])}]])])

(defn availability
  [availability]
  [:li
   [:span "Availability and Networks"]
   [:dl
    [:dt "Availability"]
    (for [availability @availability]
      availability)]])

(defn battery
  [battery]
  [phone-info-template "Battery" [{:name "Type"
                                   :value (:type @battery)}
                                  {:name "Talk Time"
                                   :value (:talkTime @battery)}
                                  {:name "Standby time (max)"
                                   :value (:standbyTime @battery)}]])
(defn storage-and-memory
  [storage]
  [phone-info-template "Storage And Memory"  [{:name "RAM"
                                               :value (:ram @storage)}
                                              {:name "Internal Storage"
                                               :value (:flash @storage)}]])

(defn connectivity
  [connectivity]
  [phone-info-template "Connectivity" [{:name "Network Support"
                                        :value (:cell @connectivity)}
                                       {:name "Wifi"
                                        :value (:wifi @connectivity)}
                                       {:name "Bluetooth"
                                        :value (:bluetooth @connectivity)}]])

(defn android
  [android]
  [phone-info-template "Android" [{:name "OS Version"
                                   :value (:os @android)}
                                  {:name "UI"
                                   :value (:ui @android)}]])

(defn size-and-weight
  [size-and-weight]
  [phone-info-template "Size And Weight" [{:name "Dimensions"
                                           :value (:dimensions @size-and-weight)}
                                          {:name "Weight"
                                           :value (:weight @size-and-weight)}]])

(defn display
  [display]
  [phone-info-template "Display" [{:name "Screen size"
                                   :value (:screenSize @display)}
                                  {:name "Screen resolution"
                                   :value (:screenResolution @display)}
                                  {:name "Touch screen"
                                   :value (:touchScreen @display)}]])

(defn hardware
  [hardware]
  [phone-info-template "Hardware" [{:name "CPU"
                                    :value (:cpu @hardware)}
                                   {:name "USB"
                                    :value (:usb @hardware)}
                                   {:name "Audio / headphone jack"
                                    :value (:audioJack @hardware)}
                                   {:name "FM Radio"
                                    :value (:fmRadio @hardware)}
                                   {:name "Accelerometer"
                                    :value (:accelerometer @hardware)}]])

(defn camera
  [camera]
  [phone-info-template "Camera" [{:name "Primary"
                                  :value (:primary @camera)}
                                 {:name "Features"
                                  :value (clojure.string/join ", " (:features @camera))}]])

(defn additional-features
  [additional-features]
  [:li
   [:span "Additional Features"]
   [:dd @additional-features]])

(defn specs
  [phone]
  [:ul {:class "specs"}
   [availability (reaction (:availability @phone))]
   [battery (reaction (:battery @phone))]
   [storage-and-memory (reaction (:storage @phone))]
   [connectivity (reaction (:connectivity @phone))]
   [android (reaction (:android @phone))]
   [display (reaction (:display @phone))]
   [hardware (reaction (:hardware @phone))]
   [camera (reaction (:camera @phone))]
   [additional-features (reaction (:additionalFeatures @phone))]])

(defn phone-page [{phone-id :phone-id}]
  (let [phone (re-frame/subscribe [:phone-query phone-id])
        image-url (re-frame/subscribe [:selected-image-url phone-id])]
    (fn []
      [:div
       [:img {:src @image-url
              :class "phone"}]
       [:h1 (:name @phone)]
       [:p (:description @phone)]
       [thumbnails phone]
       [specs phone]])))

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/phones" []
                    (session/put! :current-page #'home-page))

(secretary/defroute "/phones/:phone-id" {:as params}
                    (session/put! :current-page #'phone-page)
                    (session/put! :params params)
                    (re-frame/dispatch [:load-phone-detail (:phone-id params)]))

(defn- redirect-to
  [resource]
  (secretary/dispatch! resource)
  (.setToken (History.) resource))

(secretary/defroute "*" []
                    (redirect-to "/phones"))


 ;; -------------------------
 ;; History
 ;; must be called after routes have been defined
 (defn hook-browser-navigation! []
   (doto (History.)
     (events/listen
      EventType/NAVIGATE
      (fn [event]
        (secretary/dispatch! (.-token event))))
     (.setEnabled true)))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
;;   (accountant/configure-navigation!)
;;   (accountant/dispatch-current!)
  (re-frame/dispatch [:initialize-db])
  (re-frame/dispatch [:load-phones])
  (mount-root))

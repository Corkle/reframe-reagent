(ns phonecat-re-frame.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [re-frame.core :as re-frame])
  (:require-macros [reagent.ratom :refer [reaction]]))

;; -------------------------
;; Re-Frame Data

(defn handle-search-input-entered
  [db [_ search-input]]
  (assoc-in db [:search-input] search-input))

(re-frame/register-sub
  :search-input
  (fn [db]
    (reaction (:search-input @db))))

(re-frame/register-sub
  :phones
  (fn [db]
    (reaction (:phones @db))))

(re-frame/register-handler
  :search-input-entered
  handle-search-input-entered)

(re-frame/register-handler
  :initialize-db
  (fn [_ _]
    {:phones [{:name "Nexus S" :snippet "Fast just got faster with Nexus S."}
              {:name "Motorola XOOM™ with Wi-Fi" :snippet "The Next, Next Generation tablet."}
              {:name "Motoral Xoom" :snippet "The Next, Next Generation tablet."}]
     :search-input ""}))

;; -------------------------
;; Views

(defn search-component
  []
  (let [search-input (re-frame/subscribe [:search-input])])
  (fn []
    [:input {:on-change #(re-frame/dispatch [:search-input-entered (-> % .-target .-value)])}]))

(defn phone-component
  [phone]
  [:li
   [:span (:name phone)]
   [:p (:snippet phone)]])

(defn matches-query?
  [search-input phone]
  (if (= "" search-input)
    true
    (boolean (or
               (re-find (re-pattern search-input) (:name phone))
               (re-find (re-pattern search-input) (:snippet phone))))))

(defn phones-component
  []
  (let [phones (re-frame/subscribe [:phones])
        search-input (re-frame/subscribe [:search-input])]
    (fn []
      [:ul {:class "phones"}
       (for [phone (filter (partial matches-query? @search-input) @phones)]
         ^{:key phone} [phone-component phone])])))

(defn home-page []
  [:div
   [search-component]
   [phones-component]]
  )

(defn about-page []
  [:div [:h2 "About phonecat-re-frame"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
                    (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
                    (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (re-frame/dispatch [:initialize-db])
  (mount-root))

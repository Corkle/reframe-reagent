(ns phonecat-re-frame.prod
  (:require [phonecat-re-frame.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)

(ns slate.filetypes.export.html
  (:require-macros [hiccups.core :as hiccups :refer [html]]))

(defn doc->html
  "Converts a droplet document to an HTML "
  [droplet-doc]
  (html [:html
         [:head]
         [:body
          ;; TODO: parse document and export HTML
          [:p "Hello!"]]]))

(comment
  (doc->html nil)
  )
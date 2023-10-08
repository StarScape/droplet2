(ns drop.app.components.library
  (:require [clojure.string :as str]
            ["tailwind-merge" :refer [twMerge]]))

(defn merge-opts [{class1 :class :as opts1} {class2 :class :as opts2}]
  (letfn [(stringify [classes]
            (cond
              (string? classes) classes
              (sequential? classes) (str/join " " (filter some? classes))
              :else ""))]
   (assoc (merge opts1 opts2)
          :class (twMerge (stringify class1) (stringify class2)))))

(defn button [opts children]
  [:button (merge-opts {:on-mouse-down #(.preventDefault %)
                        :class ["m-0.5" "p-2" "rounded-md" "rounded-sm" "hover:bg-light-blue" "dark:hover:bg-dark-blue" "active:bg-slate-400"]}
                       opts)
   children])

(defn toggleable-button [{:keys [toggled?] :as opts} children]
  ;; Possible macro to make this a bit more ergonomic:
  ;;
  ;; ```
  ;; (let-rest {:keys [toggled?] :& opts}
  ;;   ;; inside this block, toggled is destructured but opts = (dissoc opts :toggled)
  ;;   )
  ;; ```
  [button (merge-opts {:class [(when toggled? "bg-light-blue dark:bg-dark-blue")]}
                      (dissoc opts :toggled?))
   children])

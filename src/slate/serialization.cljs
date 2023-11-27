(ns slate.serialization
  (:require [clojure.edn :as edn]
            [slate.model.history :as history]
            [slate.model.editor-state :as es :refer [map->EditorState map->EditorUpdate]]
            [slate.model.doc :refer [map->Document]]
            [slate.model.paragraph :refer [map->Paragraph]]
            [slate.model.run :refer [map->Run]]
            [slate.model.selection :as sel :refer [map->Selection]]
            [slate.model.dll :refer [dll]]))

(def current-version
  "Current version of Droplet's .drop file format. If you open an older versioned
   format, Droplet will attempt to automatically migrate it to the newest version."
  2)

(def slate-types-readers
  {'slate.model.selection.Selection map->Selection
   'slate.model.run.Run map->Run
   'slate.model.paragraph.Paragraph map->Paragraph
   'slate.model.doc.Document map->Document
   'slate.model.editor-state.EditorState map->EditorState
   'slate.model.editor-state.EditorUpdate map->EditorUpdate
   'DoublyLinkedList #(apply dll %)})

;; TODO: write unit tests for migrations
(def migrations
  "Map of migration funcs, where each key is the version that we are migrating to,
   from the previous version.

   So for example, the function under 2 will migrate from version 1 to version 2."
  {2 (fn [drop-file-ver-1]
       {:editor-state (history/current-state (:history drop-file-ver-1))})})

(defn migrate
  [{:keys [version] :as deserialized-data}]
  (if (= version current-version)
    deserialized-data
    ;; Migrate one version at a time until reach current version
    (let [versions (range (inc version) (inc current-version))
          migrations (->> (select-keys migrations versions)
                          (into (sorted-map))
                          (vals))]
      (reduce (fn [drop-file migration-fn]
                (update (migration-fn drop-file) :version inc))
              deserialized-data migrations))))

(defn serialize
  "Serializes the editor-state object to EDN, to be saved in a .drop file."
  [{:keys [history] :as _ui-state}]
  ; previously was saving whole history (and eventually will be again),
  ; hence why it is passed the history object instead of just editor-state
  (prn-str {:version current-version, :editor-state (history/current-state history)}))

(defn deserialize
  "Parses the EDN of the saved .drop file and returns the data structure."
  [edn-str]
  (-> (edn/read-string {:readers slate-types-readers} edn-str)
      (migrate)))

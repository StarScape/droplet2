(ns slate.serialization
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [slate.model.history :as history]
            [slate.model.editor-state :as es :refer [map->EditorState map->EditorUpdate]]
            [slate.model.doc :as doc :refer [map->Document]]
            [slate.model.paragraph :refer [map->Paragraph]]
            [slate.model.run :refer [map->Run]]
            [slate.model.selection :as sel :refer [map->Selection]]
            [slate.model.dll :as dll :refer [dll]]))

(def current-version
  "Current version of Droplet's .drop file format. If you open an older versioned
   file, Droplet will attempt to automatically migrate it to the newest version."
  3)

(s/def ::version #(= % current-version))
(s/def ::editor-state ::es/editor-state)
(s/def ::drop-file (s/keys :req-un [::version
                                    ::editor-state]))

(def slate-types-readers
  "Readers for each version of the .drop file format.
   It's necessary to keep the readers for each separate version around since sometimes what types are read
   (or _how_ they're read) changes from version to version. If there is no reader set specified for a version,
   then the most recent version number before it is the one used."
  {1 {'slate.model.selection.Selection map->Selection
      'slate.model.run.Run map->Run
      'slate.model.paragraph.Paragraph map->Paragraph
      'slate.model.doc.Document map->Document
      'slate.model.editor-state.EditorState map->EditorState
      'slate.model.editor-state.EditorUpdate map->EditorUpdate
      'DoublyLinkedList #(apply dll %)}
   3 {'slate.model.selection.Selection #(-> (map->Selection %)
                                            (update-in [:start :paragraph] dll/big-dec)
                                            (update-in [:end :paragraph] dll/big-dec))
      'slate.model.run.Run map->Run
      'slate.model.paragraph.Paragraph map->Paragraph
      'slate.model.doc.Document map->Document
      'slate.model.editor-state.EditorState map->EditorState
      'slate.model.editor-state.EditorUpdate map->EditorUpdate
      'DoublyLinkedList #(dll/from-indexed-items %)
      'Decimal dll/big-dec}})

(defn- readers-for-version [version]
  (get slate-types-readers (->> (keys slate-types-readers)
                                (filter #(<= % version))
                                (apply max))))

;; TODO: write unit tests for migrations
(def migrations
  "Map of migration funcs, where each key is the version that we are migrating to,
   from the previous version.

   So for example, the function under 2 will migrate from version 1 to version 2."
  {2 (fn [v1-file]
       {:editor-state (history/current-state (:history v1-file))})
   3 (fn [{:keys [editor-state] :as v2-file}]
       (let [{:keys [selection doc]} editor-state
             paragraphs-numbered (map-indexed vector (:children doc))
             new-sel-start-idx (->> paragraphs-numbered
                                    (filter #(= (-> selection :start :paragraph) (:uuid (second %))))
                                    (ffirst)
                                    (inc) ; recall that DLL indices start at 1 by default
                                    (dll/big-dec))
             new-sel-end-idx (->> paragraphs-numbered
                                  (filter #(= (-> selection :end :paragraph) (:uuid (second %))))
                                  (ffirst)
                                  (inc)
                                  (dll/big-dec))
             new-selection (map->Selection {:start {:paragraph new-sel-start-idx
                                                    :offset (-> selection :start :offset)}
                                            :end {:paragraph new-sel-end-idx
                                                  :offset (-> selection :end :offset)}
                                            :backwards? (:backwards? selection)
                                            :formats #{}})
             new-doc (->> (:children doc)
                          (map #(dissoc % :uuid))
                          (apply dll)
                          (doc/document))]
         (assoc v2-file :editor-state (es/editor-state new-doc new-selection))))})

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

(defn validate
  "Validates that the deserialized .drop matches expectations, and throws an error if it does not."
  [deserialized-data]
  (if (s/valid? ::drop-file deserialized-data)
    deserialized-data
    (throw (js/Error. ".drop file did not match the ::drop-file spec"))))

(defn serialize
  "Serializes the editor-state object to EDN, to be saved in a .drop file."
  [{:keys [history] :as _ui-state}]
  ; previously was saving whole history (and eventually will be again),
  ; hence why it is passed the history object instead of just editor-state
  (prn-str {:version current-version, :editor-state (history/current-state history)}))

;; TODO: there should be a spec for the deserialized file.
;; If the deserialized file does not match the spec, error out gracefully.
(defn deserialize
  "Parses the EDN of the saved .drop file and returns the data structure."
  [edn-str]
  (try
    (let [version (js/parseInt (aget (.exec #":version (\d+)" edn-str) 1))
          readers (readers-for-version version)]
      (if (<= version current-version)
        ;; ⬇ The Happy Path (TM) ⬇ ;;
        (-> (edn/read-string {:readers readers} edn-str)
            (migrate)
            (validate))
        {:error-message "Droplet cannot read the file as it is designed for a newer version of the application."
         :exception nil}))
    (catch js/Error e
      {:error-message "Droplet could not read the file."
       :exception e})))

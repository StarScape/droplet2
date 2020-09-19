(ns drop.editor.vec-utils)

(defn replace-range
  "Replaces contents of `v` between indexes `start` and `end` (both inclusive) with the contents of `items`.
   If `end` if omitted it will replace everything after `start`.
   Optionally, `items` may also be a single value.

   Examples:
   ```
   (replace-range [1 2 3 4] 1 2 [:a :b :c]) ;; [1 :a :b :c 4]
   (replace-range [1 2 3 4] 2 [:a :b :c]) ;; [1 2 :a :b :c]
   ```"
  ([v start end items]
   (vec (concat (subvec v 0 start)
                (if (sequential? items) items [items])
                (subvec v (inc end) (count v)))))
  ([v start items]
   (replace-range v start (dec (count v)) items)))

(comment
  (replace-range [1 2 3 4] 1 2 [:a :b])
  (replace-range [1 2 3 4] 1 [:a :b])
  (replace-range [1 2 3 4] 1 :a)
  (replace-range [1 2 3 4] 1 1 [:two]))


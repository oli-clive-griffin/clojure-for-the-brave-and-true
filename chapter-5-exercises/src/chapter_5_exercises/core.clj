(ns chapter-5-exercises.core)

;; You used (comp :intelligence :attributes) to create a function that returns a character’s intelligence. Create a new function, attr, that you can call like (attr :intelligence) and that does the same thing.
(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(def c-int (comp :intelligence :attributes))
(defn attr [attribute]
  (fn [character] ((comp attribute :attributes) character)))
(c-int character)
((attr :strength) character)


;; Implement the comp function.
(defn my-comp [& funcs]
  (fn [& args]
    (let [ordered-funcs (reverse funcs)
          first-func (first ordered-funcs)
          rest-funcs (rest ordered-funcs)]
      (reduce (fn [acc current-func]
                (current-func acc))
              (apply first-func args)
              rest-funcs))))


;; Implement the assoc-in function. Hint: use the assoc function and define its parameters as [m [k & ks] v].
(assoc-in character [:attributes :strength] 2)
;; (defn my-assoc-in
;;   [m [k & ks] val]
;;   (if (empty? ks)
;;     (my-assoc-in (get m k))) )

(ns-name *ns*)

(ns-interns *ns*)
(ns-map *ns*)
->Eduction







;; Look up and use the update-in function.

;; Implement update-in.

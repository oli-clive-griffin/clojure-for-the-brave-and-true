(ns fwpd.core)
(require '[clojure.string :as s])
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

;; (def str->int Integer.) 
(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string  #"\n")))

(parse (slurp filename))

(defn mapify
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn simple-mapify
  [rows]
  (map (fn [[name i]]
         {:name name
          :glitter-index (Integer. i)})
       rows))

(mapify (parse (slurp filename)))
(simple-mapify (parse (slurp filename)))

(defn glitter-filter
  [vamps low-limit]
  (map :name
       (filter #(> (get % :glitter-index) low-limit)
               vamps)))

(glitter-filter (mapify (parse (slurp filename)))
                6)
(+ 1 2 3 4 5 6 7 8 9)

(loop [i 0]
  (if (= i 3)
    (recur (+ i 1))
    (println i)))

(def list-o-maps (mapify (parse (slurp filename))))
list-o-maps

(defn list->csv
  [vamp-maps]
  (s/join "\n"
          (map (fn [vamp-map]
                 (s/join ", "
                         (let [keys [:name :glitter-index]]
                           (map (fn [key]
                                  (get vamp-map key))
                                keys))))
               vamp-maps)))

(let [text (list->csv (mapify (parse (slurp filename))))]
  (spit "test.csv" text))

(ns clojure-sandbox.core
  (:gen-class))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn mapset
  "like map but returns a set"
  [f list]
  (let [list (set list)]
    (loop [x (first list)
           xs (rest list)
           finished []]
      (if (empty? xs)
        (conj finished (f x))
        (recur (first xs)
               (rest xs)
               (conj finished (f x)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (mapset inc (first args)))

(-main [2 7 3])

'(1 2 3)

(defn pos-power
  [base exp]
  (reduce * 1 (repeat exp base)))

(pos-power 3 0)


(defn sum [xs] (reduce + xs))
(defn avg [xs] (/ (sum xs) (count xs)))

(map (fn [f]
       (f [1 2 3 4 5 6]))
     '(sum
       avg
       #(apply max %)
       first
       last))

(def langs {:clojure 1/10
            :js 3/10
            :haskell 1/10})
(map second langs)

(reduce (fn
          [final [key val]]
          (assoc final key (inc val)))
        {}
        {:max 30 :min 10})


(drop-while #(< % 4) '(1 2 3 4 5 6 8 9))
(drop-while #(< % 4) '(1 2 3 4 5 6 7 8 9))

(def vampire-database
  {0 {:makes-blood-puns? false, :has-pulse? true  :name "McFishwich"}
   1 {:makes-blood-puns? false, :has-pulse? true  :name "McMackson"}
   2 {:makes-blood-puns? true,  :has-pulse? false :name "Damon Salvatore"}
   3 {:makes-blood-puns? true,  :has-pulse? true  :name "Mickey Mouse"}})

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 1000)
  (get vampire-database social-security-number))

(defn vampire?
  [record]
  (and (:makes-blood-puns? record)
       (not (:has-pulse? record))
       record))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire?
                 (map vampire-related-details social-security-numbers))))

(time (vampire-related-details 0))

(time (def mapped-details (map vampire-related-details (range 0 1000000))))
(time (first mapped-details))

(defn threes
  ([]
   (threes 3))
  ([i]
   (cons i (lazy-seq (threes (+ i 3))))))

(threes)

(take 10 (threes))


;; Conj and into

(into {} (map identity {:a 1 :b 2}))

(defn my-conj
  [xs & items]
  (into xs items))
(my-conj [:a :d] 1 2 3)

(defn my-into
  [xs additions]
  (apply conj xs additions))
(my-into [3 4] [:a :b])


;; Partial
;; closest thing to haskell currying

(def add-3-4-5-and-2 (partial + 3 4 5 2))
(add-3-4-5-and-2 4)

(def inc-all (partial map inc))
(inc-all [1 2 3])

((partial (partial + 1) 1) 1)
(partial (partial (partial map) inc) [1 2 3])

;; is this a code smell?
(defn create-warning
  [message]
  (str "WARNING: " message))
(create-warning "red ahead!")

(def create-warning-partial
  (partial str "WARNGING: "))
(create-warning-partial "Blue Ahead!!!!")

(filter (complement even?)
        [1 2 3 4 5 6])


;; ----------------------------------------------------------------------------------------------------------------------------------

(reduce  (fn [acc person]
           (+ acc (get person :age)))
         0
         [{:age 3}
          {:age 90}
          {:age 23}])

(defn my-sum
  ([vals] 
   (my-sum vals 0))
  ([vals acc]
   (if ((complement empty?) vals)
     (recur (rest vals)
             (+ acc (first vals)))
     acc)))

(my-sum [1 2 3 4 5 6 7 8 9])

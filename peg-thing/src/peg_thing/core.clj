(ns peg-thing.core)
(require '[clojure.set :as set])
(declare successful-move prompt-move game-over query-rows)


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main
  [& args]
  (foo ""))

(defn tri*
  "generates a lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum
                                   (inc n)))))))
(def tri (tri*))


(defn triangular? [n]
  (= n (last (take-while
              #(>= n %)
              tri))))

(defn row-tri [n]
  (last (take n tri)))

(defn my-row-tri
  "uses a more geometric reasonning instead of procedural"
  [n]
  (/ (* n (inc n))
     2))

(my-row-tri 5)

(defn row-num [n]
  (inc (count (take-while #(> n %) tri))))

(defn connect
  "form mutal connection between two postitions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))
;; (connect {} (my-row-tri 6) 5 9 14)


(defn connect-down-left
  [board max-pos pos]
  (let [neighbor (+ pos (row-num pos))
        target-pos (+ neighbor (row-num neighbor))]
    (connect board max-pos pos neighbor target-pos)))
;; (connect-down-left {} 15 4)

(defn connect-down-right
  [board max-pos pos]
  (let [neighbor (+ 1 pos (row-num pos))
        target-pos (+ 1 neighbor (row-num neighbor))]
    (connect board max-pos pos neighbor target-pos)))
;; (connect-down-right {} 15 4)

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? pos) (triangular? neighbor))
      (connect board max-pos pos neighbor destination)
      board)))
;; (connect-right {} 15 4)

(defn add-pos
  [board max-pos pos]
  (let [new-board (assoc-in board [pos :pegged] true)]
    (if (<= pos max-pos)
      (reduce (fn [new-board connect-func]
                (connect-func new-board max-pos pos))
              new-board
              [connect-down-right connect-down-left connect-right])
      board)))
;; (let [board {}] (add-pos board 15 1))

(defn create-board
  [rows]
  (let [max-pos (row-tri rows)
        starter-board {:rows rows}]
    (reduce (fn [board, pos]
              (add-pos board max-pos pos))
            starter-board
            (range 1 (inc max-pos)))))
(create-board 200)

;; ----

(def memod-create-board (memoize create-board))
(memod-create-board 14)
(spit "14-layer-board.txt" (memod-create-board 14))

(def test-board (memod-create-board 6))

;; ----

(defn pegged?
  [board pos]
  (get-in board [pos :pegged]))
;; (pegged? test-board 9)

(defn remove-peg
  [board pos]
  (assoc-in board [pos :pegged] false))
;; (pegged? (remove-peg test-board 9) 9)

(defn remove-pegs
  [board positions]
  (let [initial-board board]
    (reduce (fn [board pos]
              (remove-peg board pos))
            initial-board
            positions)))

(defn place-peg
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))
(move-peg (remove-peg test-board 5) 12 5)

(defn valid-moves
  [board pos]
  (into {}
        (filter (fn [[dest jumped]]
                  (and (not (pegged? board dest))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))
;; (valid-moves (remove-peg test-board 5) 12)
;; (valid-moves (remove-pegs test-board [4 6 15]) 13)

(defn valid-move?
  [board p1 p2]
  (get (valid-moves board p1) p2))
;; (valid-move? (remove-pegs test-board [4 6 15]) 6 4)

(defn make-move
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)
    nil))
(make-move (remove-pegs test-board [4 6 15]) 13 9)

(defn can-move?
  [board]
  (some (fn [x] (not-empty (valid-moves board x)))
        (map first (filter #(get (second %) :pegged) board))))
(can-move? (remove-pegs test-board [4 6 15]))

;; ----

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(flatten (repeat 2 [1 2 3]))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         "0"
         "-")))

(defn row-positions
  "returns all positions in given row"
  [row-num]
  (range (inc (- (row-tri row-num) row-num))
         (inc (row-tri row-num))))
(row-positions 5)

(defn row-padding
  "string of spaces to add to beginning or row to center it"
  [row-num rows]
  (let [pad-length (* 2 (- rows row-num))]
    (apply str (repeat pad-length " "))))
(str (row-padding 1 9) "d")

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join "  " (map (partial render-pos board)
                                      (row-positions row-num)))))
(render-row (remove-peg test-board 4) 3)


(defn render-board
  [board]
  (reduce (fn [final-string row-num]
            (str final-string
                 (render-row board row-num)
                 "\n"))
          ""
          (range 1 (inc (:rows board)))))
(spit "test.txt" (render-board (remove-pegs (create-board 6) [4 5 13])))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))
;; (print-board (remove-pegs test-board [1 5 9]))

(defn letter->pos
  [letter]
  (inc (- (int (first letter)) alpha-start)))
(letter->pos "c")

(defn get-input
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))
(get-input)

(defn characters-as-strings
  [str]
  (filter
   (fn [char] (Character/isLetter char))
   str))
(characters-as-strings "adsf335 2z")

(defn user-entered-valid-move
  [board]
  (println "invalid move")
  (prompt-move board))

(defn user-entered-invalid-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn prompt-move
  [board]
  (println "\nThe board:")
  (print-board board)
  (println "what's your move? enter two letters")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn prompt-empty-peg
  [board]
  (println "here's your board")
  (print-board board)
  (println "what peg would you like to remove")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "how many rows")
  (let [rows (Integer. (get-input 5))
        board (create-board rows)]
    (prompt-empty-peg board)))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "game over, you had " remaining-pegs " pegs remaining")
    (print-board board)
    (println "would you like to play again? [y/y]")
    (let [input (get-input "y")]
      (if (= input "y")
        (prompt-rows)
        (do (println "Bye!")
            (System/exit 0))))))

(prompt-rows)
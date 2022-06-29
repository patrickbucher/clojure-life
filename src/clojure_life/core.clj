(ns clojure-life.core
  (:gen-class))

(def neighbours
  {:north [-1 0]
   :north-east [-1 1]
   :east [0 1]
   :south-east [1 1]
   :south [1 0]
   :south-west [1 -1]
   :west [0 -1]
   :north-west [-1 -1]})

(defn create-grid
  "Creates a two-dimensional vector with defined rows and cols; all values set to value."
  [rows cols value]
  (when (and (>= rows 0) (>= cols 0))
    (into [] (take rows (repeat (into [] (take cols (repeat value))))))))

(defn is-in-range
  "Checks whether or not the given row/col index is within the grid's limits."
  [grid row col]
  (and (>= row 0) (>= col 0) (< row (count grid)) (< col (count (get grid 0)))))

(defn get-at
  "Gets the state at the index row/col of the grid."
  [grid row col]
  (when (is-in-range grid row col)
    (get (get grid row) col)))

(defn set-at
  "Sets state at the index row/col of the grid."
  [grid row col state]
  (if (is-in-range grid row col)
    (assoc grid row (assoc (get grid row) col state))
    grid))

(defn print-row
  "Prints a single row; cols separated by space."
  [row]
  (loop [c 0
         cols (count row)]
    (when (< c cols)
      (print (str (get row c) " "))
      (recur (inc c) cols))))

(defn print-grid
  "Prints the entire grid; rows separated by newline."
  [grid]
  (loop [r 0
         rows (count grid)]
    (when (< r rows)
      (print-row (get grid r))
      (print "\n")
      (recur (inc r) rows))))

;; TODO: count neighbours
;; TODO: compute next generation

(defn -main
  "Prints a grid of 8x8."
  ;; TODO: get args from command line
  [& args]
  (print-grid (create-grid 8 8 "_")))

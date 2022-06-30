(ns clojure-life.core
  (:gen-class))

(defn as-2d-vec [grid] (vec (map vec grid)))

(defn new-grid
  "Creates a two-dimensional vector with defined rows and cols; all values set to value."
  [rows cols value]
  (when (and (>= rows 0) (>= cols 0))
    (as-2d-vec (partition cols (take (* rows cols) (repeat value))))))

(defn in-range?
  "Checks whether or not the given row/col index is within the grid's limits."
  [grid row col]
  (and (>= row 0) (>= col 0) (< row (count grid)) (< col (count (get grid 0)))))

(defn get-at
  "Gets the state at the index row/col of the grid."
  [grid row col]
  (when (in-range? grid row col)
    (get (get grid row) col)))

(defn set-at
  "Sets state at the index row/col of the grid."
  [grid row col state]
  (if (in-range? grid row col)
    (assoc grid row (assoc (get grid row) col state))
    grid))

(defn coords-of
  "Creates a two-dimensional vector of indices (row/col pairs)."
  [grid]
  (let [rows (count grid)
        cols (count (get grid 0))]
    (as-2d-vec (partition cols (for [r (range rows) c (range cols)] [r c])))))

(def directions
  "The 8 directions (horizontal, vertical, diagonal neighbours)."
  {:north [-1 0]
   :north-east [-1 1]
   :east [0 1]
   :south-east [1 1]
   :south [1 0]
   :south-west [1 -1]
   :west [0 -1]
   :north-west [-1 -1]})

(defn neighbours-of
  "Returns the neighbour coordinates of the given cell."
  [grid row col]
  (let [rows (count grid)
        cols (count (get grid 0))]
    (map (fn [[dr dc]]
           (let [nr (mod (+ row dr) rows)
                 nc (mod (+ col dc) cols)]
             [(if (neg? nr) (dec rows) nr)
              (if (neg? nc) (dec cols) nc)]))
         (vals directions))))

(defn neighbour-vals-of
  "Gets the values of the neighbours of the given cell."
  [grid row col]
  (let [neighbours (neighbours-of grid row col)]
    (map (fn [[r c]] (get-at grid r c)) neighbours)))

(defn neighbours-alive
  "Counts the number of alive neighbours of the given cell."
  [grid row col alive-state]
  (count (filter (fn [state] (= state alive-state)) (neighbour-vals-of grid row col))))

(defn next-state
  "Computes the next state of an individual cell."
  [grid row col alive-state dead-state]
  (let [state (get-at grid row col)
        n-alive (neighbours-alive grid row col alive-state)]
    (if (= state alive-state)
      (if (<= 2 n-alive 3) alive-state dead-state)
      (if (= n-alive 3) alive-state dead-state))))

(defn next-generation
  "Computes the next generation as a two-dimensional vector."
  [grid alive-state dead-state]
  (let [rows (count grid)
        cols (count (get grid 0))
        cds (apply concat (coords-of grid))]
    (as-2d-vec
     (partition cols (map (fn [[r c]] (next-state grid r c alive-state dead-state)) cds)))))

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
      (recur (inc r) rows)))
  (println))

(defn add-glider
  "Adds a glider one off the top-left corner."
  [grid alive-state]
  (set-at (set-at (set-at (set-at (set-at grid 1 2 alive-state) 2 3 alive-state) 3 1 alive-state) 3 2 alive-state) 3 3 alive-state))

(defn add-f-pentomino
  "Adds an f-Pentomino roughly into the middle of the grid."
  [grid alive-state]
  (let [rows (count grid)
        cols (count (get grid 0))
        pent [[false true true]
              [true true false]
              [false true false]]
        midr (/ rows 2)
        midc (/ cols 2)
        activate-rel-cds (filter (fn [[r c]] (get-at pent r c)) (coords-of pent))
        activate-abs-cds (map (fn [[r c]] [(+ r midr) (+ c midc)]) activate-rel-cds)]
    (println activate-rel-cds)))
;    (apply (fn [[r c]] (set-at grid r c alive-state)) activate-abs-cds)))

(defn -main
  "Runs the simulation."
  [& args]
  (loop [grid (add-f-pentomino (add-glider (new-grid 64 64 "_") "x") "x")]
    (print-grid grid)
    (Thread/sleep 250)
    (recur (next-generation grid "x" "_"))))

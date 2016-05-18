(defn empty-board
  "Creates a rectangular empty board of the specified width
  and height."
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  "Turns :on each of the cells specified as [y, x] coordinates."
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(pprint glider)

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))
(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc)))) 

(defn indexed-step
  "Yields the next state of the board, using indices to determine neighbors,
  liveness, etc."
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board [x y]]
       (let [new-liveness
             (case (count-neighbours board [x y])
               2 (get-in board [x y])
               3 :on
               nil)]
         (assoc-in new-board [x y] new-liveness)))
     board (for [x (range h) y (range w)] [x y]))))

(defn window
  "Returns a lazy sequence of 3-item windows centered
  around each item of coll, padded as necessary with
  pad or nil"
  ([coll] (window nil coll))
  ([pad coll]
  (partition 3 1 (concat [pad] coll [pad]))))

(defn cell-block
  "Creates a sequences of 3x3 windows from a triple of 3 sequences."
  [[left mid right]]
  (window (map vector left mid right)))

(defn liveness
  "Returns the liveness (nil or :on) of the center cell for
  the next step."
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block)))
             (if (= :on center) 1 0))
      2 center
      3 :on
      nil)))

(defn- step-row
  "Yields the next state of the center row."
  [rows-triple]
  (vec (map liveness (cell-block rows-triple))))

(defn index-free-step
  "Yields the next state of the board."
  [board]
  (vec (map step-row (window (repeat nil) board))))

(= (nth (iterate indexed-step glider) 8)
   (nth (iterate index-free-step glider) 8))

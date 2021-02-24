(ns santorini-player.core)

(declare within-one?
         movable-height?
         get-valid-builds
         get-valid-moves
         update-players)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn possible-moves
  [])

(defn piece-moves
  [board piece]
  (let [level (get-in board piece)
        ]
    (println level)
    (keep-indexed #(:when (within-one? level %2) %1) board)
    ))

(defn take-turn
  [board players turn]
  (let [res (for [pos (range 2)
                   move (get-valid-moves board (get-in players [0 pos]) players)
                   build (get-valid-builds board move (update-players players pos move))]
              [pos move build])
        ]
    (vec res)
    )
  )

(defn update-players
  [players pos newloc]
  (println (assoc-in players [0 pos] newloc))
  (assoc-in players [0 pos] newloc))

(defn get-valid-moves
  [board piece players]
  (let [xpos (dec (get piece 0))
        ypos (dec (get piece 1))
        player1 (get players 0)
        player2 (get players 1)
        level (get-in board [xpos ypos])
        vals (for [[x row] (map-indexed vector board)
                   [y val] (map-indexed vector row)
                   :when (and (movable-height? level val)
                              (within-one? x xpos)
                              (within-one? y ypos)
                              (or (not= x xpos) (not= y ypos))
                              (not (some #(= [(inc x) (inc y)] %) player1))
                              (not (some #(= [(inc x) (inc y)] %) player2))
                              true)]
               [(inc x) (inc y)])]
    (vec vals)))

;;
;; MUST HAVE THE CORRECT PIECES
(defn get-valid-builds
  [board piece players]
  (println piece players)
    (let [xpos (dec (get piece 0))
        ypos (dec (get piece 1))
        player1 (get players 0)
        player2 (get players 1)
        vals (for [[x row] (map-indexed vector board)
                   [y val] (map-indexed vector row)
                   :when (and (< val 4)
                              (within-one? x xpos)
                              (within-one? y ypos)
                              (or (not= x xpos) (not= y ypos))
                              (not (some #(= [(inc x) (inc y)] %) player1))
                              (not (some #(= [(inc x) (inc y)] %) player2))
                              true)]
               [(inc x) (inc y)])]
    (vec vals))
  )



(defn within-one?
  [num1 num2]
  ;; (println num1 num2)
  (not (> (Math/abs (- num1 num2)) 1))
  )

(defn movable-height?
  [level target]
  ;; (println level target (<= (- target level) 1))
  (<= (- target level) 1)
  )
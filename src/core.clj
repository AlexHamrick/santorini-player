(ns santorini-player.core
  (:require [cheshire.core :refer :all :as cheshire]))

(declare within-one?
         movable-height?
         get-valid-builds
         get-valid-moves
         update-players
         read-json
         get-players
         get-board
         get-turn
         take-turn
         pick-move
         get-move
         get-build
         get-move-height
         get-build-height
         add-coords
         compare-moves
         move-to-json
         create-new-board
         create-new-players)

(defn -main
  [json]
  (let [jmap (read-json json)
        board (get-board jmap)
        turn (get-turn jmap)
        players (get-players jmap)
        options (take-turn board players turn)
        move (pick-move options)
        result (move-to-json move board players turn)
        ]
    (println (cheshire/generate-string result))
    )
  )

(defn move-to-json
  [move board players turn]
  (let [turnmap (assoc {} "turn" (inc turn))
        pmap (assoc turnmap "players" (create-new-players players move))
        bmap (assoc pmap "spaces" (create-new-board board move))
  ]
    bmap)
  )

(defn create-new-board
  [board move]
  (assoc-in board (get-build move) (inc (get-build-height move)))
  )

(defn create-new-players
  [players move]
  (let [flip-players [(get players 1) (get players 0)]]
    (assoc-in flip-players [1 (get move 0)] (get-move move)))
  )

(defn pick-move
  [options]
  (get (vec (sort compare-moves options)) 0)
  )

(defn compare-moves
  [move1 move2]
  (let [mh1 (get-move-height move1)
        mh2 (get-move-height move2)
        build1 (get-build move1)
        build2 (get-build move2)
        c1 (compare (- 0 mh1) (- 0 mh2))
        c2 (compare (add-coords build1) (add-coords build2))
        ]
    (if (zero? c1)
      c2
      c1))
  )

(defn add-coords
  [coord]
  (+ (get coord 0) (get coord 1))
  )

(defn get-move 
  [move]
  (get (get move 1) 0)
  )

(defn get-move-height
  [move]
  (get (get move 1) 1))

(defn get-build
  [move]
  (get (get move 2) 0))

(defn get-build-height
  [move]
  (get (get move 2) 1))

(defn get-players
  [json-map]
  (get json-map "players")
  )

(defn get-board
  [json-map]
  (get json-map "spaces"))

(defn get-turn
  [json-map]
  (get json-map "turn"))

(defn read-json
  [json]
  (cheshire/parse-string json)
  )


(defn take-turn
  [board players turn]
  (let [res (for [pos (range 2)
                   move (get-valid-moves board (get-in players [0 pos]) players)
                   build (get-valid-builds board (get move 0) (update-players players pos move))]
              [pos move build])
        ]
    (vec res)
    )
  )

(defn update-players
  [players pos newloc]
  ;; (println (assoc-in players [0 pos] newloc))
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
               [[(inc x) (inc y)] val])]
    (vec vals)))

;;
;; MUST HAVE THE CORRECT PIECES
(defn get-valid-builds
  [board piece players]
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
               [[(inc x) (inc y)] val])]
    (vec vals))
  )



(defn within-one?
  [num1 num2]
  (not (> (Math/abs (- num1 num2)) 1))
  )

(defn movable-height?
  [level target]
  (<= (- target level) 1)
  )
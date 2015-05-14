(ns tetris.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener WindowListener))
  (:use clojure.contrib.import-static
        [clojure.contrib.seq-utils :only (includes?)])
  (:require color.preset)
  (:gen-class))

(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

(declare block-shape)

(def program-title "클로저 테트리스")
(def program-author "박연오 (bakyeono@gmail.com")

(defrecord Board [cols rows cells])
(defrecord Block [id color pos rotation])
(defrecord Cell [color pos])

(def color-list [color.preset/bittersweet
                 color.preset/blue
                 color.preset/blue-bell
                 color.preset/brick-red
                 color.preset/dandelion
                 color.preset/electric-lime
                 color.preset/indigo
                 color.preset/maroon
                 color.preset/midnight-blue
                 color.preset/wisteria])

(def default-board-cols 9)
(def default-board-rows 20)
(def block-start-pos [3 0])
(def block-start-rotation 0)
(def block-max-rotation 4)
(def block-fall-delay 370)

(def point-size 28)

(def dirs {:left  [-1  0]
           :right [ 1  0]
           :up    [ 0 -1]
           :down  [ 0  1]})

(defn point-to-screen-rect
  "좌표를 받아 화면상 출력할 사각형 값으로 반환한다."
  [pt]
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1]))

(defn background-screen-rect []
  [0 0 (* point-size default-board-cols) (* point-size default-board-rows)])

(defn background-dead-rect []
  [0 0 (* point-size default-board-cols) (* point-size 4)])

(defn add-points
  "주어진 좌표를 더한다."
  [& pts]
  (vec (apply map + pts)))

(defn create-board
  "초기화된 게임판을 반환한다."
  ([]
     (create-board default-board-cols default-board-rows))
  ([cols rows]
     (Board. cols rows [])))

(defn rand-fgcolor []
  (rand-int (count color-list)))

(defn get-board-line
  "게임판의 한 줄을 읽어온다."
  [board y]
  (vec (filter #(= y (second (:pos %))) (:cells board))))

(defn count-cells-in-line
  [board y]
  (count (get-board-line board y)))

(defn find-filled-line
  "게임판에 블록이 채워진 줄을 찾는다."
  [{:keys [cols rows cells] :as board}]
  (vec
   (filter (complement nil?)
           (for [i (range rows)]
             (when (<= cols (count-cells-in-line board i))
               i)))))

(defn cell-out-of-lines? [{:keys [pos]} lines]
  (every? false? (map #(= (second pos) %) lines)))

(defn fall-cleared-line
  [cells cleared-lines]
  (if (empty? cleared-lines)
    (vec cells)
    (recur (map #(let [x (first (:pos %))
                       y (second (:pos %))
                       color (:color %)]
                   (Cell. color
                          (vector x
                                  (if (<= y (first cleared-lines))
                                    (inc y)
                                    y))))
                cells)
           (rest cleared-lines))))

(defn clear-filled-line
  [{:keys [cols rows cells] :as board}]
  (let [filled-lines (find-filled-line board)]
    (if (empty? filled-lines)
      board
      (Board. cols rows
              (fall-cleared-line
               (filter #(cell-out-of-lines? % filled-lines) cells)
               filled-lines)))))

(defn board-overflow?
  [{:keys [cells]}]
  (true? (some #(< (second (:pos %)) 4) cells)))

(defn game-lose?
  [board]
  (board-overflow? board))

(defn get-block-shape
  "블록의 모양을 반환한다."
  [{:keys [id rotation color pos]}]
  (vec (map #(assoc % :color color :pos (add-points pos (:pos %)))
            ((block-shape id) rotation))))

(defn block-landed-board
  [{:keys [cells] :as board} block]
  (assoc board
    :cells (apply conj cells (get-block-shape block))))

(defn not-collision?
  "블록이 적합한 위치에 있는가?"
  [block {:keys [cols rows cells] :as board}]
  (every? (fn [block-cell] (let [[x y] (:pos block-cell)]
                             (and (<= 0 x) (< x cols)
                                  (<= 0 y) (< y rows)
                                  (not-any? #(= [x y] (:pos %))
                                            cells))))
          (get-block-shape block)))

(defn create-block
  "블록을 생성한다."
  ([]
     (create-block
      (rand-int 6)
      (rand-fgcolor)))
  ([id color]
     (Block. id
             color
             block-start-pos
             block-start-rotation)))

(defn rotate-block
  "블록을 회전시킨다."
  [{:keys [rotation] :as block} {:keys [cols] :as board}]
  (let [new-block (assoc block :rotation (rem (inc rotation) block-max-rotation))]
    (if (not-collision? new-block board)
      new-block
      block)))

(defn move-block
  "블록을 움직인다."
  [{:keys [pos] :as block} movement {:keys [cols] :as board}]
  (let [new-block (assoc block :pos (add-points pos (movement dirs)))]
    (if (not-collision? new-block board)
      new-block
      block)))

(defn reset-game
  "게임을 초기화한다."
  [board block]
  (dosync (ref-set board (create-board))
          (ref-set block (create-block)))
  nil)

(defn update-block
  "블록 정보를 ref에 업데이트 한다."
  [ref-block new-block]
  (dosync (ref-set ref-block new-block)))

(defn update-board
  "게임판 정보를 ref에 업데이트 한다."
  [ref-board new-board]
  (dosync (ref-set ref-board new-board)))

(defn fall-block
  "블록을 한 칸 내린다."
  [ref-block ref-board]
  (let [new-block (assoc @ref-block :pos (add-points (:pos @ref-block) (:down dirs)))]
    (if (not-collision? new-block @ref-board)
      (do (update-block ref-block new-block))
      (do (update-board ref-board (block-landed-board @ref-board @ref-block))
          (update-board ref-board (clear-filled-line @ref-board))
          (update-block ref-block (create-block))))
    (if (game-lose? @ref-board)
      (do (update-board ref-board (create-board))
          (update-block ref-block (create-block)))
      @ref-block)))

(defn process-key
  [key ref-board ref-block]
  (condp = key
    VK_LEFT  (update-block ref-block (move-block @ref-block :left @ref-board))
    VK_RIGHT (update-block ref-block (move-block @ref-block :right @ref-board))
    VK_UP    (update-block ref-block (rotate-block @ref-block @ref-board))
    VK_DOWN  (update-block ref-block (fall-block ref-block ref-board))
    nil))

(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defn fill-background [g]
  (let [[x y width height] (background-screen-rect)]
    (.setColor g color.preset/white)
    (.fillRect g x y width height))
  (let [[x y width height] (background-dead-rect)]
    (.setColor g color.preset/periwinkle)
    (.fillRect g x y width height)))

(defmulti paint (fn [g object & _] (class object)))

(defmethod paint tetris.core.Board [g {:keys [cols rows] :as board}]
  (do (fill-background g)
      (doseq [cell (:cells board)]
        (fill-point g (:pos cell) (color-list (:color cell))))))

(defmethod paint tetris.core.Block [g {:keys [pos] :as block}]
  (doseq [cell (get-block-shape block)]
    (fill-point g (:pos cell) (color-list (:color cell)))))

(defn game-panel [frame ref-board ref-block]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @ref-board)
      (paint g @ref-block))
    (actionPerformed [e]
      (fall-block ref-block ref-board)
      (.repaint this))
    (keyPressed [e]
      (process-key (.getKeyCode e) ref-board ref-block)
      (.repaint this))
    (getPreferredSize []
      (Dimension. (* default-board-cols point-size)
                  (* default-board-rows point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [ref-board (ref (create-board))
        ref-block (ref (create-block))
        frame (JFrame. program-title)
        panel (game-panel frame ref-board ref-block)
        timer (Timer. block-fall-delay panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add panel)
      (.pack)
      (.setVisible true)
      (.setResizable false))
    (.start timer)))

(defn -main []
  (game))

(def block-shape
  [[[(Cell. nil [1 0]) (Cell. nil [2 0]) (Cell. nil [1 1]) (Cell. nil [2 1])]
    [(Cell. nil [1 0]) (Cell. nil [2 0]) (Cell. nil [1 1]) (Cell. nil [2 1])]
    [(Cell. nil [1 0]) (Cell. nil [2 0]) (Cell. nil [1 1]) (Cell. nil [2 1])]
    [(Cell. nil [1 0]) (Cell. nil [2 0]) (Cell. nil [1 1]) (Cell. nil [2 1])]]
   [[(Cell. nil [1 0]) (Cell. nil [1 1]) (Cell. nil [1 2]) (Cell. nil [1 3])]
    [(Cell. nil [0 1]) (Cell. nil [1 1]) (Cell. nil [2 1]) (Cell. nil [3 1])]
    [(Cell. nil [1 0]) (Cell. nil [1 1]) (Cell. nil [1 2]) (Cell. nil [1 3])]
    [(Cell. nil [0 1]) (Cell. nil [1 1]) (Cell. nil [2 1]) (Cell. nil [3 1])]]
   [[(Cell. nil [1 0]) (Cell. nil [2 0]) (Cell. nil [2 1]) (Cell. nil [2 2])]
    [(Cell. nil [0 0]) (Cell. nil [1 0]) (Cell. nil [2 0]) (Cell. nil [0 1])]
    [(Cell. nil [0 0]) (Cell. nil [0 1]) (Cell. nil [0 2]) (Cell. nil [1 2])]
    [(Cell. nil [2 1]) (Cell. nil [0 2]) (Cell. nil [1 2]) (Cell. nil [2 2])]]
   [[(Cell. nil [0 0]) (Cell. nil [1 0]) (Cell. nil [0 1]) (Cell. nil [0 2])]
    [(Cell. nil [0 1]) (Cell. nil [0 2]) (Cell. nil [1 2]) (Cell. nil [2 2])]
    [(Cell. nil [2 0]) (Cell. nil [2 1]) (Cell. nil [1 2]) (Cell. nil [2 2])]
    [(Cell. nil [0 0]) (Cell. nil [1 0]) (Cell. nil [2 0]) (Cell. nil [2 1])]]
   [[(Cell. nil [1 0]) (Cell. nil [1 1]) (Cell. nil [2 1]) (Cell. nil [2 2])]
    [(Cell. nil [2 0]) (Cell. nil [3 0]) (Cell. nil [1 1]) (Cell. nil [2 1])]
    [(Cell. nil [1 0]) (Cell. nil [1 1]) (Cell. nil [2 1]) (Cell. nil [2 2])]
    [(Cell. nil [2 0]) (Cell. nil [3 0]) (Cell. nil [1 1]) (Cell. nil [2 1])]]
   [[(Cell. nil [2 0]) (Cell. nil [1 1]) (Cell. nil [2 1]) (Cell. nil [1 2])]
    [(Cell. nil [0 0]) (Cell. nil [1 0]) (Cell. nil [1 1]) (Cell. nil [2 1])]
    [(Cell. nil [2 0]) (Cell. nil [1 1]) (Cell. nil [2 1]) (Cell. nil [1 2])]
    [(Cell. nil [0 0]) (Cell. nil [1 0]) (Cell. nil [1 1]) (Cell. nil [2 1])]]])


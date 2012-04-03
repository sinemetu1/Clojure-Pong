(ns pong.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:use clojure.contrib.import-static)
  (:gen-class))

(import-static java.awt.event.KeyEvent
               VK_ENTER VK_ESCAPE
               VK_LEFT VK_RIGHT VK_UP VK_DOWN)

; Debugger Code:
(defmacro dbg[x] `(let [x# ~x] (println "*** dbg:" '~x "=" x#) x#))


(def running false)
(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 20)
(def collision-multiplier 1.1)
(def dirs { VK_LEFT  [-4 0]
            VK_RIGHT [4 0]
           })

(defn point-to-screen-rect [pt]
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1]))

(defn point-to-screen-round-rect [pt
                                  width
                                  height]
  (map #(* point-size %)
       [(pt 0) ; x
        (pt 1) ; y
        width  ; width
        height ; height
        1      ; arcWidth
        1      ; arcHeight
        ]))

(defn create-user-paddle []
  {:location [32 48]
   :width 7
   :height 1
   :dir [0 0]
   :color (Color. 210 50 90)
   :type :paddle})

(defn create-ai-paddle []
  {:location [32 2]
   :width 7
   :height 1
   :dir [0 0]
   :color (Color. 15 160 70)
   :type :paddle})

(defn create-ball []
  {:location [0 0]
   :dir [1/2 1/2]
   :color (Color. 50 50 50)
   :type :ball})

(defn add-points [& pts]
  (vec (apply map + pts)))

(defmulti move (fn [object & direction] (:type object)))

(defmethod move :paddle [{:keys [location] :as paddle} dir] 
  (let [[x y] (add-points location dir)
        right (+ x (:width paddle))
        [vx vy] dir]
    (if (and (<= x 0) (<= vx 0))
      (assoc paddle :location [0 y])
      (if (and (>= right width) (>= vx 0))
        (assoc paddle :location [width y])
        (assoc paddle :location [x y])))))

(defmethod move :ball [{:keys [location dir] :as ball}] 
  (let [[x y] (add-points location dir)
        [vx vy] dir]
      (if (<= x 0)
        (assoc ball :location [1 y] :dir [1/2 vy])
        (if (>= x width)
          (assoc ball :location [(- width 1) y] :dir [-1/2 vy])
          (assoc ball :location [x y])))))

(defn win? [{location :location}]
  (let [[x y] location]
    (<= y 0)))

(defn lose? [{location :location}]
  (let [[x y] location]
    (>= y height)))

(defn collision? [{:keys [location width height] :as paddle}
                  ball 
                  & user]
  (let [[x y] location
        [bx by] (:location ball)
        topRight (+ x width)]
    (if user
      (and (and (>= bx x) (<= bx topRight))
           (>= by y)) 
      (and (and (>= bx x) (<= bx topRight))
           (<= by (+ y height)))
      )
    )
  )

(defn handle-collission [{:keys [location dir] :as ball}]
  (let [[vx vy] dir]
      (assoc ball
             :dir [(* vx collision-multiplier) (- (* vy collision-multiplier))]
             :location (add-points location [vx (- vy)]))))

(defn update-ball [ai user ball]
  (dosync
    (let [[vx vy] (:dir @ball)]
      (if (or (collision? @ai @ball)
              (collision? @user @ball true))
        (alter ball handle-collission) 
        (alter ball move)))))

(defn update-ai [ai ball]
  (dosync
    (let [[vx vy] (:dir @ball)
          [bx by] (:location @ball)
          [ax ay] (:location @ai)]
      (if (< vy 0)
        (cond
          ; ball is to the left of the ai paddle
          (< bx ax) (alter ai move [-1 0])
          ; ball is to the right of the ai paddle
          (> bx (+ ax (:width @ai))) (alter ai move [1 0])
          )))))

(defn update-direction [user newdir]
  (when newdir
    (dosync
      (alter user move newdir))))

(defn reset-game[user ai ball]
  (dosync
    (ref-set user (create-user-paddle))
    (ref-set ai (create-ai-paddle))
    (ref-set ball (create-ball))))

; END FUNCTIONAL AREA

(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-round-rect pt 1 1)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defn fill-round-rect [g {:keys [location height width color] :as paddle}]
  (let [[x y w h aw ah] (point-to-screen-round-rect location
                                              width
                                              height)]
    (.setColor g color)
    (.fillRoundRect g x y w h aw h)))

(defmulti paint (fn [g object & _] (:type object)))

(defmethod paint :paddle [g paddle]
  (fill-round-rect g paddle))

(defmethod paint :ball [g {:keys [location color]}]
  (fill-point g location color))

(defn game-panel [frame ai user ball]
  (proxy [JPanel KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @ai)
      (paint g @user)
      (paint g @ball))
    (keyPressed [e]
      (let [keyCode (.getKeyCode e)]
        (cond
          (= keyCode VK_ENTER) (if (not running)
                                 (def running true)
                                 (def running false))
          (= keyCode VK_ESCAPE) (.dispose frame)
          :else (update-direction user (dirs keyCode)))))
    (getPreferredSize []
      (Dimension. (* (inc width) point-size)
                  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [user (ref (create-user-paddle))
        ai   (ref (create-ai-paddle))
        ball (ref (create-ball))
        frame (JFrame. "Pong")
        panel (game-panel frame ai user ball)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel)
      (.repaint))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (loop []
      (if running
        (do
          (update-ball ai user ball)
          (update-ai ai ball)
          (when (lose? @ball)
            (def running false)
            (JOptionPane/showMessageDialog frame "You Lose :("))
          (when (win? @ball)
            (def running false)
            (JOptionPane/showMessageDialog frame "You Win! :D"))
          (.repaint panel)))
          (. Thread sleep turn-millis)
      (recur))
    [ai user ball]))

(defn -main [& args]
  (game))

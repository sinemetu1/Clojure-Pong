(ns pong.screenManager
  (:import (java.awt GraphicsEnvironment DisplayMode Graphics2D)
           (java.awt.image BufferStrategy BufferedImage)
           (javax.swing JFrame)))

(def #^{:private true}
  vc)

(defn screenManager
  []
  (let [e (GraphicsEnvironment/getLocalGraphicsEnvironment)]
    (def vc (.getDefaultScreenDevice e))))

(defn getFullScreenWindow
  []
  (.getFullScreenWindow vc))

(defn getCompatibleDisplayModes
  []
  (.getDisplayModes vc))

(defn- displayModesMatch
  [^DisplayMode m1 ^DisplayMode m2]
  (if (or (not= (.getWidth m1) (.getWith m2)) (not= (.getHeight m1) (.getHeight m2)))
    false)
  (if (and (not= (.getBitDepth m1) (. DisplayMode BIT_DEPTH_MULTI))
           (not= (.getBitDepth m2) (. DisplayMode BIT_DEPTH_MULTI))
           (not= (.getBitDepth m1) (.getBitDepth m2)))
    false)
  (if (and (not= (.getRefreshRate m1) (. DisplayMode REFRESH_RATE_UNKNOWN))
           (not= (.getRefreshRate m2) (. DisplayMode REFRESH_RATE_UNKNOWN))
           (not= (.getRefreshRate m1) (.getRefreshRate m2)))
    false)
  true)

(defn findFirstCompatibleMode
  [modes]
  (let [goodModes (getCompatibleDisplayModes)]
    (for [m modes]
      (for [gm goodModes]
        (if (displayModesMatch m gm)
          m)))
    nil))



(defn setFullScreen
  [^DisplayMode dm]
  (let [f (JFrame.)]
    (.setUndecorated f true)
    (.setIgnoreRepaint f true)
    (.setResizable f false)
    (.setFullScreenWindow vc f)

    (if (and (not= dm nil)
             (.isDisplayChangeSupported vc))
      (try
        (.setDisplayMode dm)
        (catch Exception ex (.printStackTrace ex))))
    (.createBufferStrategy f 2)))

(defn getGraphics
  []
  (let [w (getFullScreenWindow)]
    (if (not= w nil)
      (let [strat (.getBufferStrategy w)]
        (cast Graphics2D (.getDrawGraphics strat)))
      nil)))

(defn update
  []
  (let [w (getFullScreenWindow)]
    (if (not= w nil)
      (let [strat (.getBufferStrategy w)]
        (if (not (.contentsLost strat))
          (.show strat))))))


(defn getWidth
  []
  (let [w (getFullScreenWindow)]
    (if (not= w nil)
      (.getWidth w)
      0)))

(defn getHeight
  []
  (let [w (getFullScreenWindow)]
    (if (not= w nil)
      (.getHeight w)
      0)))

(defn restoreScreen
  []
  (let [w (getFullScreenWindow)]
    (if (not= w nil)
      (.dispose w))
    (.setFullScreenWindow vc)))

(defn createCompatibleImage
  [w h t]
  (let [window (getFullScreenWindow)]
    (if (not= window nil)
      (let [gc (.getGraphicsConfiguration window)]
        (.createCompatibleImage gc w h t)))))

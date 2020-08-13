(ns javapro.functional-images
  (:import (java.awt Color Graphics Image BorderLayout)
           (javax.swing JFrame JLabel ImageIcon)
           java.awt.image.BufferedImage))

(defrecord Point [x y])

(def point1 (Point. 1 2))
(def point2 (Point. 0.5 4))

(defn draw-image!
  [image
   ^Graphics graphics
   width height
   x-min x-max y-min y-max]
  (let [xinc (/ (- x-max x-min)
                (double width))
        yinc (/ (- y-max y-min)
                (double height))]
    (doseq [x (range 0 width)
            y (range 0 height)]
      (let [black? (image (Point. (+ x-min (* xinc x)) (+ y-min (* yinc y))))
            color (if black?
                    Color/BLACK
                    Color/WHITE)]
        (.setColor graphics color)
        (.fillRect graphics x y 1 1)))))

(defn image->bitmap
  [image width height x-min x-max y-min y-max]
  (let [buffered-image (BufferedImage. 
                        width height
                        BufferedImage/TYPE_INT_ARGB)
        graphics (.getGraphics buffered-image)
        xinc (/ (- x-max x-min)
                (double width))
        yinc (/ (- y-max y-min)
                (double height))]
    (doseq [x (range 0 width)
            y (range 0 height)]
      (let [black? (image (Point. (+ x-min (* xinc x)) (+ y-min (* yinc y))))
            color (if black?
                    Color/BLACK
                    Color/WHITE)]
        (.setColor graphics color)
        (.fillRect graphics x y 1 1)))
    
    buffered-image))

(defn display-image!
  [image width height x-min x-max y-min y-max]
  (let [bitmap (image->bitmap image width height x-min x-max y-min y-max)
        frame (JFrame. "Image")
        label (JLabel.)]
    (.setIcon label (ImageIcon. bitmap))
    (.setSize frame width height)
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.add (.getContentPane frame) label BorderLayout/CENTER)
    (.pack frame)
    (.setVisible frame true)))
    
(defn write-image!
  [filename image width height x-min x-max y-min y-max]
  (let [bitmap (image->bitmap image width height x-min x-max y-min y-max)]
    (javax.imageio.ImageIO/write bitmap "png" (java.io.File. filename))))

(defn vstrip
  [p]
  (<= (Math/abs (:x p)) 0.5))

(defn vstripes
  [p]
  (even? (int (Math/floor (:x p)))))

(defn hstripes
  [p]
  (even? (int (Math/floor (:y p)))))

(defn checker
  [p]
  (even? (+ (int (Math/floor (:x p))) (int (Math/floor (:y p))))))

(defn distance-from-origin
  [x y]
  (Math/sqrt (+ (* x x) (* y y))))

(defn alt-rings
  [p]
  (even? (int (Math/floor (distance-from-origin (:x p) (:y p))))))

; Combinators

(defn img-xor
  [img1 img2]
  (fn [p]
    (not= (img1 p) (img2 p))))

(defn checker
  [p]
  (not= (vstripes p) (hstripes p)))

(def checker
  (img-xor hstripes vstripes))

(defn img-and
  [img1 img2]
  (fn [p]
    (and (img1 p) (img2 p))))

(defn lift2
  [f]
  (fn [img1 img2]
    (fn [p]
      (f (img1 p) (img2 p)))))

(def img-xor (lift2 not=))

(def img-and (lift2 (fn [a b] (and a b))))

(defn lift1
  [f]
  (fn [img]
    (fn [p]
      (f (img p)))))

(def img-not (lift1 not))

(defn img-subtract
  [r1 r2]
  (img-and r1 (img-not r2)))


; Transformations

(defn to-polar
  [p]
  (Point. (distance-from-origin (:x p) (:y p))
          (Math/atan2 (:x p) (:y p))))

(def to-polar
  (fn [p]
    (Point. (distance-from-origin (:x p) (:y p))
            (Math/atan2 (:x p) (:y p)))))

(defn turn
  [n]
  (fn [p]
    (Point. (:x p)
            (* (:y p)
               (/ n Math/PI)))))   

(defn polar-checker
  [n]
  (comp checker (turn n) to-polar))

; Rest

(defn from-polar
  [p]
  (Point. (* (:x p) (Math/cos (:y p)))
          (* (:x p) (Math/sin (:y p)))))

(defn from-polar-transformation
  [trafo]
  (comp from-polar trafo to-polar))

(def invert-polar-radius
  (from-polar-transformation
   (fn [p]
     (Point. (if (zero? (:x p))
               0.0
               (/ 1.0 (:x p)))
             (:y p)))))

(def rad-invert-checker
  (comp checker invert-polar-radius))



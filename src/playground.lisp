(in-package :fouric-charms)

(defclass widget ()
  ((dirty? :accessor widget.dirty? :initform t)
   (parent :accessor widget.parent :initform nil)
   ;; x and y offset relative to parent? (maybe only used by plane)
   (x :accessor x :initform 0 :initarg :x)
   (y :accessor y :initform 0 :initarg :y)
   ;; w and h are "cached" from rendering the widget previously
   (w :accessor w :initform 0)
   (h :accessor h :initform 0)))

(f:ft set-xy (widget integer integer) f:nothing)
(defun set-xy (widget x y)
  (setf (x widget) x
        (y widget) y)
  (values))

;; "draw": (1) layout (2) render (3) paint
;; "layout" means to compute position and size (although for now we're just doing size)
;; "render" means to take internal state and do compute to generate a graphical representation somewhere in memory - NOT paint to screen
;; "paint" means to take the rendered widget "texture" and display it on the screen
;; BUT we're just gonna collapse them into a single method, "draw", for athena MVP
(f:ft layout (widget) (values integer integer))
(defgeneric layout (widget))
;; RENDERPAINT assumes that the w and h fields of all widgets involved are correct and up to date
(f:ft renderpaint (widget integer integer) f:nothing)
(defgeneric renderpaint (widget x y)) ;; x and y are offsets FROM THE CORNER OF THE SCREEN

(f:ft add-child (widget widget) f:nothing)
(defgeneric add-child (parent child))

;; root
;; \- vstack
;;    \- child 1
;;    \- child 2

;; +---------+
;; |         |
;; | widget1 |
;; |         |
;; +---------+
;; | widget2 |
;; +---------+

;; first calculate dimension, and then positioning
;; we don't want offsets because that'll introduce a feedback loop from positioning back to dimensions

;; might be a special case - just draws children at position offsets, doesn't do any layout, stacks them on top of each other
;; only for debugging!
(defclass plane (widget)
  ((children :accessor children :initform nil)))
(defun make-plane (&optional parent children)
  (let ((instance (make-instance 'plane)))
    (when parent
      (add-child parent instance))
    (dolist (child children)
      (add-child instance child))
    (values instance)))
(defmethod add-child ((parent plane) child)
  (setf (widget.parent child) parent)
  (push child (children parent))
  (values))
(defmethod layout ((plane plane))
  (values (w plane) (h plane)))
;; x and y are ABSOLUTE offsets from corner
(defmethod renderpaint ((plane plane) x y)
  (dolist (child (children plane))
    ;; we have to give the child its offsets because it doesn't know those because it doesn't do its own layout!
    (renderpaint child (+ (x child) x) (+ (y child) y))))


(defclass vstack (widget)
  ((children :accessor children :initform nil)))
(defun make-vstack (&optional parent children)
  (let ((instance (make-instance 'vstack)))
    (when parent
      (add-child parent instance))
    (dolist (child (reverse children))
      (add-child instance child))
    (values instance)))
(defmethod add-child ((parent vstack) child)
  (push child (children parent))
  (values))
(defmethod layout ((vstack vstack))
  (let ((w 0)
        (h 0))
    (dolist (child (children vstack))
      (multiple-value-bind (child-w child-h)
          (layout child)
        (incf h child-h)
        (setf w (max w child-w))))
    (setf (w vstack) w
          (h vstack) h)
    (values w h)))
(defmethod renderpaint ((vstack vstack) x y)
  (let ((y* 0))
    (dolist (child (children vstack))
      (renderpaint child x (+ y y*))
      (incf y* (h child)))))

(defclass textline (widget)
  ((content :accessor textline.content :initarg :content :initform "")))
(f:ft make-textline (string &optional (f:optional widget) integer integer) (f:just textline))
(defun make-textline (content &optional (parent nil))
  (let ((instance (make-instance 'textline :content content)))
    (when parent
      (add-child parent instance))
    (values instance)))
(defmethod layout ((widget textline))
  (let ((w (length (textline.content widget)))
        (h 1))
    (setf (w widget) w
          (h widget) h)
    (values w h)))
;; if no "rendering" needs to happen, do a no-op
;;(defmethod render ((widget textline)))
(defmethod renderpaint ((widget textline) x y)
  (fouric-charms:draw-string (textline.content widget) x y))

(defun frame-function (stack)
  (let* (#++(root (getf data :root-widget))
         (textline (first (children stack))))
    (with-slots (x y) textline
      (f:update-swank)
      (f:when-case (char (get-char))
        (#\j (incf y))
        (#\k (decf y))
        (#\h (decf x))
        (#\l (incf x))
        (#\i (setf *invert* (not *invert*)))
        (#\q
         (print 'returning)
         (return-from frame-function nil))
        (#\b
         (break))
        (+resize+
         ;; refresh here
         )
        ((nil)
         t)
        (t
         (format t "got other char ~s~%" char)
         ))

      (clear-window)

      (layout stack)
      (renderpaint stack 0 0)

      (refresh-window)
      t)))

(defun playground ()
  (with-charms (:timeout 1000 :raw-input t :interpret-control-characters nil)
    (let* (#++(root (make-instance 'plane))
           #++(data `(:root-widget ,root))
           (textline1 (make-textline "hello!"))
           (textline2 (make-textline "world!"))
           (textline3 (make-textline "no cap"))
           #++(stack (make-vstack :children (list textline1 textline2)))
           (stack2a (make-vstack nil (list textline1 textline2)))
           (stack2b (make-vstack nil (list textline3)))
           (stack (make-vstack nil (list stack2a stack2b))))
      ;;(set-xy textline2 0 1)
      (loop :named main-loop :do
        (unless (frame-function stack)
          (return-from main-loop))))))

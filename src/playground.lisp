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
(f:ft handle-chord (widget (f:optional character)) (f:just symbol))
(defgeneric handle-chord (widget chord))
;; default implementation: don't handle
(defmethod handle-chord ((widget widget) chord)
  :propagate)

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
  ((child-bindings :accessor child-bindings :initform nil))) ;; each child-binding is ((x . y) . child-widget)
(defun make-plane (&optional parent children)
  (let ((instance (make-instance 'plane)))
    (when parent
      (add-child parent instance))
    (dolist (child children)
      (add-child instance child))
    (values instance)))
(defmethod children ((plane plane))
  (mapcar #'cddr (child-bindings plane)))
(defun add-plane-child (plane child x y)
  (setf (widget.parent child) plane)
  (push (cons (cons x y) child) (child-bindings plane))
  (values))
(defun remove-plane-child (plane child)
  (delete child (child-bindings plane) :key #'cddr))
(defmethod layout ((plane plane))
  (values (w plane) (h plane)))
;; x and y are ABSOLUTE offsets from corner
(defmethod renderpaint ((plane plane) x y)
  (dolist (binding (child-bindings plane))
    (destructuring-bind ((child-x . child-y) . child)
        binding
      ;; we have to give the child its offsets because it doesn't know those because it doesn't do its own layout!
      (renderpaint child (+ child-x x) (+ child-y y)))))


(defclass vstack (widget)
  ((child-bindings :accessor child-bindings :initform nil))) ;; why not CHILDREN? because the members of this list aren't going to be children, but (layout . child) cons cells
(defun make-vstack (&optional parent children)
  (let ((instance (make-instance 'vstack)))
    (when parent
      (add-child parent instance))
    (dolist (child (reverse children))
      (add-child instance child))
    (values instance)))
(defmethod children ((vstack vstack))
  (mapcar #'cdr (child-bindings vstack)))
(defmethod add-child ((parent vstack) child)
  (vstack-add-child parent child :top)
  (values))
(f:ft vstack-add-child (vstack widget &optional symbol) f:nothing)
(defun vstack-add-child (vstack child &optional (position :top))
  (assert (member position '(:top :bottom)))
  (setf (widget.parent child) vstack)
  ;; each child binding is a cons cell of (position . child)
  ;; need to push to END of list
  (setf (child-bindings vstack) (append (child-bindings vstack) (list (cons position child))))
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

(defclass label (widget)
  ((content :reader content :initarg :content :initform "")))
(f:ft make-label (string &optional (f:optional widget)) (f:just label))
(defun make-label (content &optional (parent nil))
  (let ((instance (make-instance 'label :content content)))
    (when parent
      (add-child parent instance))
    (values instance)))
(defmethod layout ((widget label))
  (let ((w (length (content widget)))
        (h 1))
    (setf (w widget) w
          (h widget) h)
    (values w h)))
;; if no "rendering" needs to happen, do a no-op
;;(defmethod render ((widget label)))
(defmethod renderpaint ((widget label) x y)
  (fouric-charms:draw-string (content widget) x y))
(defmethod children ((label label))
  nil)

;; mutable label
(defclass textline (label)
  ())
;; the CONTENT slot only had a reader for the label class, but we're going to define a setf expander that redoes layout
(defmethod (setf content) (value (line textline))
  (setf (slot-value line 'content) value
        (widget.dirty? line) t)
  (layout line))
(f:ft make-textline (string &optional (f:optional widget)) textline)
(defun make-textline (content &optional parent)
  (let ((instance (make-instance 'textline :content content)))
    (when parent
      (add-child parent instance))
    instance))
;; inherit layout, renderpaint, children too
(defmethod handle-chord ((line textline) chord)
  (cond
    ;; unnecessary?
    ((null chord)
     :propagate)
    ((eq chord +backspace+)
     (symbol-macrolet ((content (content line)))
       (unless (string= content "")
         (setf content (subseq content 0 (1- (length content)))))
       :stop))
    ((<= (char-code #\space) (char-code chord) (char-code #\tilde))
     ;; ascii character - append
     (setf (content line) (concatenate 'string (content line) (coerce (list chord) 'string)))
     :stop)
    (t
     :propagate)))

(defun frame-function (root)
  (let* ((stack1 (first (children root))))
    (with-slots (x y) stack1
      (f:update-swank)
      (let ((char (get-char)))
        (when char
          (cond
            ((eq char +c-q+)
             (print 'returning)
             (return-from frame-function nil))
            ((eq char +c-b+)
             (break))
            #++(+resize+
                ;; refresh here
                )
            (t
             (format t "result of HANDLE-CHORD is ~s~%" (handle-chord root char))
             (format t "got other char ~s~%" char)
             ))))

      (clear-window)

      (layout root)
      (renderpaint root 0 0)

      (refresh-window)
      t))

  (defun playground ()
    (with-charms (:timeout 1000 :raw-input t :interpret-control-characters nil)
      (let* (#++(label1 (make-label "hello!"))
             #++(label2 (make-label "world!"))
             #++(label3 (make-label "no cap"))
             #++(stack1 (make-vstack nil (list label1 label2)))
             #++(stack2 (make-vstack nil (list label3)))
             #++(root (make-vstack nil (list stack1 stack2)))
             (root (make-textline "sup?")))
        (loop :named main-loop :do
          (unless (frame-function root)
            (return-from main-loop)))))))

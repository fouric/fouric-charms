(in-package #:fouric-charms)

(defparameter *handle* nil)
(defparameter *invert* nil)
(defparameter *silently-clip* nil)
(defparameter *foreground* nil)
(defparameter *background* nil)
(defparameter *running* nil)

;; TODO: make constants
(defparameter +f1+ #\latin_small_letter_c_with_circumflex)
(defparameter +f2+ #\latin_capital_letter_c_with_dot_above)
(defparameter +f3+ #\latin_small_letter_c_with_dot_above)
(defparameter +f4+ #\latin_capital_letter_c_with_caron)
(defparameter +f5+ #\latin_small_letter_c_with_caron)
(defparameter +f6+ #\latin_capital_letter_d_with_caron)
(defparameter +f7+ #\latin_small_letter_d_with_caron)
(defparameter +f8+ #\latin_capital_letter_d_with_stroke)
(defparameter +f9+ #\latin_small_letter_d_with_stroke)
(defparameter +f10+ #\latin_capital_letter_e_with_macron)
(defparameter +f11+ #\latin_small_letter_e_with_macron)
(defparameter +f12+ #\latin_capital_letter_e_with_breve)

(defparameter +up+ #\latin_small_ligature_oe)
(defparameter +down+ #\latin_capital_ligature_oe)
(defparameter +left+ #\latin_capital_letter_c_with_acute)
(defparameter +right+ #\latin_capital_letter_u_with_tilde)

(defparameter +resize+ #\latin_small_letter_l_with_bar)

(defparameter +c-a+ #\soh)
(defparameter +c-b+ #\stx)
(defparameter +c-c+ #\etx)
(defparameter +c-d+ #\eot)
(defparameter +c-e+ #\enq)
(defparameter +c-f+ #\ack)
(defparameter +c-g+ #\bel)
(defparameter +c-h/backspace+ #\backspace)
(defparameter +c-i+ #\tab)
(defparameter +c-j+ #\newline)
(defparameter +c-k+ #\vt)
(defparameter +c-l+ #\page)
(defparameter +c-m+ #\newline)
(defparameter +c-n+ #\so)
(defparameter +c-o+ #\si)
(defparameter +c-p+ #\dle)
(defparameter +c-q+ #\dc1)
(defparameter +c-r+ #\dc2)
(defparameter +c-s+ #\dc3)
(defparameter +c-t+ #\dc4)
(defparameter +c-u+ #\nak)
(defparameter +c-v+ #\syn)
(defparameter +c-w+ #\etb)
(defparameter +c-x+ #\can)
(defparameter +c-y+ #\em)
(defparameter +c-z+ #\sub)

(defparameter +delete+ #\latin_capital_letter_eng)
(defparameter +escape+ #\esc)
(defparameter +newline+ #\newline)
(defparameter +pageup+ #\latin_small_ligature_oe)
(defparameter +pagedown+ #\latin_capital_ligature_oe)
(defparameter +home+ #\latin_capital_letter_c_with_acute)
(defparameter +end+ #\latin_capital_letter_u_with_tilde)
(defparameter +insert+ #\latin_small_letter_eng)
(defparameter +backspace+ #\latin_small_letter_c_with_acute) ;; when extra chars turned on, we can differentiate between C-h and backspace

;; formerly charms-handle
(defclass terminal-handle ()
  ((window :accessor window :initarg :window)
   (width :accessor charms-width :initarg :width)
   (height :accessor charms-height :initarg :height)
   (colorpair-map :accessor colorpair-map :initform (make-hash-table :test 'equal))
   (color? :accessor color? :initform nil)
   ;; can't use index 0 (see https://linux.die.net/man/3/init_pair, "The value of the first argument must be between 1 and COLOR_PAIRS-1"), but because we're incrementing and THEN getting the new value, we don't care
   (last-color-index :accessor last-color-index :initform 0)
   (raw-input :accessor raw-input :initarg :raw-input)
   (timeout :accessor timeout :initarg :timeout)
   (interpret-control-characters :accessor interpret-control-characters :initarg :interpret-control-characters)))

(defun width ()
  (charms-width *handle*))
(defun height ()
  (charms-height *handle*))
(defun out-of-bounds (x y &optional (width 1))
  (or (< (width) (+ x width))
      (< (height) y)))

(defun eclamp (val min max)
  (cond
    ((> val max) (error "value ~a is greater than max of ~a" val max))
    ((< val min) (error "value ~a is lesser than min of ~a" val min))
    (t val)))

(defmacro with-color (color &body body)
  (alexandria:once-only (color)
    `(unwind-protect
          (progn
            (when (color? *handle*)
              (charms/ll:attron (charms/ll:color-pair ,color)))
            ,@body)
       (when (color? *handle*)
         (charms/ll:attroff (charms/ll:color-pair ,color))))))

(defun draw-string (string x y &optional (foreground :white) (background :black))
  (dotimes (i (length string))
    (draw-char (char string i) (+ x i) y foreground background)))

(defun fast-draw-string (string x y foreground background)
  ;; TODO: handle corner character
  (let ((handle *handle*))
    (if (out-of-bounds x y (length string))
        (error "DRAW-STRING to point (~a, ~a) with width ~a is outside charms window dimensions of (~a, ~a)" x y (length string) (width) (height))
        (with-color (colors-to-index (if *invert* background foreground) (if *invert* foreground background))
          (charms:write-string-at-point (window handle) string (eclamp x 0 (1- (width))) (eclamp y 0 (1- (height))))))))

(defun draw-char (char x y foreground background)
  ;; TODO: handle corner character
  (let ((handle *handle*))
    (if (out-of-bounds x y)
        (unless *silently-clip*
          (error "DRAW-CHAR to point (~a, ~a) is outside charms window dimensions of (~a, ~a)" x y (width) (height)))
        ;; handle the corner case (literally)
        (unless (and (= x (1- (width)))
                     (= y (1- (height))))
          (with-color (colors-to-index (if *invert* background foreground) (if *invert* foreground background))
            (if (and (<= 0 x (1- (width)))
                     (<= 0 y (1- (height))))
                (charms:write-char-at-point (window handle) char x y)
                (unless *silently-clip*
                  (error "DRAW-CHAR to point (~a, ~a) is outside charms window dimensions of (~a, ~a)" x y (width) (height)))))))))

;; uh, shouldn't we make this a generic function?
(defun draw (object x y &optional foreground background)
  (setf foreground (or foreground *foreground* :white)
        background (or background *background* :black))
  (etypecase object
    (string
     (draw-string object x y foreground background))
    (character
     (draw-char object x y foreground background))))

(defun colors-to-index (foreground background)
  (let* ((handle *handle*)
         (index (gethash (list foreground background) (colorpair-map handle))))
    (if index
        index
        (let ((index (incf (last-color-index handle)))) ;; incf returns the post-increment value, meaning that the first index we get is 1 - that's desirable, because we can't use index 0 per https://linux.die.net/man/3/init_pair
          (setf (gethash (list foreground background) (colorpair-map handle)) index)
          (charms/ll:init-pair index (normalize-color foreground) (normalize-color background))
          index))))

(declaim (ftype (function (symbol) (or integer symbol)) normalize-color))
(defun normalize-color (color)
  (ecase color
    ;; keyword conversion
    (:black charms/ll:color_black)
    (:red charms/ll:color_red)
    (:green charms/ll:color_green)
    (:yellow charms/ll:color_yellow)
    (:blue charms/ll:color_blue)
    (:magenta charms/ll:color_magenta)
    (:cyan charms/ll:color_cyan)
    (:white charms/ll:color_white)
    ;; fallthrough: pass it on
    (t color)))

(declaim (ftype (function (&optional boolean) t) clear-window))
(defun clear-window (&optional force-repaint)
  (charms:clear-window (window *handle*) :force-repaint force-repaint))
(declaim (ftype (function () t) refresh-window))
(defun refresh-window ()
  (charms:refresh-window (window *handle*)))
(declaim (ftype (function () (or character (member nil))) get-char))
(defun get-char ()
  (let ((char (charms:get-char (window *handle*) :ignore-error t)))
    (when (equal char #\latin_small_letter_l_with_bar)
      (update-dimensions))
    char))

(defun init-charms (timeout raw-input interpret-control-characters)
  ;; TODO: if recompiled while running, if raw-input or interpret-control-characters have changed, call enable-raw-input or disable-raw-input to sync state
  (let ((handle (make-instance 'terminal-handle)))
    (force-output *terminal-io*)
    (charms:initialize)
    (charms/ll:timeout timeout) ;; timeout in milliseconds
    (setf (window handle) (charms:standard-window))
    (charms:disable-echoing)
    (charms/ll:curs-set 0) ;; invisible cursor
    (charms:enable-extra-keys (window handle)) ;; translate function and arrow keys
    (when (charms/ll:has-colors)
      (charms/ll:start-color)
      (setf (color? handle) t))
    (if raw-input
        (charms:enable-raw-input :interpret-control-characters interpret-control-characters)
        (charms:disable-raw-input))
    (setf (timeout handle) timeout
          (raw-input handle) raw-input
          (interpret-control-characters handle) interpret-control-characters)
    (multiple-value-bind (width height) (charms:window-dimensions (window handle))
      (setf (charms-width handle) width
            (charms-height handle) height))
    handle))

(defun restart-charms (handle &optional (error-if-running t) (drop-events nil))
  "given a cl-fouric terminal-handle that has initialized configuration, set up charms again using those settings"
  (if error-if-running
      (assert (not *running*))
      (when *running*
        (return-from restart-charms)))
  ;; TODO: if recompiled while running, if raw-input or interpret-control-characters have changed, call enable-raw-input or disable-raw-input to sync state
  (force-output *terminal-io*)
  (charms:initialize)
  ;; timeout set in milliseconds
  (charms/ll:timeout (timeout handle))
  (charms:disable-echoing)
  (charms/ll:curs-set 0) ;; invisible cursor
  (charms:enable-extra-keys (window handle)) ;; translate function and arrow keys
  (when (color? handle)
    (charms/ll:start-color))
  (if (raw-input handle)
      (charms:enable-raw-input :interpret-control-characters (interpret-control-characters handle))
      (charms:disable-raw-input))
  (setf *running* t)
  (when drop-events
    (charms/ll:timeout 1)
    (loop (unless (get-char) (return)))
    (charms/ll:timeout (timeout handle))))

(defmacro with-invert (&body body)
  `(let ((*invert* t))
     ,@body))

(defun write-spaces-window ()
  "write the space character to every cell on the screen to forcibly clear it if curses doesn't want to cooperate"
  (multiple-value-bind (width height) (window-dimensions *handle*)
    (with-color (colors-to-index :black :black)
      (clear-window)
      (dotimes (y height)
        (charms:write-string-at-point (window *handle*) (make-string (if (= y (1- height)) (1- width) width) :initial-element #\space) 0 y))
      (refresh-window))))

(defun running? ()
  *running*)

(defmacro with-charms ((&key (timeout 1000) (raw-input t) (interpret-control-characters nil)) &body body)
  ;; TODO: if we change parameters, live reload
  `(unwind-protect
        (let ((*handle* (init-charms ,timeout ,raw-input ,interpret-control-characters))
              (*running* t))
          (declare (type fouric-charms::terminal-handle *handle*))
          (update-dimensions)
          ,@body)
     (charms:finalize)))

(defun update-dimensions ()
  (multiple-value-bind (width height) (charms:window-dimensions (window *handle*))
    ;; ok so this is monumentally stupid BUT you apparently can't write to the cell in the very bottom right-hand corner without causing an error in charms...
    (setf (charms-width *handle*) (1- width)
          (charms-height *handle*) height)
    (values width height)))

(defmacro without-charms ((handle &key (drop-events t)) &body body)
  `(unwind-protect
        (let ((*running* nil))
          (check-type ,handle terminal-handle)
          (charms:finalize)
          (prog1
              (progn
                ,@body)
            (restart-charms ,handle t ,drop-events)))
     (charms:finalize)))

(defun repeatchar (char count)
  (if (zerop count)
      ""
      (concatenate 'string (list char) (repeatchar char (1- count)))))

(defun draw-box (x y w h)
  ;; usually takes no more than a few hundred microseconds per call, although the complexity does scale with the box size
  (let* ((w (min w (- (width) x)))
         (h (min h (- (height) y)))
         ;; make a string of entirely the horizontal line character, w units long
         (upper-left #\+)
         (upper-right #\+)
         (lower-left #\+)
         (lower-right #\+)
         (vertical "|")
         (horizontal (make-string w :initial-element #\-)))
    ;; set the first and last elements to be the upper left and right corners, respectively
    (setf (aref horizontal 0) upper-left
          (aref horizontal (1- w)) upper-right)
    ;; draw the top of the box
    (draw-string horizontal x (+ y 0))
    ;; then set the first and last elements to be the bottom characters
    (setf (aref horizontal 0) lower-left
          (aref horizontal (1- w)) lower-right)
    ;; and draw
    (draw-string horizontal x (+ y h -1))
    ;; we don't have a way to draw vertical lines, so we'll just loop
    (dotimes (i (- h 2))
      (draw-string vertical (+ x 0) (+ y i 1))
      (draw-string vertical (+ x w -1) (+ y i 1)))))

(defun window-dimensions (handle)
  (charms:window-dimensions (window handle)))

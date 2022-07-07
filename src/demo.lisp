(in-package :fouric-charms)

(defun frame-function (data)
  (symbol-macrolet ((x (getf data :x))
                    (y (getf data :y)))
    #+swank(let ((connection (or swank::*emacs-connection* (swank::default-connection)))) ;; (fouric:update-swank)
             (when connection
               (swank::handle-requests connection t)))

    (let ((char (get-char)))
      (when char
        (format t "got char ~s~%" char))
      (case char
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
        (#\latin_small_letter_l_with_bar ;; resize event!
         ;; refresh here
         )
        (t
         )))

    (clear-window)

    (draw (format nil "hello world! ~a ~a" (width) (height)) x y)

    (refresh-window)
    t))

(defun demo ()
  (with-charms (:timeout 1000 :raw-input t :interpret-control-characters nil)
    (let ((data '(:x 0 :y 0)))
      (loop :named main-loop :do
        (unless (frame-function data)
          (return-from main-loop))))))

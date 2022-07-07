(in-package :fouric-charms)

(defun time-fields-destructure (string)
  (let ((lines (loop :for line
                       :in (split-sequence:split-sequence #\newline string)
                     :collect (string-trim '(#\space) line))))
    (let ((real-time (read-from-string (first (split-sequence:split-sequence #\space (nth 1 lines)))))
          (total-run-time (read-from-string (first (split-sequence:split-sequence #\space (nth 2 lines)))))
          (cpu-cycles (read-from-string (remove #\comma (first (split-sequence:split-sequence #\space (nth 4 lines))) :test #'char=)))
          (consed (read-from-string (remove #\comma (first (split-sequence:split-sequence #\space (nth 5 lines))) :test #'char=))))
      (list :real-time real-time
            :total-run-time total-run-time
            :cpu-cycles cpu-cycles
            :consed consed))))

(defmacro time-fields (form)
  `(time-fields-destructure
    (with-output-to-string (*trace-output*)
      (time ,form))))

(defparameter foo nil)
(defparameter real-times nil)
(defparameter cycles nil)
(defparameter consed nil)
;; for 55926 characters drawn:
;;     1.802M bytes consed = 32.22 bytes/character
;;     55ms real-time = ~1us/char
;;     103M processor cycles = 1.8k cycles/char (on my older i3)
;; for 7612 chars:
;;     262K consed = 34.4 bytes/char
;;     130ms time = 17us/char (???)
;;     27M cycles = 3.5k cycles/char
;; for 3540 chars:
;;     131K consed = 37 bytes/char
;;     3ms time = 847us/char
;;     6.3M cycles = 1.8k cycles/char
(defun test ()
  (setf real-times nil
        cycles nil
        consed nil)
  (with-charms (:timeout 100 :raw-input t :interpret-control-characters nil)
    (loop :named main :do
      (progn
        (fouric:update-swank)
        ;; TODO: figure out why quitting isn't working
        (let ((char (get-char)))
          (let ((fields (time (time-fields
                               (progn
                                 (case char
                                   (#\q
                                    (print 'returning)
                                    (return-from main))
                                   (#\b
                                    (break))
                                   (#\latin_small_letter_l_with_bar ;; resize event!
                                    ;; refresh here
                                    (update-dimensions))
                                   (t
                                    (let ((w (- (width) 2))
                                          (h (- (height) 2))
                                          (cells 0))
                                      (loop :for x :to w
                                            :do
                                               (loop
                                                 :for y :to h
                                                 :do
                                                    (progn
                                                      (draw (if (evenp (+ x y (if (setf foo (not foo))
                                                                                  0
                                                                                  1)
                                                                          (if (evenp x)
                                                                              0
                                                                              1)))
                                                                #\@
                                                                #\space) x y)
                                                      (incf cells))))
                                      (format t "cells: ~a~%" cells))))
                                 (refresh-window)
                                 (clear-window))))))
            (push (getf fields :real-time) real-times)
            (push (getf fields :consed) consed)
            (push (getf fields :cpu-cycles) cycles))))))
  (setf real-times (sort real-times #'<))
  (setf consed (sort consed #'<))
  (setf cycles (sort cycles #'<)))

(defun emacsclient-eval (command)
  (funcall (intern "SHELL-COMMAND" :trivial-shell) (format nil "emacsclient --eval \"~a\"" command)))

(defun dev (quicklisp-system file-to-open package command)
  ;; thanks http://turtleware.eu/posts/cl-charms-crash-course.html
  (defvar *console-io* *terminal-io*)
  ;;(push #p"~/other-code/sly/slynk/" asdf:*central-registry*)
  ;;(asdf:load-system :slynk)
  (handler-case
      (asdf:load-system :swank)
    (asdf/find-component:missing-component ()
      (funcall (intern "QUICKLOAD" :ql) :swank)))
  (funcall (intern "CREATE-SERVER" :swank) :port 4005 :dont-close t)
  (funcall (intern "QUICKLOAD" :ql) (list quicklisp-system :trivial-shell) :silent t)
  (emacsclient-eval (format nil "(find-file \\\"~a\\\")" (namestring (asdf:system-relative-pathname quicklisp-system file-to-open))))
  (emacsclient-eval "(slime-connect \\\"localhost\\\" 4005)")
  (sleep .5)
  (emacsclient-eval (format nil "(funcall 'slime-repl-eval-string \\\"(in-package ~a)\\\")" package))
  (sleep .5)
  (emacsclient-eval (format nil "(funcall 'slime-repl-eval-string \\\"~a\\\")" command))
  (loop (sleep 1)))

(dev :fouric-charms "src/playground.lisp" :fouric-charms '(playground))

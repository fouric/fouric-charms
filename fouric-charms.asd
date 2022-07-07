;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:fouric-charms-asd
  (:use :cl :asdf))

(in-package :fouric-charms-asd)

(defsystem fouric-charms
  :name "fouric-charms"
  :version "0.0.0"
  :maintainer "fouric"
  :author "fouric"
  :license "GPLv3"
  :description "an interactive canvas for developing cl-charms applications"

  :serial t
  :depends-on (:fouric :trivial-shell :cl-charms :split-sequence)
  :pathname "src"
  :components ((:file "package")
               (:file "fouric-charms")
               (:file "playground")
               #++(:file "demo")))

(defpackage :fouric-charms
  (:use :cl)
  (:local-nicknames (:f :fouric))
  (:export
   #:eclamp
   #:out-of-bounds
   #:with-color
   #:draw-string
   #:draw-char
   #:draw
   #:colors-to-index
   #:normalize-color
   #:clear-window
   #:write-spaces-window
   #:refresh-window
   #:get-char
   #:init-charms
   #:with-charms
   #:without-charms
   #:restart-charms
   #:update-dimensions
   #:with-invert
   #:window
   #:charms-width
   #:charms-height
   #:height
   #:width
   #:color?
   #:*invert*
   #:*silently-clip*
   #:*foreground*
   #:*background*
   #:terminal-handle
   #:draw-box

   ;; constants
   #:+f1+ #:+f2+ #:+f3+ #:+f4+ #:+f5+ #:+f6+ #:+f7+ #:+f8+ #:+f9+ #:+f10+ #:+f11+ #:+f12+
   #:+up+ #:+down+ #:+left+ #:+right+
   #:+resize+
   #:+c-a+ #:+c-b+ #:+c-c+ #:+c-d+ #:+c-e+ #:+c-f+ #:+c-g+ #:+c-h/backspace+ #:+c-i+ #:+c-j+ #:+c-k+ #:+c-l+ #:+c-m+ #:+c-n+ #:+c-o+ #:+c-p+ #:+c-q+ #:+c-r+ #:+c-s+ #:+c-t+ #:+c-u+ #:+c-v+ #:+c-w+ #:+c-x+ #:+c-y+ #:+c-z+
   #:+delete+ #:+escape+ #:+newline+ #:+pageup+ #:+pagedown+ #:+home+ #:+end+ #:+insert+ #:+backspace+

   #:playground
   ))

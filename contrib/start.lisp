(ql:quickload "quicklisp-slime-helper")
(let ((swank::*loopback-interface* (sb-unix:unix-gethostname)))
  (swank:create-server :dont-close t))
(load "/piserv/piserv.asd")
(ql:quickload "piserv")
(piserv:start-server 8080)
(loop (sleep 5))

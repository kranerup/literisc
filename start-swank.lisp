(ql:quickload '(:swank) :silent t)
(swank:create-server :port 5556 :dont-close t)
(loop (sleep 1))
;;; let g:swank_port=5556

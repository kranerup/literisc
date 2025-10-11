(defvar *console-io* *terminal-io*)  ; Save before Swank redirects
(load #P"~/.local/share/nvim/lazy/slimv/slime/start-swank.lisp")
(loop (sleep 1))  ; Keep terminal from reading input

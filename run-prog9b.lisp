(asdf:load-system :literisc)
(use-package :lr-asm)
(use-package :lr-disasm)
(use-package :lr-emulator)
(in-package :cl-user)

(load "asm-prog10")

;;; setup virtual ptys: socat -d -d pty,raw,echo=0 pty,raw,echo=0
;;; run xmodem tranfers: sz -b -vv -X liteRISC_gh/tfr128.txt > /dev/pts/10 < /dev/pts/10
;; use one pty for the emulator side and the other for the xmodem side

(defparameter pty "/dev/pts/11")
(let* ((symtab *symtab*)
       (dmem (make-dmem 65536))
       (emul (make-emulator program dmem :shared-mem t :debug nil)))
       (run-with-curses-io emul pty symtab))

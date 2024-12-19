(asdf:load-system :literisc)
(load "macro-utilities")
(load "unit")
(load "charms-extra")
(use-package :lr-emulator)

(load "asm-prog9")

(defvar e nil)
(defvar dmem nil)
(setq dmem (make-dmem 65536))

(defvar xmodem-data
  (list
    SOH
    1 ; block nr
    254 ; inverted
    160 126 ; j -2
    2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
    221 ; 192 ; check sum
    EOT ))


(defun read-serial-sb (addr rd-data)
  (cond ((= (+ IO io-reg-rx-ready) addr) 1) ; always say we're ready
        ((= (+ IO io-reg-rx-data) addr) (pop xmodem-data))
        ((= (+ IO io-reg-tx-ready) addr) 1) ; always say we're ready
        ('t rd-data)))

;(defun write-serial-sb (addr wr-data)
;  (cond ((= (+ IO io-reg-rx-ready) addr) 1)
;        ((= (+ IO io-reg-rx-data) addr) (pop xmodem-data))

(setf e (make-emulator program dmem 8192 t))
(run-with-curses e *symtab* nil #'read-serial-sb)
(quit)


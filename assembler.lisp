;;; ===========================================================================
;;; =============================== assembler =================================
;;;(defpackage #:lr-asm
;;;  (:use :cl))
;;;(in-package #:lr-asm)

;;; lsb is returned at start of list
(defun integer-to-byte-list (data bytes)
  (reverse
    (loop :for i :downfrom (1- bytes) :to 0
          :collect (ldb (byte 8 (* i 8)) data))))

;;; --------------------------------------------

(defun most-negative-2sc (bits)
  (- (ash 1 (1- bits))))
(defun most-positive-2sc (bits)
  (1- (ash 1 (1- bits))))
(defun within-2sc (val bits)
  (and (<= val (most-positive-2sc bits))
       (>= val (most-negative-2sc bits))))

;;; returns a list with the byte sequence that encodes
;;; the immediate value of val.
(defun asm-immediate (val)
  (if (within-2sc val 7)
      (list (logand #x7f val))
      (if (within-2sc val 14)
          (list (logior #x80
                        (logand #x7f (ash val -7)))
                (logand #x7f val))
          (if (within-2sc val 21)
              (list (logior #x80 
                            (logand #x7f (ash val -14)))
                    (logior #x80 
                            (logand #x7f (ash val -7)))
                    (logand #x7f val))
              (list (logior #x80 
                            (logand #x7f (ash val -21)))
                    (logior #x80 
                            (logand #x7f (ash val -14)))
                    (logior #x80 
                            (logand #x7f (ash val -7)))
                    (logand #x7f val))))))

(defun mvi->r (imm r)
  (concatenate 'list
               (list (logior #x80 r))
               (asm-immediate imm)))
(defun mvi->a (imm)
  (if (within-2sc imm 4)
      (list (logior #x90 
                    (logand #xf imm)))
      (error "mvi-a value is too large: ~a" imm)))

(defun j (offs)
  (concatenate 'list
               (list #xa0)
               (asm-immediate offs)))

(defmacro def-jump (fname opcode)
  `(defun ,fname (offs)
     (concatenate 'list
                 (list (logior #xa0 ,opcode))
                 offs)))

(def-jump j     0)
(def-jump jlt   1)
(def-jump jge   2)
(def-jump jlo   3)
(def-jump jhs   4)
(def-jump jz    5)
(def-jump jnz   6)
(def-jump jlo-b 7)
(def-jump jhs-b 8)
(def-jump jz-b  9)
(def-jump jnz-b 10)
(def-jump jlo-w 11)
(def-jump jhs-w 12)
(def-jump jz-w  13)
(def-jump jnz-w 14)
(def-jump jsr   15)

(defmacro def-reg (fname opcode)
  `(defun ,fname (r)
     (list (logior ,opcode r))))

(def-reg a->r    #x00)
(def-reg r->a    #x10)
(def-reg sub-r   #xc0)
(def-reg add-r   #xb0)
(def-reg and-r   #xd0)
(def-reg or-r    #xe0)

;;; ----- load / store 32-bit words
(def-reg ld-a->r #x30)
(def-reg ld-r->a #x40)
(def-reg st-r->a #x60)
(def-reg st-a->r #x70)
(defun ld-a-rel->r (offs r)
  (concatenate 'list
               (list (logior #x20 r))
               (asm-immediate offs)))
(defun st-r->a-rel (offs r)
  (concatenate 'list
               (list (logior #x50 r))
               (asm-immediate offs)))

(defun ld.b-a->r (r)
  (concatenate 'list
               (list #xf8
                     (logior #x30 r))))

;;; ----- load / store bytes
(defun ld.b-r->a (r)
  (concatenate 'list
               (list #xf8
                     (logior #x40 r))))

(defun st.b-a->r (r)
  (concatenate 'list
               (list #xf8
                     (logior #x60 r))))

(defun st.b-r->a (r)
  (concatenate 'list
               (list #xf8
                     (logior #x70 r))))

(defun ld.b-a-rel->r (offs r)
  (concatenate 'list
               (list #xf8
                     (logior #x20 r))
               (asm-immediate offs)))

(defun st.b-r->a-rel (offs r)
  (concatenate 'list
               (list #xf8
                     (logior #x50 r))
               (asm-immediate offs)))

;;; ----- load / store 16-bit words

(defun ld.w-a->r (r)
  (concatenate 'list
               (list #xf8
                     (logior #xb0 r))))

(defun ld.w-r->a (r)
  (concatenate 'list
               (list #xf8
                     (logior #xc0 r))))

(defun st.w-a->r (r)
  (concatenate 'list
               (list #xf8
                     (logior #xe0 r))))

(defun st.w-r->a (r)
  (concatenate 'list
               (list #xf8
                     (logior #xf0 r))))

(defun ld.w-a-rel->r (offs r)
  (concatenate 'list
               (list #xf8
                     (logior #xa0 r))
               (asm-immediate offs)))

(defun st.w-r->a-rel (offs r)
  (concatenate 'list
               (list #xf8
                     (logior #xd0 r))
               (asm-immediate offs)))


(defmacro def-acc (fname opcode)
  `(defun ,fname ()
     (list ,opcode)))

(def-acc not-a    #xf0)
(def-acc lsl-a    #xf1)
(def-acc lsr-a    #xf2)
(def-acc asr-a    #xf3)
(def-acc pop-a    #xf7)
(def-acc mask-a-b #xfa)
(def-acc mask-a-w #xfb)
(def-acc sex-a-w  #xfc)
(def-acc sex-a-b  #xfd)
(def-acc j-a      #xfe)

(defun push-r (r)
  (list #xf4
        r))

(defun pop-r (r)
  (list #xf5
        r))

(defun stst-srp ()
  (list #xf6))

(defun push-srp ()
  (list #xf6))

;;; writes a list of bytes to memory at given address
(defun set-program (imem ilist &optional (start-adr 0))
  (let ((p start-adr))
    (loop for instr in ilist
          do (format t "M[~a] = ~a~%" p instr)
          do (progn (setf (aref imem p) instr)
                    (setf p (+ p 1))))))


(defmacro label (lbl)
  `(progn
     (defvar ,lbl #xffffffff)
     nil))

;;; Find all label and create global variables for all
;;; This avoids encountering forward declared labels as undefined symbols.
(defun define-labels (aprog verbose)
  (if verbose (format t "--- define labels ---~%"))
  (dolist (item aprog)
    (if (equal (car item) 'label)
        (progn
          ;(format t "label ~a~%" (cadr item))
          (eval item)))))

;;; Evaluate assembler instruction and if it is a jump
;;; then calculate the relative offset from the absoluate curr-pc.
;;; The curr-pc is pointing to the first byte/opcode of the instruction.
(defun eval-asm (instr curr-pc debug)
  (if (member (car instr)
              '(j jlt  jge  jlo  jhs  
                  jz jnz  jlo-b jhs-b jz-b 
                  jnz-b jlo-w jhs-w jz-w 
                  jnz-w jsr  ))
      (let* ((dest-pc (eval (cadr instr)))
             (imm-offs nil)
             (instr-len 2) ; initial size, at least opcode plus one offset byte
             (prev-len 2)
             (pc-after-instr (+ curr-pc instr-len))
             (offs (- dest-pc pc-after-instr)))
        (if debug (format t "label:~a pc-after-jmp:~a curr-pc:~a offs:~a~%"
                dest-pc
                pc-after-instr
                curr-pc
                offs))
        (setf imm-offs (asm-immediate offs))
        (if debug (format t "A: imm-offs:~a ilen:~a plen:~a~%" imm-offs instr-len prev-len))
        (setf instr-len (+ 1 (list-length imm-offs)))
        (if (not (equal instr-len prev-len))
            (progn ; the length increased so need to recalculate the offset
              (if debug (format t "B: imm-offs:~a ilen:~a plen:~a~%" imm-offs instr-len prev-len))
              (setf imm-offs (asm-immediate (- dest-pc (+ curr-pc instr-len))))
              (if (not (equal instr-len (+ 1 (list-length imm-offs))))
                  (progn
                    (setf imm-offs (concatenate 'list (list #x80) imm-offs))
                    (if debug (format t "C: imm-offs:~a ilen:~a plen:~a~%" imm-offs instr-len prev-len))))))
        (if debug (format t "dpc:~a offs:~a~%" dest-pc imm-offs))
        (if debug (print (list (car instr) imm-offs)))
        (funcall (car instr) imm-offs))
      (eval instr)))

;;; Iterate through the program and calculate each instructions size to
;;; determine position of each label. Labels are updated when encountered.
;;; Return true if any label changed position compared to previous values.
;;; All labels initial value is larger than 32 bits so that labels can only
;;; shrink when iterating multiple times.
(defun calc-labels (aprog &optional debug)
  (if debug (format t "--- calc labels ---~%"))
  (let ((curr-pc 0)
        (diff nil))
    (dolist (item aprog)
      (if (equal (car item) 'label)
          (progn
            (if debug (format t "label ~a pc:~a prev:~a~%" (cdr item) curr-pc (symbol-value (cadr item))))
            (setf diff (or diff (not (equal (symbol-value (cadr item)) curr-pc))))
            (setf (symbol-value (cadr item)) curr-pc))
          (progn
            (if debug (format t "mcode len:~a val:~a~%"
                              (list-length (eval-asm item curr-pc debug))
                              (eval-asm item curr-pc debug)))
            (setf curr-pc (+ curr-pc (list-length (eval-asm item curr-pc debug)) )))))
    diff))

;;; Calculate label positions until there are no more changes.
(defun minimize-labels (aprog verbose)
  (loop for i from 1 to 5 
        while (calc-labels aprog verbose)
        until (>= i 5)))

(defun assemble (aprog &optional (verbose nil))
  (define-labels aprog verbose)
  (minimize-labels aprog verbose)
  (if verbose (format t "--- assemble ---~%"))
  (loop with mcode := nil and curr-pc := 0
        for item in aprog
        if (listp item) do (setf mcode (eval-asm item curr-pc verbose))
        if (listp item) do (setf curr-pc (+ curr-pc (list-length mcode)))
        if (listp item) append mcode))


;;; macro assembler
(defmacro masm (&rest body)
  `(assemble (concatenate 'list ,@body)))
;;;---------- unit tests --------------------
;(defmacro part3 (reg)
;  `'(
;    (mvi->r 0 ,reg)))
;(masm (part1) (part2) (part3 3))

(deftest test-fw-jump ()
  (let ((p4asm '(
           (j l1)
           (a->r 1)
           (label l1)
           (a->r 2))))
    (check (equal
             '(#xa0 1 #x01 #x02)
             (assemble p4asm)))))

;;;---------- unit tests --------------------

(deftest test-jumps ()
  (let ((pjump '(
                #xa0 #x00 ; j 0
                #xa0 #x01 ; j 1
                #x00 ; skipped
                #xa0 #x02 ; j 2
                #x00 ; skipped
                #x00 ; skipped
                #x03)) ; target
        (pjump-asm
          (assemble
            '((j loffs0)
              (label loffs0)
              (j loffs1)
              (a->r 0)
              (label loffs1)
              (j loffs2)
              (a->r 0)
              (a->r 0)
              (label loffs2)
              (a->r 3)))))
    (check (equal pjump pjump-asm))))

;(run-prog pjump '() 10)                  

(deftest test-jsr ()
  (let ((pjsr (assemble
                '((jsr l1)
                  (r->a 1)
                  (label l1)
                  (r->a srp)
                  (j-a))))
        (mcode '(175 1 17 31 254)))
    (check (equal pjsr mcode))))

(deftest test-misc-asm ()
  (let ((asm-nolbl
          (concatenate 'list
                       (a->r 0)
                       (jsr '(1))
                       (a->r 0) ; skipped
                       (label l1)
                       (mvi->r 15 1)
                       (mvi->a (binary-to-signed-integer 11 4))
                       (st-a->r 1)
                       (st-r->a 1)
                       (st-r->a-rel 10 1)
                       (ld-r->a 1)
                       (ld-a->r 1)
                       (ld-a-rel->r 10 1)
                       (mvi->r #b00011110001110 2)
                       (r->a 1)
                       (r->a 2)
                       (sub-r 1)
                       (r->a 3)
                       (sub-r 3)
                       (jz (list (logand #x7f -6)))
                       (j  (list (logand #x7f -6)))
                       ))
        (mcode ; manually created machine code
          '( #x00 ; mv A,R0 - 0
             #xaf ; jsr #1  - 1
             #x01 ;
             #x00 ; skipped - 3
             #x81 ; mvi #0x0f, R1 - 4
             #x0f
             #x9b ; mvi #11, A
             #x71 ; st A,R1
             #x61 ; st R1,A
             #x51 ; st R1,A+#10
             #x0a ;
             #x41 ; ld R1,A
             #x31 ; ld A,R1
             #x21 ; ld A+#10,R1
             #x0a ;
             #x82 ; mvi #0b00011110001110, R2
             #x8f
             #x0e
             #x11 ; mv R1,A
             #x12 ; mv R2,A
             #xc1 ; sub R1,A
             #x13 ; mv R3,A  21
             #xc3 ; sub R3,A
             #xa5 ; jz #-6   23
             #x7a ;
             #xa0 ; j #-6    25
             #x7a ;
             ))
        (asm-labels
          (assemble 
            '(
              (a->r 0)
              (jsr l1)
              (a->r 0) ; skipped
              (label l1)
              (mvi->r 15 1)
              (mvi->a (binary-to-signed-integer 11 4))
              (st-a->r 1)
              (st-r->a 1)
              (st-r->a-rel 10 1)
              (ld-r->a 1)
              (ld-a->r 1)
              (ld-a-rel->r 10 1)
              (mvi->r #b00011110001110 2)
              (r->a 1)
              (label l2)
              (r->a 2)
              (sub-r 1)
              (label l3)
              (r->a 3)
              (sub-r 3)
              (jz l2)
              (j l3)
              ))))
    (check 
      (equal mcode asm-nolbl)
      (equal mcode asm-labels))))

(deftest test-long-jmp ()
  (let ((asm-prog '( 
              (a->r 0)
              (jsr 1)
              (a->r 0) ; skipped
              (mvi->r 15 1)
              (label l1)
              (mvi->a (binary-to-signed-integer 11 4))
              (j l1)
              (st-a->r 1)
              (st-r->a 1)
              (st-r->a-rel 10 1)
              (ld-r->a 1)
              (ld-a->r 1)
              (ld-a-rel->r 10 1)
              (mvi->r #b00011110001110 2)
              (r->a 2)
              (label l2)
              (sub-r 1)
              (r->a 3)
              (sub-r 3)
              (jz -5)
              (j -5)
              (j l3)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (a->r 0)
              (label l3)
              (j l1))))
    (check (equal (list-length (assemble asm-prog)) 58))))

(deftest test-hello-world-asm ()
  (let ((hello-world 
      (assemble 
        '( 
           ; --- main ---
           (mvi->r 0 0)
           (jsr prtstr)
           (label end)
           (j end)
           ; --- prtstr ---
           ; r0 - ptr to zero terminated string
           (label prtstr)
           (ld.b-r->a 0)
           (mask-a-b)
           (a->r 1)
           (mvi->r -1 2) ; ptr to I/O reg
           (mvi->a 0)
           (sub-r 1)
           (jz ret-prtstr)
           (r->a 1)
           (st-a->r 2)
           (mvi->a 1)
           (add-r 0)
           (a->r 0)
           (j prtstr)
           (label ret-prtstr)
           (r->a srp)
           (j-a))))
        (mcode '( 128 0 175 2 160 126 248 64 250 1 130 127 144 193 165 7 17 114 145 176 0 160 111 31 254)))
    (check (equal hello-world mcode))))
; (run-prog hello-world (string-to-mem "Hello World!") 200 t)

(deftest test-asm ()
  (test-fw-jump)
  (test-jumps)
  (test-jsr)
  (test-misc-asm)
  (test-long-jmp)
  (test-hello-world-asm))
  

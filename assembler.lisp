;;; ===========================================================================
;;; =============================== assembler =================================
(defpackage :lr-asm
  (:use :cl :unit :lr-opcodes)
  (:export :integer-to-byte-list
	   :binary-to-signed-integer
	   :bit-vector->integer
	   :integer->bit-vector
           :string-to-mem 
           :assemble
           :masm
           :set-program
           :hexdump
           :output-binary-file 
           :get-label

           :srp :sp
           :r0 
           :r1            
           :r2            
           :r3            
           :r4            
           :r5            
           :r6            
           :r7            
           :r8            
           :r9            
           :r10 :p0
           :r11 :p1
           :r12 :p2
           :r13 :p3

           :label
           :m

           :mvi->r 
           :mvi->a 
           :j    
           :jlt             
           :jge             
           :jlo             
           :jhs             
           :jz              
           :jnz             
           :jlo-b           
           :jhs-b           
           :jz-b            
           :jnz-b           
           :jlo-w           
           :jhs-w           
           :jz-w            
           :jnz-w           
           :jsr             
           :a->r 
           :r->a 
           :sub-r
           :add-r
           :and-r
           :or-r 
           :ld-a->r
           :ld-r->a
           :st-r->a
           :st-a->r
           :ld-a-rel->r
           :st-r->a-rel
           :ld.b-a->r
           :ld.b-r->a
           :st.b-a->r
           :st.b-r->a
           :ld.b-a-rel->r
           :st.b-r->a-rel
           :ld.w-a->r
           :ld.w-r->a
           :st.w-a->r
           :st.w-r->a
           :ld.w-a-rel->r
           :st.w-r->a-rel
           :not-a   
           :lsl-a    
           :A=A<<1
           :lsr-a    
           :A=A>>1
           :asr-a    
           :pop-a    
           :mask-a-b 
           :mask-a-w 
           :sex-a-w  
           :sex-a-b  
           :j-a      
           :push-r
           :pop-r
           :stst-srp
           :push-srp
           :A=M[Rx].b
           :A=M[Rx].w
           :A-=Rx
           :A+=Rx
           :A&=Rx
           :A\|=Rx
           :M[Rx]=A
           :M[Rx].b=A
           :M[Rx].w=A
           :Rx=M[A+n].b
           :M[A+n].b=Rx
           :Rx=M[A+n]
           :M[A+n]=Rx
           :A=Rx
           :Rx=A
           :A=
           :Rx=
           :M[A]=Rx
           :M[A].b=Rx
           :M[A].w=Rx
           :Rx=M[A].w
           :Rx=M[A].b
           :Rx=M[A]
           :A=M[Rx]

           :lcons
           :lstring
           :lalloc-bytes
           :lalloc-words
           :lalloc-dwords
           :abyte
           :aword
           :adword
           :lalign-dword 
           :lalign-word 
           :word-align
           :dword-align
           ))

(in-package :lr-asm)

(load-opcodes 
  (asdf:system-relative-pathname :literisc "rtl/literisc/cpu.py" ))

(defconstant R0  0 )
(defconstant R1  1 )
(defconstant R2  2 )
(defconstant R3  3 )
(defconstant R4  4 )
(defconstant R5  5 )
(defconstant R6  6 )
(defconstant R7  7 )
(defconstant R8  8 )
(defconstant R9  9 )
(defconstant R10 10) (defconstant P0 R10)
(defconstant R11 11) (defconstant P1 R11)
(defconstant R12 12) (defconstant P2 R12)
(defconstant R13 13) (defconstant P3 R13)
(defconstant srp 14)
(defconstant sp 15)

;;; lsb is returned at start of list
(defun integer-to-byte-list (data bytes)
  (reverse
    (loop :for i :downfrom (1- bytes) :to 0
          :collect (ldb (byte 8 (* i 8)) data))))

(defun binary-to-signed-integer (binary num-bits)
    (if (>= (ash binary (- 1 num-bits)) 1)
              (- binary (ash 1 num-bits))
                    binary))

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

;;; converts a string into a list of bytes and appends a 0
(defun string-to-mem (s)
  (loop for c in (append (coerce s 'list) '(#\Nul))
        append (list (char-code c) )))
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
              (if (within-2sc val 28)
                (list (logior #x80 
                              (logand #x7f (ash val -21)))
                      (logior #x80 
                              (logand #x7f (ash val -14)))
                      (logior #x80 
                              (logand #x7f (ash val -7)))
                      (logand #x7f val))
                (list (logior #x80 
                              (logand #x7f (ash val -28)))
                      (logior #x80 
                              (logand #x7f (ash val -21)))
                      (logior #x80 
                              (logand #x7f (ash val -14)))
                      (logior #x80 
                              (logand #x7f (ash val -7)))
                      (logand #x7f val)))))))

;;; -------------- build op codes -----------------
;;; reg/field are alias for the lower 4 bits in the opcode
(defun opc ( opcode &key (reg 0) (field 0) )
  (logior 
    (ash opcode 4)
    reg
    field))

(defun opci2 ( opcode &key (reg 0) (field 0) )
  (list (opc OPC_NEXT :field OPCI_NEXT)
        (opc opcode :field (logior reg field))))


;;; -------------- assembler instructions -----------------
(defun mvi->r (imm r)
  (cons (opc OPC_MVI :reg r)
        (asm-immediate imm)))
(setf (symbol-function 'Rx=) #'mvi->r)

(defun mvi->a (imm)
  (if (within-2sc imm 4)
      (cons (opc OPC_MVIA
                 :field (logand #xf imm))
            nil)
      (error "mvi-a value is too large: ~a" imm)))
(setf (symbol-function 'A=) #'mvi->a)

(defmacro def-jump (fname opcode)
  `(defun ,fname (offs)
     (concatenate 'list
                  (list (opc OPC_JMP :field ,opcode))
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
     (cons (opc ,opcode :reg r) nil)))

(def-reg a->r    OPC_A_Rx)
(setf (symbol-function 'Rx=A) #'a->r)
(def-reg r->a    OPC_RX_A)
(setf (symbol-function 'A=Rx) #'r->a)
(def-reg sub-r   OPC_SUB)
(setf (symbol-function 'A-=Rx) #'sub-r)
(def-reg add-r   OPC_ADD)
(setf (symbol-function 'A+=Rx) #'add-r)
(def-reg and-r   OPC_AND)
(setf (symbol-function 'A&=Rx) #'and-r)
(def-reg or-r    OPC_OR)
(setf (symbol-function 'A\|=Rx) #'or-r)

;;; ----- load / store 32-bit words
(def-reg ld-a->r OPC_LD_A)
(setf (symbol-function 'Rx=M[A]) #'ld-a->r)
(def-reg ld-r->a OPC_LD_RX)
(setf (symbol-function 'A=M[Rx]) #'ld-r->a)
(def-reg st-r->a OPC_ST_A)
(setf (symbol-function 'M[A]=Rx) #'st-r->a)
(def-reg st-a->r OPC_ST_RX) ;;; 7   st A,Rx  M[Rx].l = A
(setf (symbol-function 'M[Rx]=A) #'st-a->r)

(defun ld-a-rel->r (offs r)
  (cons (opc OPC_LD_A_OFFS :reg r)
        (asm-immediate offs)))
(setf (symbol-function 'Rx=M[A+n]) #'ld-a-rel->r)

(defun st-r->a-rel (offs r)
  (cons (opc OPC_ST_A_OFFS :reg r)
        (asm-immediate offs)))
(setf (symbol-function 'M[A+n]=Rx) #'st-r->a-rel )

;;; ----- load / store bytes
(defun ld.b-a->r (r)
  (opci2 OPCI2_LDB_A :reg r))
(setf (symbol-function 'Rx=M[A].b) #'ld.b-a->r)

(defun ld.b-r->a (r)
  (opci2 OPCI2_LDB_RX :reg r))
(setf (symbol-function 'A=M[Rx].b) #'ld.b-r->a)

(defun st.b-a->r (r)
  (opci2 OPCI2_STB_RX :reg r))
(setf (symbol-function 'M[Rx].b=A) #'st.b-a->r)

(defun st.b-r->a (r)
  (opci2 OPCI2_STB_A :reg r))
(setf (symbol-function 'M[A].b=Rx) #'st.b-r->a)

(defun ld.b-a-rel->r (offs r)
  (append (opci2 OPCI2_LDB_A_OFFS  :reg r)
        (asm-immediate offs)))
(setf (symbol-function 'Rx=M[A+n].b) #'ld.b-a-rel->r)

(defun st.b-r->a-rel (offs r)
  (append (opci2 OPCI2_STB_A_OFFS :reg r)
        (asm-immediate offs)))
(setf (symbol-function 'M[A+n].b=Rx) #'st.b-r->a-rel)

;;; ----- load / store 16-bit words

(defun ld.w-a->r (r)
  (opci2 OPCI2_LDW_A :reg r))
(setf (symbol-function 'Rx=M[A].w) #'ld.w-a->r)

(defun ld.w-r->a (r)
  (opci2 OPCI2_LDW_RX :reg r))
(setf (symbol-function 'A=M[Rx].w) #'ld.w-r->a)

(defun st.w-a->r (r)
  (opci2 OPCI2_STW_RX :reg r))
(setf (symbol-function 'M[Rx].w=A) #'st.w-a->r)

(defun st.w-r->a (r)
  (opci2 OPCI2_STW_A :reg r))
(setf (symbol-function 'M[A].w=Rx) #'st.w-r->a)

(defun ld.w-a-rel->r (offs r)
  (append (opci2 OPCI2_LDW_A_OFFS :reg r)
        (asm-immediate offs)))

(defun st.w-r->a-rel (offs r)
  (append (opci2 OPCI2_STW_A_OFFS :reg r)
        (asm-immediate offs)))


(defun opci (opcode)
  (logior (ash OPC_NEXT 4)
          opcode))

(defmacro def-acc (fname opcode)
  `(defun ,fname ()
     (cons (opci ,opcode) nil)))

(def-acc not-a    OPCI_NOT)
(def-acc lsl-a    OPCI_LSL)
(setf (symbol-function 'A=A<<1) #'lsl-a)
(def-acc lsr-a    OPCI_LSR)
(setf (symbol-function 'A=A>>1) #'lsr-a)
(def-acc asr-a    OPCI_ASR)
(def-acc pop-a    OPCI_POP_A)
(def-acc mask-a-b OPCI_MASKB)
(def-acc mask-a-w OPCI_MASKW)
(def-acc sex-a-w  OPCI_SEXW)
(def-acc sex-a-b  OPCI_SEXB)
(def-acc j-a      OPCI_J_A)

(defun push-r (r)
  (list (opc OPC_NEXT :field OPCI_PUSH_R)
        r))

(defun pop-r (r)
  (list (opc OPC_NEXT :field OPCI_POP_R)
        r))

(defun push-srp ()
  (cons (opc OPC_NEXT :field OPCI_PUSH_SRP) nil))

;;;-----------------------------------------------------------

;;; writes a list of bytes to memory at given address
(defun set-program (imem ilist &optional (start-adr 0))
  (let ((p start-adr))
    (loop for instr in ilist
          ;do (format t "M[~a] = ~a~%" p instr)
          do (progn (setf (aref imem p) instr)
                    (setf p (+ p 1)))))
  imem)

(defmacro label (lbl)
  `(progn
     (defvar ,lbl #xfffffff0) ; dword aligned so that we don't get alignment error first iteration with forward rerences
     nil))

;; ----------------------------------------------------------------------------
(defun abyte (b) (list (logand #xff b)))
(defun aword (w) (list (logand #xff w)
                       (logand #xff (ash w -8))))
(defun adword (w) (list (logand #xff w)
                        (logand #xff (ash w -8))
                        (logand #xff (ash w -16))
                        (logand #xff (ash w -24))))
(defun word-align (addr)
  (if (equal (logand 1 addr) 0)
      addr
      (1+ addr)))
             
(defun dword-align (addr)
  (if (equal (logand #b11 addr) 0)
      addr
      (+ addr (- 4 (logand #b11 addr)))))

(defun lcons (acar acdr)
  (concatenate 'list (aword acar) (aword acdr)))
(defun lstring (s)
  (string-to-mem s))
(defun lalloc-bytes (nr-bytes)
  (make-list nr-bytes :initial-element 0))
(defun lalloc-words (nr-words pc)
  (let* ((align-bytes (- (word-align pc) pc))
         (aligned-len (+ (* nr-words 2) align-bytes)))
    (lalloc-bytes aligned-len)))
(defun lalloc-dwords (nr-dwords pc)
  (let* ((align-bytes (- (dword-align pc) pc))
         (aligned-len (+ (* nr-dwords 4) align-bytes)))
    (lalloc-bytes aligned-len)))

(defun lalign-dword (dummy pc)
  (let* ((align-bytes (- (dword-align pc) pc)))
    (lalloc-bytes align-bytes)))

(defun lalign-word (dummy pc)
  (let* ((align-bytes (- (word-align pc) pc)))
    (lalloc-bytes align-bytes)))
;; ----------------------------------------------------------------------------
;;; Find all label and create global variables for all
;;; This avoids encountering forward declared labels as undefined symbols.
(defun define-labels (aprog verbose)
  (if verbose (format t "--- define labels ---~%"))
  (dolist (item aprog)
    (if (equal (car item) 'label)
        (progn
          (if verbose (format t "label ~a~%" (cadr item)))
          (eval item)
          (setf (symbol-value (cadr item)) #xfffffff0)
          ))))

;;; Evaluate assembler instruction and if it is a jump
;;; then calculate the relative offset from the absolute curr-pc.
;;; The curr-pc is pointing to the first byte/opcode of the instruction.
(defvar m) ; just used to indicate a macro in the assembler code
(defun eval-asm (instr curr-pc check-labels debug)
  (when debug (format t "pc:~a instr ~a ~a~%" curr-pc (car instr)
                      (if (equal (car instr) 'label)
                          (cadr instr)
                          "")))
  (when (and check-labels (equal (car instr) 'label))
    (when (not (equal curr-pc (symbol-value (cadr instr))))
      (format t "missmatch pc:~a label:~a ~a~^"
              curr-pc (cadr instr) (symbol-value (cadr instr)))))
  (cond ((member (car instr) '(j jlt  jge  jlo  jhs  
                                  jz jnz  jlo-b jhs-b jz-b 
                                  jnz-b jlo-w jhs-w jz-w 
                                  jnz-w jsr  ))
         (eval-asm-jump instr curr-pc debug))
        ((member (car instr) '(lalloc-words lalloc-dwords lalign-dword lalign-word))
         (funcall (car instr) (eval (cadr instr)) curr-pc))
        ((eq (car instr) 'm)
         (apply (cadr instr) (mapcar 'eval (cddr instr)) ))
        (t (eval instr))))

(defun eval-asm-jump (instr curr-pc debug)
  (let* ((dest-pc (eval (cadr instr)))
             (imm-offs nil)
             (instr-len 2) ; initial size, at least opcode plus one offset byte
             (prev-len 2)
             (pc-after-instr (+ curr-pc instr-len))
             (offs (- dest-pc pc-after-instr)))
        (if debug (format t "jump to label:~a pc-after-jmp:~a curr-pc:~a offs:~a~%"
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
        (funcall (car instr) imm-offs)))

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
            (when debug
            (when (not (equal (symbol-value (cadr item)) curr-pc))
              (format t "label ~a updated prev:~a new:~a~%" (cdr item) (symbol-value (cadr item)) curr-pc)))
            (setf (symbol-value (cadr item)) curr-pc))
          (progn
            (if debug (format t "item:~a~%" item))
            (if debug (format t "mcode len:~a val:~a~%"
                              (list-length (eval-asm item curr-pc nil debug))
                              (eval-asm item curr-pc nil debug)))
            (setf curr-pc (+ curr-pc (list-length (eval-asm item curr-pc nil debug)) )))))
    diff))

(defun list-labels (aprog)
  (dolist (item aprog)
    (if (equal (car item) 'label)
        (format t "label ~a ~a~%" (cdr item) (symbol-value (cadr item))))))

(defun create-symtab (aprog symtab)
  (dolist (item aprog)
    (if (equal (car item) 'label)
        (setf (gethash (symbol-value (cadr item)) symtab)
              (symbol-name (cadr item))))))

(defun get-label (label-name symtab)
  (let ((lbl (string-upcase label-name)))
    (maphash (lambda (addr label) 
               (when (equal label lbl) 
                 (return-from get-label addr)))
             symtab)))

(defun print-labels (symtab)
    (maphash (lambda (addr label) 
               (format t "~5d: ~a~%" addr label))
             symtab))

;;; Calculate label positions until there are no more changes.
(defun minimize-labels (aprog verbose)
  (loop for i from 1 to 10
        while (calc-labels aprog verbose)
        finally (when (= i 11)
                  (error "minimize labels didn't stabilize"))))

(defun assemble (aprog &optional (verbose nil) (symtab nil))
  (define-labels aprog verbose)
  (minimize-labels aprog verbose)
  (if symtab
      (progn
        (when verbose (list-labels aprog))
        (create-symtab aprog symtab)))
  (if verbose (format t "--- assemble ---~%"))
  (loop with mcode := nil and curr-pc := 0
        for item in aprog
        if (listp item) do (setf mcode (eval-asm item curr-pc t verbose))
        ;if (listp item) do (format t "pc:~a mcode:~a~%" curr-pc mcode)
        if (listp item) do (setf curr-pc (+ curr-pc (list-length mcode)))
        if (listp item) append mcode))


;;; macro assembler
(defmacro masm (symtab &rest body)
  `(assemble (concatenate 'list ,@body) nil ,symtab))

(defun hexdump-noascii (bytes &optional (prefix ""))
  "Print a hexadecimal dump of the given list of bytes.
   BYTES is a list of integers representing the bytes to dump.
   PREFIX is an optional string to prepend to each line."
  (loop for i from 0 by 16 below (length bytes) do
        (format t "~a~8,'0x: " prefix i)
        (loop for j from 0 below 16
              for b = (if (< (+ i j) (length bytes)) (nth (+ i j) bytes) 0)
              do (format t "~2,'0x " b))
        (format t "~%")))

(defun hexdump (bytes &optional (prefix ""))
  "Print a hexadecimal dump of the given list of bytes.
   BYTES is a list of integers representing the bytes to dump.
   PREFIX is an optional string to prepend to each line."
  (loop for i from 0 by 16 below (length bytes) do
        (format t "~a~8,'0x: " prefix i)
        ;; Print hex values
        (loop for j from 0 below 16
              for b = (if (< (+ i j) (length bytes)) (nth (+ i j) bytes) 0)
              do (format t "~2,'0x " b))
        ;; Print ASCII representation
        (format t " |")
        (loop for j from 0 below 16
              for b = (if (< (+ i j) (length bytes)) (nth (+ i j) bytes) 0)
              do (format t "~c" 
                        (if (and (>= b 32) (<= b 126))  ; Check if printable ASCII
                            (code-char b)
                            #\.)))
        (format t"|~%")))

(defun output-binary-file (mcode filename)
  (with-open-file (stream filename
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (write-sequence mcode stream)))

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
        (mcode '(175 1 17 30 254)))
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
        (mcode '( 128 0 175 2 160 126 248 64 250 1 130 127 144 193 165 7 17 114 145 176 0 160 111 30 254)))
    (check (equal hello-world mcode))))
; (run-prog hello-world (string-to-mem "Hello World!") 200 t)

(deftest test-asm ()
  (combine-results
    (test-fw-jump)
    (test-jumps)
    (test-jsr)
    (test-misc-asm)
    (test-long-jmp)
    (test-hello-world-asm)))
  

;;; -------------------------------------------------------------------------------
;;; 
;;;                     liteRISC instruction set
;;;                             v 0.4   
;;; 
;;; -------------------------------------------------------------------------------
;;; The idea was to create a compact instruction set like the early 8-bit
;;; processor but with 32-bit operations and a large general purpose
;;; register bank. It should also be efficient for compiling high-level
;;; languages which basically means efficient stack handling.
;;; Variable length instructions is choosen to get a memory efficient
;;; instruction set. Basically only immediate data is variable length.
;;; All instructions are encoded in one byte with the exception of immediate
;;; data which is variable length and follows the first byte in the instruction.
;;; 
;;; Issues:
;;;  - CC is not part of the register bank. Awkward to save CC on interrupt,
;;;  - no XOR instruction
;;;  - should mvi really sign extend? Loading constants with upper zeros but
;;;    with highest immediate bit set is not possible. Instead must add another
;;;    immediate byte with all zeros.
;;;  - document this:
;;;    But since ld/st can't do byte operations perhaps all should use word addresses.
;;;    - all memory accesses are 32-bit long
;;;    - memory addresses are byte addresses
;;;    - byte order is little endian (lsb is a lowest address)
;;;    - unaligned 32-bit access is never done. The lower 2 address bits
;;;      are masked before memory access, resulting in aligned access.
;;;  - jump offset is relative the first byte after the jump instruction.
;;;    - j #0 is therefore a NOP
;;;    - j #1 skips one byte after the jump instruction
;;;    - j #-1 jumps to last byte in jump instruction
;;;    - j #-2 jumps to first byte in jump insruction, i.e. an endless loop.
;;;  - byte operations like string ops seems inefficient
;;;    - have added byte ld/st to solve this
;;;  - no wait for interrupt instruction
;;;
;;;
;;;  +------------+
;;;  |    R0      |
;;;  +------------+
;;;  |    R1      |
;;;  +------------+
;;;  |    ...     |
;;;  +------------+
;;;  |  R14 ( SP) |
;;;  +------------+
;;;  |  R15 (SRP) |
;;;  +------------+
;;; 
;;; [ o o o o  r r r r ]
;;; 
;;; 0   mv  A,Rx                Rx = A
;;; 1   mv  Rx,A                A = Rx
;;; 
;;; 2   ld   A+#nn,Rx           Rx = M[A+nn].l
;;; 3   ld   A,Rx               Rx = M[A].l
;;; 4   ld   Rx,A               A = M[Rx].l
;;; 5   st   Rx,A+#nn           M[A+nn].l = Rx
;;; 6   st   Rx,A               M[A].l = Rx
;;; 7   st   A,Rx               M[Rx].l = A
;;; 
;;; ! One opcode for all. Size is determined by bit in each following byte. See below.
;;; 8   mvi #nn,Rx              Rx = sex(nn)
;;;     mvi #nnnn,Rx            Rx = sex(nnnn)
;;;     mvi #nnnnnn,Rx          Rx = sex(nnnnnn)
;;; 
;;; [ o o o o  n n n n ]
;;; 9   mvi #n,A                A = sex(n)
;;; 
;;; ! All jumps relative, offset variable size as in mvi.
;;; [ o o o o  f f f f ]
;;; 10 0        j   #nn                 jump always
;;;    1        jlt #nn                 jump <   signed      n ^ v
;;;    2        jge #nn                 jump >=  signed    !(n ^ v)
;;;    3        jlo #nn                 jump <   unsigned    c
;;;    4        jhs #nn                 jump >=  unsigned   !c
;;;    5        jz  #nn                 jump on zero         z
;;;    6        jnz #nn                 jump on not zero    !z
;;; 
;;;    7        jlo.b #nn               jump <   unsigned    c8
;;;    8        jhs.b #nn               jump >=  unsigned   !c8
;;;    9        jz.b  #nn               jump on zero         z8
;;;    10       jnz.b #nn               jump on not zero    !z8
;;; 
;;;    11       jlo.w #nn               jump <   unsigned    c16
;;;    12       jhs.w #nn               jump >=  unsigned   !c16
;;;    13       jz.w  #nn               jump on zero         z16
;;;    14       jnz.w #nn               jump on not zero    !z16
;;; 
;;;    15       jsr #nn                 SRP = PC; PC = PC + sex(nn)
;;; 
;;; 11  add Rx,A                A = A + Rx
;;; 12  sub Rx,A                A = A - Rx      sets CC = n v z c8 z8 c16 z16
;;; 13  and Rx,A                A = A & Rx
;;; 14  or  Rx,A                A = A | Rx
;;; 
;;; [ o o o o  f f f f ]
;;; o  ffff
;;; 15 0        not   A                 A = ~A
;;;    1        lsl   A                 c = A, A = A << 1, A<0> = 0
;;;    2        lsr   A                 c = A, A = A >> 1, A<31> = 0
;;;    3        asr   A                 c = A, A = A >> 1, A<31> = A<30>
;;;    4 see below
;;;    5 see below
;;;    6    stst  srp           M[sp].l = srp
;;;    7        popa                    a = M[sp].l, sp = sp + 4
;;;    8 
;;;       [ o o o o  r r r r ]
;;;       oooo
;;;       0-1 Unused
;;;       2    ld   A+#nn,Rx            Rx = M[A+nn].b
;;;       3    ld   A,Rx                Rx = M[A].b
;;;       4    ld   Rx,A                A = M[Rx].b
;;;       5    st   Rx,A+#nn            M[A+nn].b = Rx
;;;       6    st   Rx,A                M[A].b = Rx
;;;       7    st   A,Rx                M[Rx].b = A
;;;       8-15 Unused
;;;      
;;;    9 Unused
;;;    10       maskb A                 A = A & 0xff
;;;    11       maskw A                 A = A & 0xffff
;;;    12 sexb  A                       A<31:8>  = A<7>
;;;    13 sexw  A                       A<31:16> = A<15>
;;;    14       j     A                 PC = A
;;;    15 Unused
;;; 
;;; [ o o o o  f f f f ]
;;; [ 0 0 0 0  r r r r ]
;;; 15 4    push  R0..Rn            for (r=R0..Rn) { sp = sp - 4; M[sp].l=r;  }
;;;    5    pop   R0..Rn            for (r=Rn..R0) { r = M[sp].l; sp = sp + 4; }
;;; 
;;; 
;;; Variable size immediate data:
;;; 
;;; 7-bits
;;;   [    instr        ]
;;;   [ 0 n n n n n n n ]
;;; 
;;; 14-bits
;;;   [    instr        ]
;;;   [ 1 n n n n n n n ]
;;;   [ 0 m m m m m m m ] -> 0bnnnnnnnmmmmmmm
;;; 
;;; 21-bits
;;;   [    instr        ]
;;;   [ 1 n n n n n n n ]
;;;   [ 1 n n n n n n n ]
;;;   [ 0 n n n n n n n ]
;;; 
;;; 28-bits
;;;   [    instr        ]
;;;   [ 1 n n n n n n n ]
;;;   [ 1 n n n n n n n ]
;;;   [ 1 n n n n n n n ]
;;;   [ 0 n n n n n n n ]
;;; 
;;; 32-bits
;;;   [    instr        ]
;;;   [ 1 n n n n n n n ]
;;;   [ 1 n n n n n n n ]
;;;   [ 1 n n n n n n n ]
;;;   [ 1 n n n n n n n ]
;;;   [ 0 0 0 0 n n n n ]

;;; ===========================================================================
;;; ======================== emulator =========================================
(defstruct processor-state 
  R
  A
  PC
  N
  V
  Z
  C
  C8
  Z8
  C16
  Z16
  debug
  write-callback
  )
(defun make-processor ()
 (make-processor-state :R (make-array '(16))
                                    :PC 0
                                    :A 0
                                    ;-- CC --
                                    :n 0
                                    :v 0
                                    :z 0
                                    :c 0
                                    :c8 0
                                    :z8 0
                                    :c16 0
                                    :z16 0
                                    :debug t
                                    :write-callback nil))

; 1     mv  Rx,A                A = Rx
(defun i-mv-r (ps rx)
  (setf (processor-state-a ps)
         (aref (processor-state-r ps) rx))
  (if (processor-state-debug ps) (format t "mv A = R~a~%" rx)))

; 0     mv  A,Rx                Rx = A
(defun i-mv-a (ps rx)
  (setf (aref (processor-state-r ps) rx)
        (processor-state-a ps))
  (if (processor-state-debug ps) (format t "mv R~a = A~%" rx)))

; 8     mvi #nn,Rx              Rx = sex(nn)
; - imm must be signed extended before call
(defun i-mvi-rx (ps rx imm)
  (setf (aref (processor-state-r ps) rx) imm)
  (if (processor-state-debug ps) (format t "R~a = ~a~%" rx imm)))

; 9     mvi #n,A                A = sex(n)
(defun i-mvi-a (ps imm)
  (setf (processor-state-a ps) (sex imm 4))
  (if (processor-state-debug ps) (format t "A = ~a~%" imm)))

;    15 jsr #nn                 SRP = PC; PC = PC + sex(nn)
(defconstant SRP 15)
(defconstant SP 14)

(defun i-jsr (ps imm)
  (setf (aref (processor-state-r ps) SRP)
        (processor-state-pc ps))
  (setf (processor-state-pc ps)
        (+ (processor-state-pc ps)
           imm))
  (if (processor-state-debug ps)
      (format t "jsr srp:~a pc:~a~%"
        (aref (processor-state-r ps) SRP)
        (processor-state-pc ps))))

; A = A + Rx
(defun i-add (ps rx)
  (setf (processor-state-a ps)
        (logand #xffffffff
                (+
                  (processor-state-a ps)
                  (aref (processor-state-r ps) rx)))))

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


(defun overflow8 (a b)
  (let* ((ai (binary-to-signed-integer a 8))
         (bi (binary-to-signed-integer b 8))
         (res (- ai bi)))
    ;(format t "ai ~d bi ~d res ~d~%" ai bi res)
    (or (> res 127)
        (< res -128))))
        
(defun overflow (a b width)
  (let* ((ai (binary-to-signed-integer a width))
         (bi (binary-to-signed-integer b width))
         (res (- ai bi)))
    ;(format t "ai ~d bi ~d res ~d~%" ai bi res)
    (or (> res (- (ash 1 (- width 1)) 1))  ; highest pos 2s compl
        (< res (- (ash 1 (- width 1))))))) ; lowest neg 2s compl

; 12    sub Rx,A                A = A - Rx      sets CC = n v z c8 z8 c16 z16

(defun subn (a r width)
  (let* ((mask (1- (ash 1 width)))
         (am (logand a mask))
         (rm (logand r mask))
         (res (logand (- am rm) mask))
         (n (if (equal (logand (ash 1 (- width 1)) res) 0) 0 1))
         (z (if (equal res 0) 1 0))
         (c (if (< am rm) 1 0))
         (v (if (overflow am rm width) 1 0))) 
   (list res n z v c)))

(defun i-sub (ps rx)
  (let* ((a (processor-state-a ps))
         (r (aref (processor-state-r ps) rx))
         (s8 (subn a r 8))
         (s16 (subn a r 16))
         (s32 (subn a r 32)))
    (setf (processor-state-a ps) (car s32))
    (setf (processor-state-n ps) (nth 1 s32))
    (setf (processor-state-z ps) (nth 2 s32))
    (setf (processor-state-v ps) (nth 3 s32))
    (setf (processor-state-c ps) (nth 4 s32))
    (setf (processor-state-c8 ps) (nth 4 s8))
    (setf (processor-state-z8 ps) (nth 2 s8))
    (setf (processor-state-c16 ps) (nth 4 s16))
    (setf (processor-state-z16 ps) (nth 2 s16)))
  (if (processor-state-debug ps) (format t "A = A - R~a~%" rx)))

; A = A & Rx
(defun i-and (ps rx)
  (setf (processor-state-a ps)
        (logand
          (processor-state-a ps)
          (aref (processor-state-r ps) rx))))

; A = A | Rx
(defun i-or (ps rx)
  (setf (processor-state-a ps)
        (logior
          (processor-state-a ps)
          (aref (processor-state-r ps) rx))))

; A = ~A
(defun i-not (ps)
  (setf (processor-state-a ps)
        (lognor
          (processor-state-a ps)
          (processor-state-a ps))))

; A = A << 1
(defun i-lsl (ps)
  (setf (processor-state-a ps)
        (logand #xffffffff
                (ash (processor-state-a ps) 1))))

; A = A >> 1
(defun i-lsr (ps)
  (setf (processor-state-a ps)
        (ash (processor-state-a ps) -1)))

; A = A >> 1 arithmetic
(defun i-asr (ps)
  (let* ((a (processor-state-a ps))
        (sign (logand #x80000000 a))
        (shifted (ash a -1)))
        (setf (processor-state-a ps)
              (logior shifted sign))))

; maskb A, A = A & 0xff
(defun i-maskb (ps)
  (setf (processor-state-a ps)
        (logand #xff (processor-state-a ps)))
  (if (processor-state-debug ps) (format t "maskb A~%")))

; maskw A, A = A & 0xffff
(defun i-maskw (ps)
  (setf (processor-state-a ps)
        (logand #xffff (processor-state-a ps)))
  (if (processor-state-debug ps) (format t "maskw A~%")))

(defun sex (val width)
  (let* ((msb (- width 1))
         (sign (logand 1 (ash val (- msb))))
         (extend-mask (logxor #xffffffff (- (ash 1 width) 1)))
         (zero-mask (- (ash 1 width) 1)))
    ;(format t "sign ~a emask ~X zmask ~X msb ~a" sign extend-mask zero-mask msb)
    (if (equal 1 sign)
        (logior val extend-mask)
        (logand val zero-mask))))
  

; sexb  A, A<31:8>  = A<7>
(defun i-sexb (ps)
  (setf (processor-state-a ps)
        (sex (processor-state-a ps) 8)))

; sexw  A, A<31:16>  = A<15>
(defun i-sexw (ps)
  (setf (processor-state-a ps)
        (sex (processor-state-a ps) 16)))
 
(defun i-j-a (ps)
  (setf (processor-state-pc ps) (processor-state-a ps))
  (if (processor-state-debug ps) (format t "j A~%")))

(defun i-j (ps offset)
  (setf (processor-state-pc ps)
        (+ (processor-state-pc ps)
           offset))
  (if (processor-state-debug ps) (format t "j offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jz (ps offset)
  (if (equal (processor-state-z ps) 1)
      (let ((curr-pc (processor-state-pc ps)))
        (setf (processor-state-pc ps)
              (+ (processor-state-pc ps)
                 offset))
        (if (processor-state-debug ps) (format t "jz offs ~d c-pc:~d n-pc:~d~%" offset curr-pc (processor-state-pc ps))))
      (if (processor-state-debug ps) (format t "jz offs ~d skip~%" offset))))

(defun i-jz-b (ps offset)
  (if (equal (processor-state-z8 ps) 1)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jz.b offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jz-w (ps offset)
  (if (equal (processor-state-z16 ps) 1)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jz.w offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jnz (ps offset)
  (if (equal (processor-state-z ps) 0)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jnz offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jnz-b (ps offset)
  (if (equal (processor-state-z8 ps) 0)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jnz.b offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jnz-w (ps offset)
  (if (equal (processor-state-z16 ps) 0)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jnz.w offs ~d pc ~d~%" offset (processor-state-pc ps))))


(defun i-jhs (ps offset)
  (if (equal (processor-state-c ps) 0)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jhs offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jhs-b (ps offset)
  (if (equal (processor-state-c8 ps) 0)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jhs.b offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jhs-w (ps offset)
  (if (equal (processor-state-c16 ps) 0)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (format t "jhs.w offs ~d pc ~d~%" offset (processor-state-pc ps)))


(defun i-jlo (ps offset)
  (if (equal (processor-state-c ps) 1)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jlo offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jlo-b (ps offset)
  (if (equal (processor-state-c8 ps) 1)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jlo.b offs ~d pc ~d~%" offset (processor-state-pc ps))))

(defun i-jlo-w (ps offset)
  (if (equal (processor-state-c16 ps) 1)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jlo.w offs ~d pc ~d~%" offset (processor-state-pc ps))))

;;;    1        jlt #nn                 jump <   signed      n ^ v
(defun i-jlt (ps offset)
  (if (equal (logxor (processor-state-n ps)
                     (processor-state-v ps))
             1)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jlt offs ~d pc ~d~%" offset (processor-state-pc ps))))

;;;    2        jge #nn                 jump >=  signed    !(n ^ v)
(defun i-jge (ps offset)
  (if (equal (logxor (processor-state-n ps)
                     (processor-state-v ps))
             0)
    (setf (processor-state-pc ps)
          (+ (processor-state-pc ps)
             offset)))
  (if (processor-state-debug ps) (format t "jge offs ~d pc ~d~%" offset (processor-state-pc ps))))


(defun mem-read-dword (dmem addr)
  (let ((res (aref dmem addr)))
    (setf res (logior res 
                      (ash (aref dmem (+ addr 1)) 8)))
    (setf res (logior res 
                      (ash (aref dmem (+ addr 2)) 16)))
    (setf res (logior res 
                      (ash (aref dmem (+ addr 3)) 24)))
    res))

(defun mem-read-byte (dmem addr)
  (logand #xff (aref dmem addr)))

(defun mem-write-byte (dmem addr data &optional (write-callback nil))
  ;;(format t "in mem-write-byte ~a ~a~%" addr data)
  (if write-callback (funcall write-callback addr (logand #xff data)))
  (if (and (>= addr 0) (<= 65535))
      (setf (aref dmem addr) (logand #xff data))))

(defun mem-write-dword (dmem addr data &optional (write-callback nil))
  ;;(format t "in mem-write-dword ~a ~a~%" addr data)
  (if write-callback (funcall write-callback addr data))
  ;(if (equal (logand #xffffffff addr) #xffffffff)
  ;    (format t "~c" (code-char (logand #xff data)))
  (if (and (>= addr 0) (<= 65535))
      (progn
        (setf (aref dmem addr)
              (logand #xff data))
        (setf (aref dmem (+ addr 1))
              (logand #xff (ash data -8)))
        (setf (aref dmem (+ addr 2))
              (logand #xff (ash data -16)))
        (setf (aref dmem (+ addr 3))
              (logand #xff (ash data -24))))))

; 7     st   A,Rx               M[Rx].l = A
(defun i-st-rx-a (ps rx dmem)
  (mem-write-dword dmem (aref (processor-state-r ps) rx)
        (processor-state-a ps)
        (processor-state-write-callback ps))
  (if (processor-state-debug ps)
      (format t "st M[~a] = ~a~%"
          (aref (processor-state-r ps) rx)
          (processor-state-a ps))))

; 7     st   A,Rx               M[Rx].b = A
(defun i-st-b-rx-a (ps rx dmem)
  (mem-write-byte dmem (aref (processor-state-r ps) rx)
                  (processor-state-a ps)
                  (processor-state-write-callback ps))
  (if (processor-state-debug ps)
      (format t "st M[~a].b = ~a~%"
          (aref (processor-state-r ps) rx)
          (processor-state-a ps))))

; 6     st   Rx,A               M[A].l = Rx
(defun i-st-a-rx (ps rx dmem)
  (mem-write-dword dmem 
                   (processor-state-a ps)
                   (aref (processor-state-r ps) rx)
                   (processor-state-write-callback ps))
  (if (processor-state-debug ps)
      (format t "st M[~a] = ~a~%"
          (processor-state-a ps)
          (aref (processor-state-r ps) rx))))

; 6     st   Rx,A               M[A].b = Rx
(defun i-st-b-a-rx (ps rx dmem)
  (mem-write-byte dmem 
                  (processor-state-a ps)
                  (aref (processor-state-r ps) rx))
  (if (processor-state-debug ps)
      (format t "st M[~a].b = ~a~%"
          (processor-state-a ps)
          (aref (processor-state-r ps) rx))))

; 5     st   Rx,A+#nn           M[A+nn].l = Rx
(defun i-st-a-rx-imm (ps rx offs dmem)
  (mem-write-dword dmem 
                   (+ (processor-state-a ps) offs)
                   (aref (processor-state-r ps) rx)
                   (processor-state-write-callback ps))
  (if (processor-state-debug ps)
      (format t "st M[~a] = ~a~%"
          (+ (processor-state-a ps) offs)
          (aref (processor-state-r ps) rx))))

; 5     st   Rx,A+#nn           M[A+nn].b = Rx
(defun i-st-b-a-rx-imm (ps rx offs dmem)
  (mem-write-byte dmem 
                  (+ (processor-state-a ps) offs)
                  (aref (processor-state-r ps) rx)
                  (processor-state-write-callback ps))
  (if (processor-state-debug ps)
      (format t "st M[~a].b = ~a~%"
          (+ (processor-state-a ps) offs)
          (aref (processor-state-r ps) rx))))

; 2     ld   A+#nn,Rx           Rx = M[A+nn].l
(defun i-ld-a-rx-imm (ps rx offs dmem)
  (setf (aref (processor-state-r ps) rx)
        (mem-read-dword dmem 
              (+ (processor-state-a ps) offs)))
  (if (processor-state-debug ps)
      (format t "ld Rx = M[~a] = ~a~%"
          (+ (processor-state-a ps) offs)
          (aref (processor-state-r ps) rx))))

; 2     ld   A+#nn,Rx           Rx = M[A+nn].b
(defun i-ld-b-a-rx-imm (ps rx offs dmem)
  (setf (aref (processor-state-r ps) rx)
        (mem-read-byte dmem 
              (+ (processor-state-a ps) offs)))
  (if (processor-state-debug ps)
      (format t "ld Rx = M[~a].b = ~a~%"
          (+ (processor-state-a ps) offs)
          (aref (processor-state-r ps) rx))))

; 3     ld   A,Rx               Rx = M[A].l
(defun i-ld-a-rx (ps rx dmem)
  (setf (aref (processor-state-r ps) rx)
        (mem-read-dword dmem (processor-state-a ps)))
  (if (processor-state-debug ps) 
      (format t "ld Rx = M[~a] = ~a~%"
          (processor-state-a ps)
          (aref (processor-state-r ps) rx))))
          
; 3     ld   A,Rx               Rx = M[A].b
(defun i-ld-b-a-rx (ps rx dmem)
  (setf (aref (processor-state-r ps) rx)
        (mem-read-byte dmem (processor-state-a ps)))
  (if (processor-state-debug ps) 
      (format t "ld Rx = M[~a].b = ~a~%"
          (processor-state-a ps)
          (aref (processor-state-r ps) rx))))

; 4     ld   Rx,A               A = M[Rx].l
(defun i-ld-rx-a (ps rx dmem)
  (setf (processor-state-a ps)
        (mem-read-dword dmem (aref (processor-state-r ps) rx)))
  (if (processor-state-debug ps)
      (format t "ld A = M[~a] = ~a~%"
        (aref (processor-state-r ps) rx)
        (processor-state-a ps))))
  
; 4     ld   Rx,A               A = M[Rx].b
(defun i-ld-b-rx-a (ps rx dmem)
  (setf (processor-state-a ps)
        (mem-read-byte dmem (aref (processor-state-r ps) rx)))
  (if (processor-state-debug ps)
      (format t "ld A = M[~a].b = ~a~%"
        (aref (processor-state-r ps) rx)
        (processor-state-a ps))))

;    6    stst  srp             M[sp] = srp
(defun i-stst-srp (ps dmem)
  (mem-write-dword dmem (aref (processor-state-r ps) SP)
                   (aref (processor-state-r ps) SRP)
                   (processor-state-write-callback ps))
  (if (processor-state-debug ps)
      (format t "stst srp, M[~a] = ~a~%" 
          (aref (processor-state-r ps) SP)
          (aref (processor-state-r ps) SRP))))

;    6    push  srp             sp = sp -4, M[sp] = srp
(defun i-push-srp (ps dmem)
  (setf (aref (processor-state-r ps) SP)
        (- (aref (processor-state-r ps) SP) 4))
  (mem-write-dword dmem (aref (processor-state-r ps) SP)
                   (aref (processor-state-r ps) SRP)
                   (processor-state-write-callback ps))
  (if (processor-state-debug ps)
      (format t "push srp, M[~a] = ~a~%" 
          (aref (processor-state-r ps) SP)
          (aref (processor-state-r ps) SRP))))

; 15 4    push  R0..Rn            for (r=R0..Rn) { sp = sp - 4; M[sp]=r;  }
(defun i-push (ps rx dmem)
  (loop for r from 0 to rx
        do (progn
             (setf (aref (processor-state-r ps) SP)
                   (- (aref (processor-state-r ps) SP) 4))
             (mem-write-dword dmem (aref (processor-state-r ps) SP)
                   (aref (processor-state-r ps) r)
                   (processor-state-write-callback ps))
             (if (processor-state-debug ps)
                 (format t "push ~a = ~a sp ~a~%"
                     r 
                     (aref (processor-state-r ps) r)
                     (aref (processor-state-r ps) SP))))))
  
;    5    pop   R0..Rn            for (r=Rn..R0) { r = M[sp]; sp = sp - 4; }
(defun i-pop (ps rx dmem)
  (loop for r from rx downto 0 
        do (progn
             (setf (aref (processor-state-r ps) r)
                   (mem-read-dword dmem (aref (processor-state-r ps) SP)))
             (setf (aref (processor-state-r ps) SP)
                   (+ (aref (processor-state-r ps) SP) 4))
             (if (processor-state-debug ps)
                 (format t "pop ~a = ~a sp ~a~%"
                     r 
                     (aref (processor-state-r ps) r)
                     (aref (processor-state-r ps) SP))))))

;;;       7     popa                    a = M[sp], sp = sp + 4
(defun i-pop-a (ps dmem)
  (setf (processor-state-a ps)
        (mem-read-dword dmem (aref (processor-state-r ps) SP)))
  (setf (aref (processor-state-r ps) SP)
        (+ (aref (processor-state-r ps) SP) 4))
  (if (processor-state-debug ps)
      (format t "pop A = ~a sp ~a~%"
          (processor-state-a ps)
          (aref (processor-state-r ps) SP))))
  
(defun make-imem (nr-bytes)
  (make-array nr-bytes
    :element-type '(unsigned-byte 8)
    :initial-element 0))

(defun make-dmem (nr-words)
  (make-array nr-words
    :element-type '(unsigned-byte 32)
    :initial-element 0))

; Variable size immediate data:
; 
; 7-bits
;   [    instr        ]
;   [ 0 n n n n n n n ]
; 
; 14-bits
;   [    instr        ]
;   [ 1 n n n n n n n ]
;   [ 0 m m m m m m m ] -> 0bnnnnnnnmmmmmmm
; 
; 21-bits
;   [    instr        ]
;   [ 1 n n n n n n n ]
;   [ 1 n n n n n n n ]
;   [ 0 n n n n n n n ]
; 
; 28-bits
;   [    instr        ]
;   [ 1 n n n n n n n ]
;   [ 1 n n n n n n n ]
;   [ 1 n n n n n n n ]
;   [ 0 n n n n n n n ]
; 
; 32-bits
;   [    instr        ]
;   [ 1 n n n n n n n ]
;   [ 1 n n n n n n n ]
;   [ 1 n n n n n n n ]
;   [ 1 n n n n n n n ]
;   [ 0 0 0 0 n n n n ]
(defun get-immediate (p imem signed)
  (let* ((pc (processor-state-pc p))
         (imm (aref imem pc))
         (imm-end nil)
         (bits 0)
         (res 0))
    ;(format t "get-imm pc ~a imm ~a~%" pc imm)
    (loop do (progn
               (setf res (logior (ash res 7) (logand imm #x7f)))
               (setf imm-end (not (equal (logand imm #x80) #x80)))
               (setf pc (+ pc 1))
               (setf bits (+ bits 7))
               (setf imm (aref imem pc))
               (if nil (format t "res ~a pc ~a imm ~a bits ~a~%" res pc imm bits)))
          until imm-end)
    (setf (processor-state-pc p) pc)
    (if signed (setf res (binary-to-signed-integer res bits)))
    res))

(defun get-next-opcode (p imem)
  (let* ((pc (processor-state-pc p))
         (instr1 (aref imem pc))
         (opcode (ash instr1 -4))
         (param (logand instr1 15)))
    (setf (processor-state-pc p) (+ pc 1))
    (list opcode param)))
  

(defun execute-instruction (p imem dmem)
  (let* ((pc (processor-state-pc p))
         (instr0 (aref imem pc))
         (opcode (ash instr0 -4))
         (param (logand instr0 15)))
    (progn
      (if (processor-state-debug p) (format t "pc:~d opc:~2d p:~d " pc opcode param))
      (setf (processor-state-pc p) (+ pc 1))
      (cond ((equal opcode 0)
             (i-mv-a p param))
            ((equal opcode 1)
             (i-mv-r p param))
            ((equal opcode 2)
             (i-ld-a-rx-imm p param (get-immediate p imem nil) dmem))
            ((equal opcode 3)
             (i-ld-a-rx p param dmem))
            ((equal opcode 4)
             (i-ld-rx-a p param dmem))
            ((equal opcode 5)
             (i-st-a-rx-imm p param (get-immediate p imem nil) dmem))
            ((equal opcode 6)
             (i-st-a-rx p param dmem))
            ((equal opcode 7)
             (i-st-rx-a p param dmem))
            ((equal opcode 8)
             (i-mvi-rx p param (get-immediate p imem t)))
            ((equal opcode 9)
             (i-mvi-a p param))
            ((equal opcode 11)
             (i-add p param))
            ((equal opcode 12)
             (i-sub p param))
            ((equal opcode 10)
             (cond ((equal param 0)
                    (i-j p (get-immediate p imem t)))
                   ((equal param 1)
                    (i-jlt p (get-immediate p imem t)))
                   ((equal param 2)
                    (i-jge p (get-immediate p imem t)))
                   ((equal param 3)
                    (i-jlo p (get-immediate p imem t)))
                   ((equal param 4)
                    (i-jhs p (get-immediate p imem t)))
                   ((equal param 5)
                    (i-jz p (get-immediate p imem t)))
                   ((equal param 6)
                    (i-jnz p (get-immediate p imem t)))
                   ((equal param 7)
                    (i-jlo-b p (get-immediate p imem t)))
                   ((equal param 8)
                    (i-jhs-b p (get-immediate p imem t)))
                   ((equal param 9)
                    (i-jz-b p (get-immediate p imem t)))
                   ((equal param 10)
                    (i-jnz-b p (get-immediate p imem t)))
                   ((equal param 11)
                    (i-jlo-w p (get-immediate p imem t)))
                   ((equal param 12)
                    (i-jhs-w p (get-immediate p imem t)))
                   ((equal param 13)
                    (i-jz-w p (get-immediate p imem t)))
                   ((equal param 14)
                    (i-jnz-w p (get-immediate p imem t)))
                   ((equal param 15)
                    (i-jsr p (get-immediate p imem t)))))
            ((equal opcode 15)
             (cond ((equal param 0)
                    (i-not p))
                   ((equal param 1)
                    (i-lsl p))
                   ((equal param 2)
                    (i-lsr p))
                   ((equal param 3)
                    (i-asr p))
                   ((equal param 4)
                    (i-push p (get-immediate p imem nil) dmem))
                   ((equal param 5)
                    (i-pop p (get-immediate p imem nil) dmem))
                   ((equal param 6)
                    (i-push-srp p dmem))
                   ((equal param 7)
                    (i-pop-a p dmem))
                   ((equal param 8)
                    (destructuring-bind (opcode2 param2)
                      (get-next-opcode p imem)
                      (cond ((equal opcode2 2)
                             (i-ld-b-a-rx-imm o param2 (get-immediate p imem nil) dmem))
                            ((equal opcode2 3)
                             (i-ld-b-a-rx p param2 dmem))
                            ((equal opcode2 4)
                             (i-ld-b-rx-a p param2 dmem))
                            ((equal opcode2 5)
                             (i-st-b-a-rx-imm o param2 (get-immediate p imem nil) dmem))
                            ((equal opcode2 6)
                             (i-st-b-a-rx p param2 dmem))
                            ((equal opcode2 7)
                             (i-st-rx-a-b p param2 dmem))
                    )))
                   ((equal param 10)
                    (i-maskb p))
                   ((equal param 11)
                    (i-maskw p))
                   ((equal param 12)
                    (i-sexb p))
                   ((equal param 13)
                    (i-sexw p))
                   ((equal param 14)
                    (i-j-a p))))
            ))))

(defun write-cb-write-char (addr data)
  (if (equal (logand #xffffffff addr) #xffffffff)
      (format t "~c" (code-char (logand #xff data)))))

(defun run-prog ( prog-list dmem-list max-instr &optional (debug t) )
  (let ((the-prog prog-list)
        (imem (make-imem 500))
        (dmem (make-dmem 500))
        (p (make-processor)))
    (setf (processor-state-debug p) debug)
    (set-program imem the-prog)
    (set-program dmem dmem-list)
    (setf (processor-state-write-callback p) 'write-cb-write-char)
    (dotimes (n max-instr)
      (execute-instruction p imem dmem))
    (if debug (print p)))
  nil)

(defun string-to-mem (s)
  (loop for c in (append (coerce s 'list) '(#\Nul))
        append (list (char-code c) )))

(defstruct emulated-system
  imem
  dmem
  processor)

  
(defun make-emulator ( prog-list dmem max-instr &optional (debug t) )
  (let ((emul
          (make-emulated-system
            :imem (make-imem (+ (list-length prog-list) imem-padding))
            :dmem dmem
            :processor (make-processor))))
    (setf (processor-state-debug (emulated-system-processor emul)) debug)
    (setf (processor-state-write-callback
            (emulated-system-processor emul))
            'write-cb-write-char)
    (set-program (emulated-system-imem emul) prog-list)
    emul))

(defun run-emul ( emul max-instr &optional (debug t))
    (setf (processor-state-write-callback
            (emulated-system-processor emul))
          'write-cb-write-char)
    (dotimes (n max-instr)
      (execute-instruction
        (emulated-system-processor emul) 
        (emulated-system-imem emul)
        (emulated-system-dmem emul))))

(defvar *instr-row* 1)
(defvar *pc-row* 3)
(defvar *a-row* 4)
(defvar *reg-row* 5)
(defvar *cc-row* 22)

(setf *instr-row* 1)
(setf *pc-row* 3)
(setf *a-row* 4)
(setf *reg-row* 5)
(setf *cc-row* 22)

(defconstant imem-padding 7) ; disassembler reads ahead so need this much after end of program

(defun write-processor-state (window proc imem)
  (charms:write-string-at-point
    window
    (format nil "PC  ~8,'0x ~10d " 
            (processor-state-pc proc)
            (processor-state-pc proc))
    1 *pc-row*)
  (charms:write-string-at-point
    window
    (format nil "A   ~8,'0x ~10d " 
            (processor-state-a proc)
            (processor-state-a proc))
    1 *a-row*)
  (loop for r from 0 to 15 
     do (charms:write-string-at-point
          window
          (format nil "R~2a ~8,'0x ~10d "
                  r
                  (logand #xffffffff (aref (processor-state-r proc) r))
                  (logand #xffffffff (aref (processor-state-r proc) r)))
          1 (+ *reg-row* r)))
  (charms:write-string-at-point
    window
    (format nil "N V C Z8 C8 Z16 C16")
    1 *cc-row*)
  (charms:write-string-at-point
    window
    (format nil "~a ~a ~a ~a  ~a  ~a   ~a"
            (processor-state-n proc)
            (processor-state-v proc)
            (processor-state-c proc)
            (processor-state-z8 proc)
            (processor-state-c8 proc)
            (processor-state-z16 proc)
            (processor-state-c16 proc))
    1 (1+ *cc-row*))
  (let* ((mem-at-pc (coerce (subseq imem
                                    (processor-state-pc proc)
                                    (+ (processor-state-pc proc) imem-padding))
                            'list))
         (dis-str (str:trim-right
                    (with-output-to-string (*standard-output*)
                      (disasm mem-at-pc 1)))))
    (charms:write-string-at-point
      window
      (format nil "i: ~a" (str:substring 0 20 dis-str))
      1 *instr-row*)))

(defun write-disasm (window proc imem row)
  (let* ((pc (processor-state-pc proc))
         (mem-at-pc (coerce (subseq imem pc (+ pc 7)) 'list))
         (dis-str (str:trim-right
                    (with-output-to-string (*standard-output*)
                    (disasm mem-at-pc 1)))))
    (charms:write-string-at-cursor
      window
      (format nil "~4d: ~a~%" pc (str:substring 0 28 dis-str)))))

(defun write-cb-write-win (addr data window)
  (if (equal (logand #xffffffff addr) #xffffffff)
      (charms:write-string-at-cursor
        window
        (format nil "~c" (code-char (logand #xff data))))))

(defun run-with-curses ( emul )
  (setq *print-pretty* nil)
  (setf (processor-state-debug 
          (emulated-system-processor emul)) nil)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (charms:clear-window (charms:standard-window))
    (let ((disasm-window (charms:make-window 40 40 55 1))
          (output-window (charms:make-window 40 20 10 32))
          (cpu-window (charms:make-window 40 30 10 0)))
      (setf (processor-state-write-callback
              (emulated-system-processor emul))
            (lambda (addr data) (write-cb-write-win addr data output-window)))
      (charms:clear-window disasm-window)
      (charms:clear-window output-window)
      (charms:clear-window cpu-window)
      (draw-window-box cpu-window)
      (charms/ll:scrollok (charms::window-pointer disasm-window) 1)
      (charms/ll:scrollok (charms::window-pointer output-window) 1)
      (charms:enable-non-blocking-mode cpu-window)
      (charms:refresh-window disasm-window)
      (charms:refresh-window output-window)
      (charms:refresh-window cpu-window)
      (loop named emulate
            with single-step := nil and run := nil and update-windows := t
            do (progn
                 (if update-windows
                     (progn
                       (write-processor-state 
                         cpu-window
                         (emulated-system-processor emul)
                         (emulated-system-imem emul))
                       (charms:refresh-window cpu-window)
                       (write-disasm 
                         disasm-window
                         (emulated-system-processor emul)
                         (emulated-system-imem emul)
                         39)
                       (charms:refresh-window disasm-window)
                       ;(charms:refresh-window (charms:standard-window))
                       (charms:refresh-window output-window)
                       (setf update-windows nil)))

                 ;(case (charms:get-char charms:*standard-window* :ignore-error t)
                 (case (charms:get-char cpu-window :ignore-error t)
                   ((nil) nil)
                   ((#\Space) (setf single-step 1))
                   ((#\r) (setf run (not run)))
                   ((#\q #\Q) (return-from emulate)))
                 ;(sleep 0.1)
                 (if (or run single-step)
                     (progn
                       (setf update-windows t)
                       (execute-instruction
                         (emulated-system-processor emul) 
                         (emulated-system-imem emul)
                         (emulated-system-dmem emul))))
                 (setf single-step nil))))))


;;; =========================== unit test =====================================


;;; ------ test status ------------------
;;; Done?
;;; Y  0        mv  A,Rx                Rx = A
;;; Y  1        mv  Rx,A                A = Rx
;;; Y  2        ld   A+#nn,Rx           Rx = M[A+nn].l
;;; Y  3        ld   A,Rx               Rx = M[A].l
;;; Y  4        ld   Rx,A               A = M[Rx].l
;;; Y  5        st   Rx,A+#nn           M[A+nn].l = Rx
;;; Y  6        st   Rx,A               M[A].l = Rx
;;; Y  7        st   A,Rx               M[Rx].l = A
;;; Y  8        mvi #nn,Rx              Rx = sex(nn)
;;; Y  9        mvi #n,A                A = sex(n)
;;; Y  10 0     j   #nn                 jump always
;;; Y     1     jlt #nn                 jump <   signed      n ^ v
;;; Y     2     jge #nn                 jump >=  signed    !(n ^ v)
;;; Y     3     jlo #nn                 jump <   unsigned    c
;;; Y     4     jhs #nn                 jump >=  unsigned   !c
;;; Y     5     jz  #nn                 jump on zero         z
;;; Y     6     jnz #nn                 jump on not zero    !z
;;; Y     7     jlo.b #nn               jump <   unsigned    c8
;;; Y     8     jhs.b #nn               jump >=  unsigned   !c8
;;; Y     9     jz.b  #nn               jump on zero         z8
;;; Y     10    jnz.b #nn               jump on not zero    !z8
;;; Y     11    jlo.w #nn               jump <   unsigned    c16
;;; Y     12    jhs.w #nn               jump >=  unsigned   !c16
;;; Y     13    jz.w  #nn               jump on zero         z16
;;; Y     14    jnz.w #nn               jump on not zero    !z16
;;; Y     15    jsr #nn                 SRP = PC; PC = PC + sex(nn)
;;; Y  11       add Rx,A                A = A + Rx
;;; Y  12       sub Rx,A                A = A - Rx      sets CC = n v z c8 z8 c16 z16
;;; Y  13       and Rx,A                A = A & Rx
;;; Y  14       or  Rx,A                A = A | Rx
;;; Y  15 0     not   A                 A = ~A
;;; Y     1     lsl   A                 c = A, A = A << 1, A<0> = 0
;;; Y     2     lsr   A                 c = A, A = A >> 1, A<31> = 0
;;; Y     3     asr   A                 c = A, A = A >> 1, A<31> = A<30>
;;; Y     4    push  R0..Rn            for (r=R0..Rn) { sp = sp - 4; M[sp]=r;  }
;;; Y     5    pop   R0..Rn            for (r=Rn..R0) { r = M[sp]; sp = sp + 4; }
;;; Y     6    stst  srp                M[sp] = srp
;;; Y     7     popa                    a = M[sp], sp = sp + 4
;;; -     8
;;; -     9
;;; Y     10    maskb A                 A = A & 0xff
;;; Y     11    maskw A                 A = A & 0xffff
;;; Y     12 sexb  A                    A<31:8>  = A<7>
;;; Y     13 sexw  A                    A<31:16> = A<15>
;;; Y     14    j     A                 PC = A
;;; -     15
;;; -------------------------------------

;;; set registers to unique values and return the values as a list
;;; with A at the end
(defun init-reg-state (p)
  (loop with val := nil
        for r from 0 to 15
        ;do (setf val (* 256 (1+ r)))
        do (setf val (random #x100000000))
        do (setf (aref (processor-state-r p) r) val)
        collect val into ret-list
        finally (setf (processor-state-a p) (random #x100000000))
        finally (return (append ret-list (list (processor-state-a p))))))

(defun get-expected-a (expected)
  (nth 16 expected))
(defun set-expected-a (expected val)
  (setf (nth 16 expected) val))
(defun get-expected-r (expected r)
  (nth r expected))
(defun set-expected-r (expected r val)
  (setf (nth r expected) val))

#|
(defun check-state (p expected)
  (loop for r from 0 to 15
        do (assert (equal (get-expected-r expected r)
                          (aref (processor-state-r p) r)))
  (assert (equal (get-expected-a expected)
                 (processor-state-a p)))))
|#

(defun check-state (p expected)
  (and
    (every #'identity
           (loop for r from 0 to 15
                 collect (equal (get-expected-r expected r)
                                (aref (processor-state-r p) r))))
    (equal (get-expected-a expected)
           (processor-state-a p))))

(deftest test-mv ()
  (let* ((p (make-processor))
        (expected (init-reg-state p)))
    (check (progn
             ;--- r15->a
             (set-expected-a expected (get-expected-r expected 15))
             (i-mv-r p 15)
             (check-state p expected))
           (progn
             ;--- r4->a
             (set-expected-a expected (get-expected-r expected 4))
             (i-mv-r p 4)
             (check-state p expected))
           (progn
             ;--- a->r15
             (set-expected-r expected 15 (get-expected-a expected))
             (i-mv-a p 15)
             (check-state p expected)))))

(deftest test-add ()
  (let* ((p (make-processor))
        (expected (init-reg-state p)))
    (check
      (progn
        ;--- add r15,a
        (set-expected-a expected 
                        (logand #xffffffff
                                (+ (get-expected-a expected)
                                   (get-expected-r expected 15))))
        (i-add p 15)
        (check-state p expected))
      (progn
        ;-- add r15,a with overflow
        (let ((opa #xf1234567)
              (opb #xf7890123))
          (setf (aref (processor-state-r p) 15) opb)
          (set-expected-r expected 15 opb)
          (setf (processor-state-a p) opa)
          (set-expected-a expected 
                          (logand (+ opa opb) #xffffffff))
          (i-add p 15)
          ;(format t "a:~a e:~a~%" (processor-state-a p) (get-expected-a expected))
          (check-state p expected))))))

(deftest test-and-or ()
  (let* ((p (make-processor))
        (expected (init-reg-state p)))
    (check
      (progn
        ;--- and r15,a
        (set-expected-a expected 
                        (logand (get-expected-a expected)
                                (get-expected-r expected 15)))
        (i-and p 15)
        (check-state p expected))
      (progn
        ;--- or r14,a
        (set-expected-a expected 
                        (logior (get-expected-a expected)
                                (get-expected-r expected 14)))
        (i-or p 14)
        (check-state p expected)))))

(deftest test-shifts ()
  (let* ((p (make-processor))
        (expected (init-reg-state p)))
    (check
      (progn
        ;--- not a
        (set-expected-a expected 
                        (lognor (get-expected-a expected)
                                (get-expected-a expected)))
        (i-not p)
        (check-state p expected))
      (progn
        ;--- lsl a
        (set-expected-a expected 
                        (logand #xffffffff
                                (ash (get-expected-a expected) 1)))
        (i-lsl p)
        (check-state p expected))
      (progn
        ;--- lsr a
        (set-expected-a expected 
                        (ash (get-expected-a expected) -1))
        (i-lsr p)
        (check-state p expected))
      (progn
        ;--- asr a
        (setf (processor-state-a p) #x80000003)
        (set-expected-a expected #xc0000001)
        (i-asr p)
        (check-state p expected)))))

(deftest test-mask-sex ()
  (let* ((p (make-processor))
         (expected (init-reg-state p)))
    (check
      (progn
        ;--- maskb a
        (setf (processor-state-a p) #x800001ff)
        (set-expected-a expected #x000000ff)
        (i-maskb p)
        (check-state p expected))
      (progn
        ;--- maskw a
        (setf (processor-state-a p) #x8001ffff)
        (set-expected-a expected #x0000ffff)
        (i-maskw p)
        (check-state p expected))
      (progn
        ;--- sexb a
        (setf (processor-state-a p) #x12345681)
        (set-expected-a expected #xffffff81)
        (i-sexb p)
        (check-state p expected))
      (progn
        ;--- sexw a
        (setf (processor-state-a p) #x12348001)
        (set-expected-a expected #xffff8001)
        (i-sexw p)
        (check-state p expected)))))

(deftest test-j ()
  (let* ((p (make-processor))
         (expected (init-reg-state p)))
    (check
      (progn
        ;--- j A
        (setf (processor-state-a p) #xd1239876)
        (setf (processor-state-pc p) #x01234567)
        (i-j-a p)
        (equal #xd1239876
                       (processor-state-pc p)))
      (progn
        (setf (processor-state-pc p) 100)
        (i-j p 2)
        (equal 102 (processor-state-pc p)))
      (progn
        (setf (processor-state-pc p) 100)
        (i-j p -2)
        (equal 98 (processor-state-pc p))))))

(deftest test-jz ()
  (let* ((p (make-processor))
         (expected (init-reg-state p)))
    (check
      (progn
        ;--- jz taken
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #xd1239876)
        (setf (aref (processor-state-r p) 7) #xd1239876)
        (i-sub p 7) ; A = 0
        (equal (processor-state-a p) 0))
      (progn
        (i-jz p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))
      (progn
        ;--- jnz not taken
        (setf (processor-state-pc p) 1234)
        (i-jnz p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jz not taken
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 900)
        (setf (aref (processor-state-r p) 15) 899)
        (i-sub p 15) ; A = 1
        (equal (processor-state-a p) 1))
      (progn
        (i-jz p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jnz taken
        (setf (processor-state-pc p) 1234)
        (i-jnz p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jz.b taken
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #xd1239876)
        (setf (aref (processor-state-r p) 7) #x00000076)
        (i-sub p 7) ; A = 0xd1239800, lowest byte is 00
        (equal (processor-state-a p) #xd1239800))
      (progn
        (i-jz-b p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jnz.b not taken
        (setf (processor-state-pc p) 1234)
        (i-jnz-b p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jz.b not taken
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 900)
        (setf (aref (processor-state-r p) 15) 899)
        (i-sub p 15) ; A = 1
        (equal (processor-state-a p) 1))
      (progn
        (i-jz-b p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jnz.b taken
        (setf (processor-state-pc p) 1234)
        (i-jnz-b p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jz.w
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #xd1239876)
        (setf (aref (processor-state-r p) 7) #x00009876)
        (i-sub p 7) ; A = 0xd1230000, lowest word is 00
        (equal (processor-state-a p) #xd1230000))
      (progn
        (i-jz-w p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jnz.w not taken
        (setf (processor-state-pc p) 1234)
        (i-jnz-w p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jz.w not taken
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 900)
        (setf (aref (processor-state-r p) 15) 899)
        (i-sub p 15) ; A = 1
        (equal (processor-state-a p) 1))
      (progn
        (i-jz-w p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jnz.w taken
        (setf (processor-state-pc p) 1234)
        (i-jnz-w p 1000) ; take the jump
        (equal (processor-state-pc p) 2234)))))
 
(deftest test-jhs ()
  (let* ((p (make-processor))
         (expected (init-reg-state p)))
    (check
      (progn
        ;--- jhs taken when >
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 100000)
        (setf (aref (processor-state-r p) 7) 99999)
        (i-sub p 7) ; A = 1
        (equal (processor-state-a p) 1))
      (progn
        (i-jhs p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlo not taken when >
        (setf (processor-state-pc p) 1234)
        (i-jlo p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jhs taken when =
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 100000)
        (setf (aref (processor-state-r p) 7) 100000)
        (i-sub p 7) ; A = 0
        (equal (processor-state-a p) 0))
      (progn
        (i-jhs p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlo not taken when =
        (setf (processor-state-pc p) 1234)
        (i-jlo p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jhs not taken when <
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 100000)
        (setf (aref (processor-state-r p) 7) 100001)
        (i-sub p 7) ; A = -1 
        (equal (processor-state-a p) #xffffffff))
      (progn
        (i-jhs p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jlo taken when <
        (setf (processor-state-pc p) 1234)
        (i-jlo p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jhs.b taken when >
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #xfedcba70)
        (setf (aref (processor-state-r p) 7) #xfedcba6f)
        (i-sub p 7) ; A = 1
        (equal (processor-state-a p) 1))
      (progn
        (i-jhs-b p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlo.b not taken when >
        (setf (processor-state-pc p) 1234)
        (i-jlo-b p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jhs.b taken when =
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #xfedcba70)
        (setf (aref (processor-state-r p) 7) #x12345670)
        (i-sub p 7) ; A =  
        (equal (processor-state-a p) #xeca86400))
      (progn
        (i-jhs-b p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlo.b not taken when =
        (setf (processor-state-pc p) 1234)
        (i-jlo-b p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jhs.b not taken when <
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #xfedcba70)
        (setf (aref (processor-state-r p) 7) #xfedcba71)
        (i-sub p 7) ; A = -1 
        (equal (processor-state-a p) #xffffffff))
      (progn
        (i-jhs-b p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jlo.b taken when <
        (setf (processor-state-pc p) 1234)
        (i-jlo-b p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jhs.w taken when >
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #x02007000)
        (setf (aref (processor-state-r p) 7) #x03006fff)
        (i-sub p 7) ; A = 0xff000001 
        (equal (processor-state-a p) #xff000001))
      (progn
        (i-jhs-w p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlo.w not taken when >
        (setf (processor-state-pc p) 1234)
        (i-jlo-w p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jhs.w taken when =
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #xfedc7000)
        (setf (aref (processor-state-r p) 7) #x12347000)
        (i-sub p 7) ; A = xxx0000
        (equal (processor-state-a p) #xeca80000))
      (progn
        (i-jhs-w p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlo.w not taken when =
        (setf (processor-state-pc p) 1234)
        (i-jlo-w p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jhs.w not taken when <
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) #x09017000)
        (setf (aref (processor-state-r p) 7) #x08007001)
        (i-sub p 7) ; A = xxxxffff
        (equal (processor-state-a p) #x0100ffff))
      (progn
        (i-jhs-w p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jlo.w taken when <
        (setf (processor-state-pc p) 1234)
        (i-jlo-w p 1000) ; take the jump
        (equal (processor-state-pc p) 2234)))))

(deftest test-jge ()
  (let* ((p (make-processor))
         (expected (init-reg-state p)))
    (check
      (progn
        ;;;---- first with positive numbers ----
        ;--- jge taken when >
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 100000)
        (setf (aref (processor-state-r p) 7) 99999)
        (i-sub p 7) ; A = 1
        (equal (processor-state-a p) 1))
      (progn
        (i-jge p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlt not taken when >
        (setf (processor-state-pc p) 1234)
        (i-jlt p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jge taken when =
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 100000)
        (setf (aref (processor-state-r p) 7) 100000)
        (i-sub p 7) ; A = 0
        (equal (processor-state-a p) 0))
      (progn
        (i-jge p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlt not taken when =
        (setf (processor-state-pc p) 1234)
        (i-jlt p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jge not taken when <
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 100000)
        (setf (aref (processor-state-r p) 7) 100001)
        (i-sub p 7) ; A = -1 
        (equal (processor-state-a p) #xffffffff))
      (progn
        (i-jge p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jlt taken when <
        (setf (processor-state-pc p) 1234)
        (i-jlt p 1000) ; take the jump
        (equal (processor-state-pc p) 2234)))))

(deftest test-jge-signed ()
  (let* ((p (make-processor))
         (expected (init-reg-state p)))
    (check
      (progn
        ;;;---- then with one positive and one negative number ----
        ;--- jge taken when >
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) 100000)
        (setf (aref (processor-state-r p) 7) (logand #xffffffff -99999))
        (i-sub p 7) ; A = 199999
        (equal (processor-state-a p) 199999))
      (progn
        (i-jge p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlt not taken when >
        (setf (processor-state-pc p) 1234)
        (i-jlt p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jge taken when =
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) -100000)
        (setf (aref (processor-state-r p) 7) -100000)
        (i-sub p 7) ; A = 0
        (equal (processor-state-a p) 0))
      (progn
        (i-jge p 1000) ; take the jump
        (equal (processor-state-pc p) 2234))

      (progn
        ;--- jlt not taken when =
        (setf (processor-state-pc p) 1234)
        (i-jlt p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jge not taken when <
        (setf (processor-state-pc p) 1234)
        (setf (processor-state-a p) (logand #xffffffff -100000))
        (setf (aref (processor-state-r p) 7) 100001)
        (i-sub p 7) ; A = -200001 
        (equal (processor-state-a p) (logand #xffffffff -200001)))
      (progn
        (i-jge p 1000) ; skip the jump
        (equal (processor-state-pc p) 1234))

      (progn
        ;--- jlt taken when <
        (setf (processor-state-pc p) 1234)
        (i-jlt p 1000) ; take the jump
        (equal (processor-state-pc p) 2234)))))

(deftest test-jsr-regs ()
  (let* ((p (make-processor))
         (expected (init-reg-state p)))
    (check
      (progn
        (setf (processor-state-pc p) #x12345678)
        (i-jsr p 10)
        (equal (aref (processor-state-r p) 15) #x12345678)) ; SRP
      (progn
        (equal (processor-state-pc p) (+ #x12345678 10))))))
  
(deftest test-mvi ()
  (let* ((p (make-processor))
         (expected (init-reg-state p)))
    (check
      (progn
        ;--- mvi #n,A
        (i-mvi-a p #b0111)
        (set-expected-a expected 7)
        (check-state p expected))
      (progn
        ;--- negative
        (i-mvi-a p #b1000)
        (set-expected-a expected #xfffffff8)
        (check-state p expected))
      (progn
        ;--- mvi #n,Rx
        ; note that sex can't be tested here since it's done when
        ; extracting immediate constant
        (set-expected-r expected 15 234)
        (i-mvi-rx p 15 234)
        (check-state p expected)))))

(deftest test-ld ()
  (let* ((p (make-processor))
         (expected (init-reg-state p))
         (dmem (make-dmem 100)))
    (check
      (progn
        ;--- ld A,Rx 
        (set-expected-a expected 11)
        (setf (processor-state-a p) 11)
        (mem-write-dword dmem 11 #x04030201)
        (set-expected-r expected 4 #x04030201)
        (i-ld-a-rx p 4 dmem)
        (check-state p expected))
      (progn
        ;--- ld Rx,A
        (set-expected-r expected 5 13)
        (setf (aref (processor-state-r p) 5) 13)
        (mem-write-dword dmem 13 #xaabbccdd)
        (set-expected-a expected #xaabbccdd)
        (i-ld-rx-a p 5 dmem)
        (check-state p expected))
      (progn
        ;--- ld A+#nn,Rx
        (set-expected-a expected 11)
        (setf (processor-state-a p) 11)
        (mem-write-dword dmem 21 #x44332211)
        (set-expected-r expected 6 #x44332211)
        (i-ld-a-rx-imm p 6 10 dmem)
        (check-state p expected)))))

(deftest test-st ()
  (let* ((p (make-processor))
         (expected (init-reg-state p))
         (dmem (make-dmem 100)))
    (check
      (progn
        ;--- st Rx,A
        (set-expected-a expected 11)
        (setf (processor-state-a p) 11)
        (setf (aref (processor-state-r p) 5) #x04030201)
        (set-expected-r expected 5 #x04030201)
        (i-st-a-rx p 5 dmem)
        (equal (mem-read-dword dmem 11) #x04030201))
      (check-state p expected)
      (progn
        ;--- st A,Rx
        (set-expected-a expected #x44332211)
        (setf (processor-state-a p) #x44332211)
        (setf (aref (processor-state-r p) 6) 13)
        (set-expected-r expected 6 13)
        (i-st-rx-a p 6 dmem)
        (equal (mem-read-dword dmem 13) #x44332211))
      (check-state p expected)
      (progn
        ;--- st Rx,A+#nn
        (set-expected-a expected 11)
        (setf (processor-state-a p) 11)
        (setf (aref (processor-state-r p) 5) #x04030201)
        (set-expected-r expected 5 #x04030201)
        (i-st-a-rx-imm p 5 10 dmem)
        (equal (mem-read-dword dmem 21) #x04030201))
      (check-state p expected))))

(deftest test-push-pop ()
  (let* ((p (make-processor))
         (expected (init-reg-state p))
         (dmem (make-dmem 100)))
    (check
      (progn
        ;--- stst srp M[sp] = srp
        (setf (aref (processor-state-r p) SP) 50)
        (setf (aref (processor-state-r p) SRP) #x04030201)
        (set-expected-r expected SP 50)
        (set-expected-r expected SRP #x04030201)
        (i-stst-srp p dmem)
        (equal (mem-read-dword dmem 50) #x04030201))
      (progn
        ;--- push SRP
        (setf (aref (processor-state-r p) SP) 54)
        (setf (aref (processor-state-r p) SRP) #x04030201)
        (set-expected-r expected SP 50)
        (set-expected-r expected SRP #x04030201)
        (i-push-srp p dmem)
        (equal (mem-read-dword dmem 50) #x04030201))
      (check-state p expected)
      (progn
        ;--- push R0
        (i-push p 0 dmem)
        (set-expected-r expected SP 46)
        (check-state p expected)
        (equal (mem-read-dword dmem 46) (get-expected-r expected 0)))
      (progn
        ;--- push R2
        (i-push p 2 dmem)
        (set-expected-r expected SP 34)
        (check-state p expected))
      (equal (mem-read-dword dmem 42) (get-expected-r expected 0))
      (equal (mem-read-dword dmem 38) (get-expected-r expected 1))
      (equal (mem-read-dword dmem 34) (get-expected-r expected 2))
      (progn
        ;--- popa, a = M[sp], sp = sp + 4
        (setf (aref (processor-state-r p) SP) 50)
        (setf (aref (processor-state-r p) SRP) #x04030201)
        (set-expected-a expected #x04030201)
        (set-expected-r expected SP 54)
        (i-pop-a p dmem)
        (check-state p expected))
      (progn
        ;--- pop R0
        (mem-write-dword dmem 54 #xaabbccdd)
        (set-expected-r expected 0 #xaabbccdd)
        (set-expected-r expected SP 58)
        (i-pop p 0 dmem)
        (check-state p expected))
      (progn
        ;--- pop R2
        (mem-write-dword dmem 58 #x11111111)
        (mem-write-dword dmem 62 #x22222222)
        (mem-write-dword dmem 66 #x33333333)
        (set-expected-r expected 0 #x33333333)
        (set-expected-r expected 1 #x22222222)
        (set-expected-r expected 2 #x11111111)
        (set-expected-r expected SP 70)
        (i-pop p 2 dmem)
        (check-state p expected)))))

(deftest test-run-hello ()
  (let* ((hw-prog (assemble 
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
         (dmem (make-dmem 1000))
         (prog-output nil))
    (set-program dmem (string-to-mem "Hello World!"))
    (setf prog-output
          (with-output-to-string (*standard-output*)
            (run-emul (make-emulator hw-prog dmem 200 nil) 180)))
    (check (equal prog-output "Hello World!"))))


(deftest test-instructions ()
  (test-mv)
  (test-add)
  (test-and-or )
  (test-shifts )
  (test-mask-sex )
  (test-j )
  (test-jz )
  (test-jhs )
  (test-jge )
  (test-jge-signed )
  (test-jsr-regs )
  (test-mvi )
  (test-ld )
  (test-st )
  (test-push-pop )
  (test-run-hello))



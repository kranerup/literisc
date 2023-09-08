(ql:quickload :literisc)

;(defmacro part3 (reg)
;  `'(
;    (mvi->r 0 ,reg)))
;(masm (part1) (part2) (part3 3))

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

(defvar dmem nil)
(defvar dmem-allocated 0)
(setq dmem (make-dmem 1000))

(defmacro alloc-init ( sym data )
  `(progn
     (defvar ,sym nil)
     (setq ,sym dmem-allocated)
     (let* ((d ,data)
            (d-len (list-length d)))
       (set-program dmem d dmem-allocated)
       (setq dmem-allocated (+ dmem-allocated d-len)))
     (assert (< dmem-allocated (length dmem)))))

(defmacro alloc ( sym len )
  `(progn
     (defvar ,sym nil)
     (setq ,sym dmem-allocated)
     (setq dmem-allocated (+ dmem-allocated ,len))
     (assert (< dmem-allocated (length dmem)))))

(defun word-align (addr)
  (if (equal (logand 1 addr) 0)
      addr
      (1+ addr)))
             
(defun dword-align (addr)
  (if (equal (logand #b11 addr) 0)
      addr
      (+ addr (- 4 (logand #b11 addr)))))

;;; allocate 16-bit words, word aligned
(defmacro alloc-words (sym words)
  `(progn
     (defvar ,sym nil)
     (setq dmem-allocated (word-align dmem-allocated))
     (setq ,sym dmem-allocated)
     (setq dmem-allocated (+ dmem-allocated (* 2 ,words)))
     (assert (< dmem-allocated (length dmem)))))

;;; allocate 32-bit words, word aligned
(defmacro alloc-dwords (sym words)
  `(progn
     (defvar ,sym nil)
     (setq dmem-allocated (dword-align dmem-allocated))
     (setq ,sym dmem-allocated)
     (setq dmem-allocated (+ dmem-allocated (* 4 ,words)))
     (assert (< dmem-allocated (length dmem)))))

(defvar string-space-free 0) ; relative n-string-space

(defvar cons-free nil)
(defvar n-cons-end nil)

;;; cons-type: [ t:8 ]  (msb..lsb)
;;;   [ 0:4 | gc:1 | type:3 ]
;;;   type: 0 - free
;;;         1 - number
;;;         2 - symbol
;;;         3 - char
;;;         4 - cons
;;;         5 - macro
;;;         6 - function
;;;         7 - special function
;;;
;;; cons: [ f1:16 | f2:16 ]
;;;       [      f:32     ]
;;;
;;;          msb ... lsb
;;;       [   f1  |    f2 ]
;;; byte     3  2    1  0
;;;
;;;   type   |    f1    |     f2    |
;;;   -------+----------+-----------+
;;;   symbol | name-ptr | value-ptr |
;;;     name-ptr:string space ptr
;;;     value-ptr:cons cell ptr
;;;   cons   | cdr-ptr  | car-ptr   |
;;;     car/cdr-ptr: cons cell ptr
;;;          |          f           |
;;;   number |        value         |
;;;   char   |        value         |
;;;   
;;; cons pointers are relative indexes in the
;;; cons table with each entry being a complete cons
;;; cell (so not byte indexes but 4-bytes per index).

;;;
(defun cons-cell (f1 f2)
  (logior (logand #xFFFF f2)
          (ash (logand #xFFFF f1) 16)))
(defun cons-bytes ( cell )
  (integer-to-byte-list cell cons-size))

    
(defconstant c-cons-free      0)
(defconstant c-cons-number    1)
(defconstant c-cons-symbol    2)
(defconstant c-cons-char      3)
(defconstant c-cons-cons      4)
(defconstant c-cons-macro     5)
(defconstant c-cons-func      6)
(defconstant c-cons-spec-func 7)

(defvar n-stack-highest nil)

(defun init-lisp ()
  (setq dmem-allocated 0)
  (loop for idx from 0 to (1- (length dmem))
        do (setf (aref dmem idx) 0))
  ;;(alloc-init n-source-start     (string-to-mem "symbol symbol2 ( inner )"))
  ;;(alloc-init n-sym-string-start (string-to-mem "symbol"))
  (alloc n-source-start 32)
  (alloc n-sym-string-start 32)
  (alloc-init n-print-sep        (string-to-mem "---
"))
  (alloc n-read-sym-str 20)

  (alloc n-string-space 200)
  (setq string-space-free 0)

  (defparameter nr-cons 20)
  (defparameter cons-size 4) ; bytes

  (alloc-words n-cons-free 1) ; cons index to next free cons cell
  (set-program dmem '(0 0) n-cons-free)

  (alloc-dwords n-cons nr-cons)
  (setq n-cons-end dmem-allocated)
  (alloc n-cons-type nr-cons) ; holds type of each cons cell, one byte per cons
  (setq cons-free 0) ; points to entry nr in n-cons table

  ;; use-unread, last-read, read-ptr
  (alloc-dwords reader-state 3)
  (defparameter rs-use-unread 0)
  (defparameter rs-last-read 4)
  (defparameter rs-read-ptr 8)
  
  (alloc-dwords n-stack 100)
  (setq n-stack-highest (- dmem-allocated 4)))


(init-lisp)

;;; -- sym-strings --
(defun add-symbol (dmem sym-name value-ptr)
  (let ((str (string-to-mem sym-name))
        (str-ptr nil)
        (cons-ptr cons-free)) ; cons index, not address
    (set-program dmem str (+ n-string-space string-space-free))
    (setf str-ptr string-space-free)

    (set-program dmem (list c-cons-symbol )
                 (+ n-cons-type cons-ptr)) ; one byte per cons-type item
    ;(format t "ccell ~x cbytes ~a~%"
    ;        (cons-cell str-ptr value-ptr)
    ;        (cons-bytes (cons-cell str-ptr value-ptr)))

    (set-program dmem
                 (cons-bytes 
                   (cons-cell str-ptr value-ptr))
                 (+ n-cons (* cons-size cons-ptr)))

    (setf cons-free (1+ cons-free))
    (setf (aref dmem n-cons-free) (logand #xff cons-free))
    (setf (aref dmem (1+ n-cons-free)) (ash cons-free -8))
    (setq string-space-free (+ string-space-free
                               (list-length str)))))

;(set-program dmem (string-to-mem "symbol2") (+ (* sym-length n-sym-strings)))

;(set-program dmem (string-to-mem "symbol next ( inside ) end") source-start)
;(set-program dmem (string-to-mem "symbol") source-start)
;(set-program dmem (string-to-mem "symbol") sym-string-start)


;;; read-c keeps it's state in global memory variables relative
;;; reader-state base address.
;;; 
;;; R0 - reader-state base address
;;;    read/updates the following as offset from base address
;;;    - use-unread (updated)
;;;    - last-read (updated)
;;;    - read-ptr (updated)
;;; P0 - returns char
(defvar read-c nil)
(setq read-c
  '( (label l-read-c)
     (push-r R2)
     (mvi->r reader-state R0)
     (r->a R0) ; A = R0 base-ptr
     (ld-a-rel->r rs-use-unread R1) ; R1 = M[base-ptr+use-unread-offs]
     ;; if use-unread == 0
     (mvi->a 0)
     (sub-r R1)
     (jnz l-use-unread)
     ;; read-char = M[ read-ptr ]
     (r->a R0) ; base-ptr
     (ld-a-rel->r rs-read-ptr R2) ; R2 (read-ptr) = M[ A(base) + read-ptr-offs ]
     (ld.b-r->a R2) ; P0 = A = M[ R2 (read-ptr) ]
     (a->r P0)
     ;; read-ptr += 1
     (mvi->a 1)
     (add-r R2)
     (a->r R2)
     ;; save read-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R2) ; M[ A(base) + read-ptr-offs ] = R2 (read-ptr)
     ;; update last-read
     ;;   last-read = read-char
     ;; A (base-ptr)
     (st-r->a-rel rs-last-read P0) ; M[ A(base) + last-read-offs ] = P0 (last-read)
     ;; return
     (pop-r R2)
     (r->a srp)
     (j-a)

     ;; use-unread == 1
     (label l-use-unread)
     ;; clear use-unread
     (mvi->r 0 R1) ; R1 = use-unread = 0
     (r->a R0) ; A = R0 base-ptr
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     ;; read-char = last-read
     (ld-a-rel->r rs-last-read P0) ; P0 = M[ A(base) + last-read-offs ]
     ;; return
     (pop-r R2)
     (r->a srp)
     (j-a)

     ))

(defvar func-find-symbol nil)
(setq func-find-symbol
  '( ;; find-symbol
     ;; P0 - string-ptr, searched symbol
     ;;    - returns true if found
     ;; P1 - returns cons-ptr to symbol
     (label l-find-symbol)
     (push-srp)
     (push-r R5)

     ;; R0 = string-ptr to searched symbol
     ;; R1 = cons-rel-ptr - relative offset
     ;; R2 = cons-table-ptr - absolute address
     ;; R3 = string-space
     ;; R4 = tmp
     ;; R5 = cons-end
     (r->a P0) (a->r R0)
     (mvi->r 0 R1)
     (mvi->r n-cons R2)
     (mvi->r n-string-space R3) ; strings
     (mvi->r n-cons-end R5)

     (label l-next-cons)
     ;; R4 = n-cons + 2
     (r->a R2)
     (a->r R4)
     (mvi->a 2) ; high word (cons-f1)
     (add-r R4)
     (a->r R4)
     ;; read cdr
     (ld.w-r->a R4) ; cdr -> sym-name-ptr
     (add-r R3) ; string-space + sym-name-ptr
     (a->r P1) ; P1 = string-ptr in sym-table
     (r->a R0) (a->r P0)
     ;; P0 = string-ptr searched symbol
     (jsr l-str-equal)

     (mvi->a 0)
     (sub-r P0)
     (jnz l-found-sym)

     ;; cons-rel-ptr += 1
     (mvi->a 1)
     (add-r R1)
     (a->r R1)

     ;; cons-ptr += 4
     (mvi->a 4)
     (add-r R2)
     (a->r R2)

     (sub-r R5) ; equal end of cons table
     (jlo l-next-cons) ; A < cons-end
     ;; end
     (mvi->a 0)
     (a->r P0)
     (j l-ret-find-sym)

     (label l-found-sym) 
     (mvi->a 1)
     (a->r P0) ; P0 = 1, return flag
     (r->a R1)
     (a->r P1) ; P1 = cons-ptr, return sym-cons
    
     (label l-ret-find-sym)
     (pop-r R5)
     (pop-a)
     (j-a)
     ))

(defconstant reader-lpar 1)
(defconstant reader-rpar 2)
(defconstant reader-sym 3)
(defconstant reader-num 4)

;;; output:
;;;   P0 - returns read object type
;;;   P1 - number if type is reader-num
;;;   if type is reader-sym then symbol string is in n-read-sym-str
(defvar scan nil)
(setq scan
  '(
    (label f-scan)
    (push-srp)
    (push-r R4)
    ;; R1 - read-sym-ptr
    (mvi->r n-read-sym-str R1)
    ;; R3 - io-ptr
    (mvi->r -1 R3) ; ptr to I/O reg
    ;; R4 - reading-symbol
    (mvi->r 0 R4)

    (label l-rd-more)
    (jsr l-read-c)
    ;; R0 = P0 = char
    (r->a P0) (a->r R0)

    ;; --- if reading-symbol -----
    (mvi->a 0)
    (sub-r R4)
    (jz l-not-rd-sym)

    ;; ----------- reading-symbol --------------
    ;; end of symbol?
    ;; if c in ['(',')',' ',0]:  
    (mvi->r (char-code #\( ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym-unrd)
    (mvi->r (char-code #\) ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym-unrd)
    (mvi->r (char-code #\  ) R2)
    (r->a R2)
    (sub-r R0)
    (jz l-end-of-sym)
    (mvi->a 0)
    (sub-r R0)
    (jnz l-read-sym-char)

    (label l-end-of-sym-unrd)
    ;; unread-c
    (mvi->r reader-state R2) ; reader-state base-ptr
    (r->a R2) ; A = base-ptr
    ;; last-read = char
    (st-r->a-rel rs-last-read R0) ; M[ A(base) + last-read-offs ] = R0 (char)
    (mvi->r 1 R2) ; use-unread = 1
    (st-r->a-rel rs-use-unread R2) ; M[ A(base) + use-unread-offs ] = R2 (1)

    (label l-end-of-sym)

    ;; try to convert string to number
    (mvi->r n-read-sym-str P0) ; string-ptr = start of read sym buffer
    (jsr l-str2num) ; P0=string-ptr -> P0=num-flag P1=num
    (mvi->a 0)
    (sub-r P0)
    (jz l-ret-sym)
    ;; return num
    (mvi->r reader-num P0)
    ;; P1 is already num
    (j l-reader-ret)
    
    ;; return sym
    (label l-ret-sym)
    (mvi->r reader-sym P0)
    (j l-reader-ret)
   
    ;; else ------- not reading-symbol ---------
    (label l-not-rd-sym)
    ;;  if char == '('
    (mvi->r (char-code #\( ) R2)
    (r->a R2)
    (sub-r R0)
    (jnz l-not-lpar)
    ;;   then return lpar
    (mvi->r reader-lpar P0)
    (j l-reader-ret)
    
    (label l-not-lpar)
    ;;  if char == ')'
    (mvi->r (char-code #\) ) R2)
    (r->a R2)
    (sub-r R0)
    (jnz l-not-rpar)
    ;;   then return rpar
    (mvi->r reader-rpar P0)
    (j l-reader-ret)
   
    (label l-not-rpar)
    ;; is it a space then just read next char
    (mvi->r (char-code #\ ) R2)
    (r->a R2)
    (sub-r R0) ; ' ' - R0
    (jz l-rd-more) ; loop back and read next char

    ;; all other chars are written to sym buffer
    (label l-read-sym-char)
    (r->a R0)
    (st-a->r R1) ; M[read-sym-str] = char
    (mvi->a 1)
    (add-r R1) ; read-sym-str++
    (a->r R1)
    ;; debug print
    ;(r->a R0)
    ;(st-a->r R3); putchar
    ;; reading-symbol = True
    (mvi->r 1 R4)
    (j l-rd-more) ; loop back and read next char

    ;; return
    (label l-reader-ret)
    (pop-r R4)
    (pop-a)
    (j-a)
    
    ))

;;; parse (input from scan):
;;;   if '('
;;;     return list() 
;;;   elif \'
;;;     return quote()
;;;   else
;;;     return atom()
;;;
;;; atom:
;;;   return symbol / number
;;;   symbol = cons-cell with sym-type
;;;   number = cons-cell with num-type
;;;   if symbol found return existing symbol
;;;   else create a new symbol
;;; list:
;;;    if ')'
;;;       return nil
;;;    else
;;        return cons( sym, list() )

(defvar func-str2num nil)
(setq func-str2num
  '( ;; --- str2num ---
     ;; params:  P0 = string-ptr
     ;; returns: P0 = true if number
     ;;          P1 = number
     (label l-str2num)
     (push-r R2)
     (mvi->r 0 P1) ; result

     (label l-num-loop)
     (ld.b-r->a P0)
     (a->r R0) ; the char
     (mvi->a 0)
     (sub-r R0)
     (jz l-num-end)
     
     (mvi->r (1+ (char-code #\9 )) R1)
     (r->a R0)
     (sub-r R1) ; C - '9'
     (jhs-b l-no-num)

     (mvi->r (char-code #\0 ) R1)
     (r->a R0)
     (sub-r R1) ; C - '0'
     (jlo-b l-no-num)

     ; digit, then convert to int
     ; but that is already done: A = C - '0'
     ; result *= 10 -> res << 3 + res + res
     (a->r R0) ; int-digit
     (r->a P1)
     (lsl-a)
     (lsl-a)
     (lsl-a)
     (add-r P1)
     (add-r P1) ; result*10
     (add-r R0) ; + int-digit
     (a->r P1)
     ;; string-ptr++
     (mvi->a 1)
     (add-r P0)
     (a->r P0)
     (j l-num-loop)

     (label l-no-num)
     (mvi->a 0)
     (a->r P0) ; P0 = false
     (j l-num-ret)
     
     (label l-num-end) 
     (mvi->a 1)
     (a->r P0) ; P0 = true

     (label l-num-ret)
     (pop-r R2)
     (r->a SRP)
     (j-a)))
     
(defvar func-parse nil)
(setq func-parse
  '( ;; --- parse ---
     ;; P0 - object type from scan()
     ;; P0 - returns cons ptr to read objects
     (label l-parse)
     (push-srp)
     (push-r R3)

     (mvi->a reader-lpar)
     (sub-r P0)
     (jnz l-parse-nxt)

     ;; '('
     (jsr l-list) ; -> P0
     (j l-parse-ret)
     
     ;; quote TBD

     ;; atom
     (label l-parse-nxt)
     ;; number
     (mvi->r n-read-sym-str P0)
     (jsr l-str2num) ; -> P0=is-num flag P1=number
     (mvi->a 0)
     (sub-r P0)
     (jz l-parse-sym)
     (r->a P1) (a->r P0)
     (jsr l-num-atom) ; P0=number -> P0=cons
     (j l-parse-ret)
     
     ;; symbol
     (label l-parse-sym)
     (mvi->r n-read-sym-str P0)
     (jsr l-find-symbol) ; P0=found-flag P1=symbol
     (mvi->a 1)
     (sub-r P0)
     (jnz l-parse-new-sym)
    
     (r->a P1) (a->r P0) ; P0 = symbol (from l-find-symbol)
    
     (label l-parse-ret)
     (pop-r R3)
     (pop-a)
     (j-a)
     
     (label l-parse-new-sym)
     ;; new symbol TBD

     ;; -----------------
     ;; P0=number -> P0=cons
     (label l-num-atom)
     (push-r R1)
     (mvi->r n-cons R1)
     (r->a P0) (a->r R0) ; R0=num
     (jsr l-cons) ; P0 = cons-cell
     (r->a P0) (a->r R1) ; R1=cons
     (lsl-a)
     (lsl-a)
     (add-r R1) ; cons-addr = n-cons + cons-idx * 4
     (st-a->r R0) ; cons-cell-content = number
     ;; set type to num
     ;; set type to cons
     (mvi->r c-cons-number R0)
     (mvi->r n-cons-type R1)
     (r->a P0)
     (add-r R1) ; A = n-cons-type + cons-idx
     (st.b-a->r R0) ; n-cons-type[cons-idx] = c-cons-number

     (r->a SRP)
     (j-a)
     ;; -----------------
     ;; L list() {
     ;;   L x;
     ;;   return scan() == ')' ?
     ;;     nil :
     ;;     !strcmp(buf, ".") ?
     ;;       (x = read(),scan(),x) :
     ;;       (x = parse(),cons(x,list()));
     ;;    }
     ;; list:
     ;;    if ')'
     ;;       return nil
     ;;    else
     ;;       return cons( sym, list() )
     (label l-list)
     (push-srp)
     (push-r R1)

     (jsr f-scan) ; -> P0
     (mvi->a reader-rpar) ; if ')'
     (sub-r P0)
     (jnz l-list-cont)
     ;; return nil
     (mvi->r 0 P0)
     (pop-r R1)
     (pop-a)
     (j-a)

     ;; else 
     ;;   parse()
     ;;   return cons( sym, list() )
     (label l-list-cont)

     (jsr l-parse) ; P0 = scan result -> cons (sym)
     
     (r->a P0) (a->r R0) ; R0 = sym
     (jsr l-cons) ; P0 = cons-cell
     (r->a P0) (a->r R1) ; R1 = cons-cell = P0
     (r->a R0) (a->r P1) ; P1 = sym
     (jsr l-rplca) ; (rplca cons-cell sym)
     (jsr l-list)
     (r->a P0) (a->r P1) ; P1 = list-ret-val = P0
     (r->a R1) (a->r P0) ; P0 = cons-cell
     (jsr l-rplcd) ; (rplcd cons-cell list-ret-val) 
     ;; return cons-cell
     (r->a R1) (a->r P0) ; P0 = cons-cell = R1
     (pop-r R1)
     (pop-a)
     (j-a)
))

(defvar func-rplca nil)
(setq func-rplca
  '( ;; rplca
     ;; P0 - cons ptr
     ;; P1 - value to replace car of cons cell
     (label l-rplca)
     (push-r R0)
     (mvi->r n-cons R0)
     (r->a P0)
     (lsl-a)
     (lsl-a)
     (add-r R0) ; n-cons + 4*cons-ptr is car ptr
     (st.w-a->r P1) ; write car
     (pop-r R0)
     (r->a SRP)
     (j-a)))

(defvar func-rplcd nil)
(setq func-rplcd
  '( ;; rplcd
     ;; P0 - cons ptr
     ;; P1 - value to replace cdr of cons cell
     (label l-rplcd)
     (push-r R0)
     (mvi->r n-cons R0)
     (r->a P0)
     (lsl-a)
     (lsl-a)
     (add-r R0) ; n-cons + 4*cons-ptr
     (a->r R0)
     (mvi->a 2) ; offs for cdr
     (add-r R0)
     (st.w-a->r P1) ; write cdr
     (pop-r R0)
     (r->a SRP)
     (j-a)))
     
(defvar func-cons nil)
(setq func-cons
  '( ;; cons
     ;; P0 - returns a ptr to a new allocated cons cell
     (label l-cons)
     (push-r R1)
     (mvi->r n-cons-free R0)
     (ld.w-r->a R0) ; A = next free cons
     (a->r P0) ; return value
     (mvi->a 1) ; cons-free += 1
     (add-r P0)
     (st.w-r->a R0)
     ;; set type to cons
     (mvi->r c-cons-cons R0)
     (mvi->r n-cons-type R1)
     (r->a P0)
     (add-r R1) ; A = n-cons-type + cons-idx
     (st.b-a->r R0) ; n-cons-type[cons-idx] = c-cons-cons
     (pop-r R1)
     (r->a SRP)
     (j-a)))

(defvar func-div10 nil)
(setq func-div10
  '( ;; div10
     (label l-div10)
     ;; input: P0 = number
     ;; output: P0 = number//10
     ;;         P1 = number % 10
     ;; using this algorithm:
     ;; x = 0
     ;; B =   0x10000000  # 32-bit binary 10000000000000000000000000000000
     ;; b10 = 0xA0000000  # 32-bit binary 10100000000000000000000000000000
     ;; 
     ;; while b != 0:
     ;;     if b10 <= v:
     ;;         v -= b10
     ;;         x |= b
     ;;     b10 >>= 1
     ;;     b >>= 1
     ;; return x,v
     (push-r R2)
     (mvi->r #x10000000 R0) ; b
     (mvi->r #xA0000000 R1) ; b10
     (mvi->r 0 R2) ; x
     ; P0 = v
     (label l-div10-loop)
     (mvi->a 0)
     (sub-r R0)
     (jz l-div10-ret)
     (r->a P0) ; v 
     (sub-r R1) ; v - b10
     (jlo l-no-sub) ; if b10 <= v then sub -> if v >= b10 -> if v < b10 then not sub
     ; 123 <= 122 -> 122 >= 123
     ; v = v - b10 (already in A)
     (a->r P0)
     ; x |= b
     (r->a R2) ; x
     (or-r R0) ; | b
     (a->r R2) ; ->x

     (label l-no-sub)
     ; b10 >>= 1
     (r->a R1)
     (lsr-a)
     (a->r R1)
     ; b >>= 1
     (r->a R0)
     (lsr-a)
     (a->r R0)
     
     (j l-div10-loop)
     
     (label l-div10-ret)
     (r->a P0) (a->r P1) ; P1 = v (remainder)
     (r->a R2) (a->r P0) ; P0 = x (quotient)
     (pop-r R2)
     (r->a SRP)
     (j-a)))

(defvar test-read-c nil)
(setq test-read-c 
  '( 
     (mvi->r n-stack-highest SP)

     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (r->a R0) ; base-ptr
     (mvi->r 1 R1) ; use-unread = 1
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (1)

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (jsr l-read-c) ; P0 = char
     (jsr l-putchar) ; print P0

     (label end)
     (j end)))

(defvar test-reader nil)
(setq test-reader
 '(
     (mvi->r n-stack-highest SP)

     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     ;; sym
     (jsr f-scan)
     (mvi->r n-read-sym-str P0)
     (jsr prtstr)
    
     ;; num
     (jsr f-scan)
     (r->a P1) (a->r P0)
     (jsr l-prtdec)

     ;;sym
     (jsr f-scan)
     (mvi->r n-read-sym-str P0)
     (jsr prtstr)

     (jsr f-scan)
     (mvi->r n-read-sym-str P0)
     (jsr prtstr)

     (jsr f-scan)
     (mvi->r n-read-sym-str P0)
     (jsr prtstr)

     (label end-reader)
     (j end-reader)))

(defvar test-find-symbol nil)
(setq test-find-symbol
 '(      
     (mvi->r n-stack-highest SP)

     (mvi->r n-source-start P0)
     (jsr l-find-symbol) ; P0=found-flag P1=symbol
     (r->a P1)(a->r P0)
     (jsr l-cdr) ; P0 = name-ptr = cdr(symbol)
     ;; name-ptr is index into string-space
     (mvi->r n-string-space R0)
     (r->a P0)
     (add-r R0)
     (a->r P0)
     (jsr prtstr)

     (label end-fs)
     (j end-fs)))

;;; test reading a symbol
(defvar test-parse-1 nil)
(setq test-parse-1
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr l-parse) ; P0=object (cons prt)
     ;; in this test we know it's a symbol
     (jsr l-cdr) ; P0 = name-ptr = cdr(symbol)
     ;; name-ptr is index into string-space
     (mvi->r n-string-space R0)
     (r->a P0)
     (add-r R0)
     (a->r P0)
     (jsr prtstr)

     (label end-fs)
     (j end-fs)))

;;; test reading a number
(defvar test-parse-num nil)
(setq test-parse-num
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr f-scan) ; P0 = read obj type
     
     (jsr l-parse) ; P0= obj type from scan -> object (cons ptr)

     (jsr l-print-number)

     (label end-pn)
     (j end-pn)))

;;; test reading a list
(defvar test-parse-2 nil)
(setq test-parse-2
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr f-scan) ; P0 = read obj type
     
     (jsr l-parse) ; P0= obj type from scan -> object (cons ptr)
     ;; in this test we know it's a symbol
     (jsr l-cdr) ; P0 = name-ptr = cdr(symbol)
     ;; name-ptr is index into string-space
     (mvi->r n-string-space R0)
     (r->a P0)
     (add-r R0)
     (a->r P0)
     (jsr prtstr)

     (label end-fs)
     (j end-fs)))

;;; test reading a list
(defvar test-parse-3 nil)
(setq test-parse-3
 '(      
     (mvi->r n-stack-highest SP)

     ;; --- init scan -----------
     ;; setup read-ptr to point to source-start
     (mvi->r n-source-start R1)
     (mvi->r reader-state R0) ; base-ptr
     (r->a R0) ; base-ptr
     (st-r->a-rel rs-read-ptr R1) ; M[ A(base) + read-ptr-offs ] = R1 (read-ptr)
     (mvi->r 0 R1) ; use-unread
     (st-r->a-rel rs-use-unread R1) ; M[ A(base) + use-unread-offs ] = R1 (0)

     (jsr f-scan) ; P0 = read obj type
     
     (jsr l-parse) ; P0= obj type from scan -> object (cons ptr)
     (r->a P0) (a->r R0)
     ;; [ sym1 | . ]
     ;;          |
     ;;          +->[ sym2 | . ]
     ;;                      |
     ;;                      +-> nil
     (jsr l-print)

     (label end-fs)
     (j end-fs)))

(defvar test-str2num nil)
(setq test-str2num
  '( 
     (mvi->r n-stack-highest SP)
     (mvi->r n-source-start P0)
     (jsr l-str2num)
     (label end-s2n)
     (j end-s2n)))

(defvar test-div10 nil)
(setq test-div10
  '( (mvi->r n-stack-highest SP)
     (mvi->r 439 P0)
     (jsr l-div10)
     (mvi->r 141 P0)
     (jsr l-div10)
     (mvi->r 4200000000 P0)
     (jsr l-div10)
     (jsr l-div10)
     (jsr l-div10)
     (jsr l-div10)
     (label end-d10)
     (j end-d10)))

(defvar test-prtdec nil)
(setq test-prtdec
  '( (mvi->r n-stack-highest SP)
     (mvi->r 439 P0)
     (jsr l-prtdec)
     (mvi->r 100001 P0)
     (jsr l-prtdec)
     (mvi->r 0 P0)
     (jsr l-prtdec)
     (label end-pd)
     (j end-pd)))

(defvar main nil)
(setq main 
  '( ;; --- main ---
     ;(mvi->r 0 R1)
     ;(jsr prtstr)
     
     (mvi->r n-stack-highest SP)

     ;; test str-equal
     ;;(mvi->r n-source-start P1)
     ;;(mvi->r n-sym-string-start P0)
     ;;(jsr l-str-equal)
     ;;(j end)
     
     (mvi->r n-source-start P0)
     (jsr l-find-symbol)
     ))
     
     ;; setup:
     ;; P3 - use-unread
     ;(mvi->r 0 P3)
     ;; P1 - read-ptr
     ;(mvi->r n-source-start P1)

     ;; ------- scan 1 --------------
     ;(jsr f-scan)
     ;(r->a P0) (a->r R0)

     ;; R5 - the source symbol that we are searching for
     ;;(mvi->r n-source-start R5)
     
     ;;;(mvi->r n-cons-type R1) ; R1 cons type table ptr
     ;;(mvi->r n-cons R2) ; R2 cons table ptr
     ;;(mvi->r n-string-space R3) ; strings
     ;;;(ld.b-r->a R1) ;; ignore value for now

     ;;; R2 = cons-table-ptr 
     ;;(label l-next-cons)
     ;;;; n-cons + 2
     ;;(r->a R2)
     ;;(a->r R4)
     ;;(mvi->a 2) ; high word
     ;;(add-r R4)
     ;;(a->r R4)
     ;;;; read cdr
     ;;(ld.w-r->a R4) ; cdr -> sym-name-ptr
     ;;(add-r R3) ; string-space + sym-name-ptr
     ;;(a->r P0)
     ;;(r->a R5)
     ;;(a->r P1)
     ;;(jsr l-str-equal)

     ;;(mvi->a 0)
     ;;(sub-r P0)
     ;;(jnz l-found-sym)
     ;;;(jsr prtstr)

     ;;;; cons-ptr += 4
     ;;(mvi->a 4)
     ;;(add-r R2)
     ;;(a->r R2)

     ;;(j l-next-cons)

     ;;(label l-found-sym) 
     ;;(label l-stop)
     ;;(j l-stop)
     ;;
     ;;(mvi->r n-read-sym-str R1)
     ;;(jsr prtstr)
     
     
     

(defvar func-prtstr nil)
#|
(setq func-prtstr-old
  '( ;; --- prtstr ---
     ;; r1 - ptr to zero terminated string
     (label prtstr)
     (ld.b-r->a R1)
     (mask-a-b)
     (a->r R2)
     (mvi->r -1 R3) ; ptr to I/O reg
     (mvi->a 0)
     (sub-r R2)
     (jz ret-prtstr)
     (r->a R2)
     (st-a->r R3)
     (mvi->a 1)
     (add-r R1)
     (a->r R1)
     (j prtstr)
     (label ret-prtstr)
     (r->a srp)
     (j-a)))
|#

(setq func-prtstr
  '( ;; --- prtstr ---
     ;; P0 - ptr to zero terminated string
     (label prtstr)
     (push-r R1)
     (label l-prtstr-loop)
     (ld.b-r->a P0)
     (mask-a-b)
     (a->r R0)
     (mvi->r -1 R1) ; ptr to I/O reg
     (mvi->a 0)
     (sub-r R0)
     (jz ret-prtstr)
     (r->a R0)
     (st-a->r R1)
     (mvi->a 1)
     (add-r P0)
     (a->r P0)
     (j l-prtstr-loop)
     (label ret-prtstr)
     (pop-r R1)
     (r->a srp)
     (j-a)))

(defvar func-putchar nil)
(setq func-putchar
  '( ;; --- putchar ---
     ;; P0 - char to print
     (label l-putchar)
     (push-r R0)
     (mvi->r -1 R0) ; ptr to I/O reg
     (r->a P0)
     (st-a->r R0)
     (pop-r R0)
     (r->a srp)
     (j-a)))
     

(defvar func-str-equal nil)
(setq func-str-equal
  '( ;; str-equal
     ;; P1 - string-ptr
     ;; P0 - string-ptr / return value
     (label l-str-equal)
     (push-r R1)
     (label l-str-equal-loop)
     (ld.b-r->a P1)
     (a->r R0)
     (ld.b-r->a P0)
     (a->r R1)
     (sub-r R0)
     (jnz l-ret-uneq)
     ;; equal but are we at end of string?
     (mvi->a 0)
     (sub-r R0)
     (jnz l-inc-strs) ; not 0, inc ptrs then loop back
     ;; end of string and equal
     (mvi->r 1 P0)
     (pop-r R1)
     (r->a SRP)
     (j-a)

     (label l-ret-uneq)
     (mvi->r 0 P0)
     (pop-r R1)
     (r->a SRP)
     (j-a)
     
     (label l-inc-strs)
     (mvi->a 1)
     (add-r P1)
     (a->r P1)
     (mvi->a 1)
     (add-r P0)
     (a->r P0)
     (j l-str-equal-loop)))

(defvar func-car nil)
(setq func-car
  '( ;; car
     ;; P0 - cons ptr 
     ;;    - returns car ptr from the cons cell
     (label l-car)
     (r->a P0) ; cons-ptr = cons-index << 2 + n-cons
     (lsl-a)
     (lsl-a)
     (ld.w-a-rel->r n-cons P0) ; R0 = lowest word of cons-cell (cons.f2/car)
     (r->a SRP)
     (j-a)))

(defvar func-cdr nil)
(setq func-cdr
  '( ;; cdr
     ;; P0 - cons ptr 
     ;;    - returns cdr ptr from the cons cell
     (label l-cdr)
     (r->a P0) ; cons-ptr = cons-index << 2 + n-cons + 2
     (lsl-a)
     (lsl-a)
     (ld.w-a-rel->r (+ 2 n-cons) P0) ; R0 = highest word of cons-cell (cons.f1/cdr)
     (r->a SRP)
     (j-a)))

(defvar func-print-symbol nil)
(setq func-print-symbol
  '( ;; print-symbol
     ;; P0 = cons ptr to a symbol cons-cell (P0 destroyed)
     (label l-print-symbol)
     (push-srp)
     (push-r R0)
     (jsr l-cdr) ; P0 = name-ptr = cdr(symbol)
     ;; name-ptr is index into string-space
     (mvi->r n-string-space R0)
     (r->a P0)
     (add-r R0)
     (a->r P0)
     (jsr prtstr)
     (pop-r R0)
     (pop-a)
     (j-a)))

(defvar func-print-number nil)
(setq func-print-number
  '( ;; print-number
     ;; P0 = cons ptr to a number cons-cell (P0 destroyed)
     (label l-print-number)
     (push-srp)
     (push-r R0)
     (mvi->r n-cons R0)
     (r->a P0)
     (lsl-a)
     (lsl-a)
     (add-r R0) ; cons-addr = n-cons + cons-idx * 4
     (ld-a->r P0) ; the number
     (jsr l-prtdec)
     (pop-r R0)
     (pop-a)
     (j-a)
     
     (label l-prtdec) 
     ;; P0 = number
     (push-srp)
     (push-r R1)
     (r->a P0) (a->r R0)
     (mvi->r 10 R1)
     (r->a P0)
     (sub-r R1) ; P0 >= 10
     (jhs l-is-more-digits)
     ;; single digit
     (label l-prt-single)
     (mvi->r (char-code #\0 ) R0)
     (r->a P0)
     (add-r R0) ; '0' + num
     (a->r P0)
     (jsr l-putchar)
     (pop-r R1)
     (pop-a)
     (j-a)

     (label l-is-more-digits)
     (jsr l-div10) ; P0 -> P0=q P1=r
     (r->a P1) (a->r R1) 
     (jsr l-prtdec)
     (r->a R1) (a->r P0)
     (j l-prt-single)
     ))


(defvar func-print nil)
(setq func-print
  '( ;; print
     ;; P0 = object to be printed
     (label l-print)
     (push-srp)
     (push-r R1)
     (mvi->r n-cons-type R1)
     (r->a P0) (a->r R0)
     (add-r R1) ; n-cons-type + cons-idx
     (ld.b-a->r R1) ; R1 = type
     (mvi->a c-cons-symbol)
     (sub-r R1)
     (jnz l-not-sym)
     ;; symbol
     (jsr l-print-symbol) ; P0 is still cons-idx
     (j l-print-ret)
    
     ;;
     (label l-not-sym)
     (mvi->a c-cons-cons)
     (sub-r R1)
     (jnz l-not-cons)
     ;; cons, i.e. a list
     (jsr l-print-list)
     (j l-print-ret)
     
     (label  l-not-cons)
     (mvi->a c-cons-number)
     (sub-r R1)
     (jnz l-not-num)
     ;; number
     (jsr l-print-number)
     (j l-print-ret)
   
     (label l-not-num)
     ;; other type TBD
     
     (label l-print-ret)
     (pop-r R1)
     (pop-a)
     (j-a)))

(defvar func-print-list nil)
(setq func-print-list
  '( ;; print-list
     ;; P0 = cons-idx of list
     (label l-print-list)
     (push-srp)
     (push-r R1)
     (r->a P0) (a->r R0)
     (mvi->r (char-code #\() P0)
     (jsr l-putchar)
     (label l-pr-list-loop)
     (r->a R0) (a->r P0)
     (jsr l-car)
     (jsr l-print)
     (r->a R0) (a->r P0)
     (jsr l-cdr)
     (r->a P0) (a->r R0)
     (mvi->a 0) ;; nil?
     (sub-r R0)
     (jz l-eol)

     (mvi->r (char-code #\ ) P0)
     (jsr l-putchar)
     (j l-pr-list-loop)
    
     (label l-eol)
     (mvi->r (char-code #\)) P0)
     (jsr l-putchar)
     (pop-r R1)
     (pop-a)
     (j-a)))
     


(defvar *hello-world* nil)
;(setq *hello-world*
;      (masm main func-prtstr read-c scan))

(defvar e nil)
                                  
;(setf e (make-emulator *hello-world* dmem 200 nil))
;(run-with-curses e)

(defun t1 ()
  (asm-n-run test-read-c
    #'(lambda (dmem)
        (set-program dmem 
                     (string-to-mem "symbol symbol2 ( inner )") 
                     n-source-start))))
(defun t2 ()
  (asm-n-run test-reader
    #'(lambda (dmem)
        (set-program dmem 
                     (string-to-mem "symbol 123 123x symbol2 ( inner )") 
                     n-source-start))))

(defun t3 ()
  (asm-n-run test-find-symbol
    #'(lambda (dmem)
        (setq string-space-free 0)
        (add-symbol dmem "not-this" 0)
        (add-symbol dmem "symbol2" 0)
        (add-symbol dmem "symbol" 0)
        (set-program dmem 
                     (string-to-mem "symbol") 
                     n-source-start))))

(defun t4 ()
  (asm-n-run test-parse-1
    #'(lambda (dmem)
        (setq string-space-free 0)
        (add-symbol dmem "not-this" 0)
        (add-symbol dmem "symbol" 0)
        (set-program dmem 
                     (string-to-mem "symbol") 
                     n-source-start))))

;;; reading an empty list == nil
(defun t5 ()
  (asm-n-run test-parse-2
    #'(lambda (dmem)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "()") 
                     n-source-start))))

(defun t6 ()
  (asm-n-run test-parse-2
    #'(lambda (dmem)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "(sym1)") 
                     n-source-start))))

(defun t7 ()
  (asm-n-run test-parse-3
    #'(lambda (dmem)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "(sym1 (sym2 sym1))") 
                     n-source-start))))

(defun t8 ()
  (asm-n-run test-str2num
    #'(lambda (dmem)
        (setq string-space-free 0)
        (set-program dmem 
                     (string-to-mem "1234") 
                     n-source-start))))

(defun t9 ()
  (asm-n-run test-div10))

(defun t10 ()
  (asm-n-run test-prtdec))

(defun t11 ()
  (asm-n-run test-parse-num
    #'(lambda (dmem)
        (setq string-space-free 0)
        (add-symbol dmem "nil" 0) ; nil must be symbol 0
        (add-symbol dmem "sym1" 0)
        (add-symbol dmem "sym2" 0)
        (set-program dmem 
                     (string-to-mem "123") 
                     n-source-start))))

(defvar *symtab* nil)
(defun asm-n-run ( main &optional (setup nil) (debug nil))
  (setq *symtab* (make-hash-table))
  (setq *hello-world*
        (masm *symtab*
              main func-prtstr read-c scan func-str-equal
              func-find-symbol func-putchar func-cdr
              func-car func-parse func-rplca func-rplcd
              func-cons func-print-symbol func-print
              func-print-list func-str2num func-div10
              func-print-number))
  (setf e (make-emulator *hello-world* dmem 200 debug))
  (if setup (funcall setup dmem))
  (run-with-curses e *symtab*))

;(run-emul e 200 nil)


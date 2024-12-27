;;; ===========================================================================
(defpackage :lr-disasm
  (:use :cl :unit :lr-asm :lr-opcodes)
  (:export :disasm))
(in-package :lr-disasm)

(load-opcodes 
  (asdf:system-relative-pathname :literisc "rtl/literisc/cpu.py" ))

;;; ========================== disassembler ===================================
(defun disasm-immediate (bytes signed &optional debug)
  (let* ((pc 0)
         (imm (car bytes))
         (imm-end nil)
         (bits 0)
         (res 0))
    (if debug (format t "get-imm pc ~a imm ~a~%" pc imm))
    (loop do (progn
               (setf res (logior (ash res 7) (logand imm #x7f)))
               (setf imm-end (not (equal (logand imm #x80) #x80)))
               (setf pc (+ pc 1))
               (setf bits (+ bits 7))
               (setf imm (nth pc bytes))
               (if debug (format t "res ~a pc ~a imm ~a bits ~a~%" res pc imm bits)))
          until imm-end)
    (if signed (setf res (binary-to-signed-integer res bits)))
    (list res (+ 1 pc))))

(defun disassemble-instr ( bytes )
  (let* ((byte0 (car bytes))
         (opcode (ash byte0 -4))
         (param (logand byte0 15)))
    (cond ((equal opcode OPC_A_RX)
           (list (format nil "R~a = A" param) 1))
          ((equal opcode OPC_RX_A)
           (list (format nil "A = R~a" param) 1))
          ((equal opcode OPC_LD_A_OFFS)
           (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) nil )
            (list (format nil "R~a = M[A+~a]" param val) nr-bytes)))
          ((equal opcode OPC_LD_A)
           (list (format nil "R~a = M[A]" param) 1))
          ((equal opcode OPC_LD_RX)
           (list (format nil "A = M[R~a]" param) 1))
          ((equal opcode OPC_ST_A_OFFS)
           (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) nil )
            (list (format nil "M[A+~a] = R~a" val param) nr-bytes)))
          ((equal opcode OPC_ST_A)
           (list (format nil "M[A].l = R~a" param) 1))
          ((equal opcode OPC_ST_RX)
           (list (format nil "M[R~a].l = A" param) 1))
          ((equal opcode OPC_MVI)
           (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) nil)
            (list (format nil "R~a = ~a" param val) nr-bytes)))
          ((equal opcode OPC_MVIA)
           (list (format nil "A = sex(~a)" param) 1))
          ((equal opcode OPC_ADD)
           (list (format nil "A = A + R~a" param) 1))
          ((equal opcode OPC_SUB)
           (list (format nil "A = A - R~a" param) 1))
          ((equal opcode OPC_AND)
           (list (format nil "A = A & R~a" param) 1))
          ((equal opcode OPC_OR)
           (list (format nil "A = A | R~a" param) 1))
          ((equal opcode OPC_JMP)
           (cond ((equal param OPCJ_J)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "j #~a" val) nr-bytes)))
                 ((equal param OPCJ_JLT)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jlt #~a" val) nr-bytes)))
                 ((equal param OPCJ_JGE)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jge #~a" val) nr-bytes)))
                 ((equal param OPCJ_JLO)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jlo #~a" val) nr-bytes)))
                 ((equal param OPCJ_JHS)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jhs #~a" val) nr-bytes)))
                 ((equal param OPCJ_JZ)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jz #~a" val) nr-bytes)))
                 ((equal param OPCJ_JNZ)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jnz #~a" val) nr-bytes)))
                 ((equal param OPCJ_JLO8)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jlo.b #~a" val) nr-bytes)))
                 ((equal param OPCJ_JHS8)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jhs.b #~a" val) nr-bytes)))
                 ((equal param OPCJ_JZ8)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jz.b #~a" val) nr-bytes)))
                 ((equal param OPCJ_JNZ8)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jnz.b #~a" val) nr-bytes)))
                 ((equal param OPCJ_JLO16)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jlo.w #~a" val) nr-bytes)))
                 ((equal param OPCJ_JHS16)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jhs.w #~a" val) nr-bytes)))
                 ((equal param OPCJ_JZ16)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jz.w #~a" val) nr-bytes)))
                 ((equal param OPCJ_JNZ16)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jnz.w #~a" val) nr-bytes)))
                 ((equal param OPCJ_JSR)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) t )
                    (list (format nil "jsr #~a" val) nr-bytes)))))
          ((equal opcode OPC_NEXT)
           (cond ((equal param OPCI_NOT)
                  (list (format nil "A = ~~A") 1))
                 ((equal param OPCI_LSL)
                  (list (format nil "A = A << 1") 1))
                 ((equal param OPCI_LSR)
                  (list (format nil "A = A >> 1") 1))
                 ((equal param OPCI_ASR)
                  (list (format nil "A = A >> 1") 1))
                 ((equal param OPCI_PUSH_R)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) nil )
                    (list (format nil "push R0..R~a" val) nr-bytes)))
                 ((equal param OPCI_POP_R)
                  (destructuring-bind (val nr-bytes) (disasm-immediate (cdr bytes) nil )
                    (list (format nil "pop R0..R~a" val) nr-bytes)))
                 ((equal param OPCI_PUSH_SRP)
                  (list (format nil "push srp") 1))
                 ((equal param OPCI_POP_A)
                  (list (format nil "popa") 1))
                 ((equal param OPCI_NEXT)
                  (let* ((byte1 (cadr bytes))
                         (opcode2 (ash byte1 -4))
                         (param2 (logand byte1 15)))
                      (cond ((equal opcode2 OPCI2_LDB_A_OFFS)
                             (destructuring-bind (val nr-bytes) (disasm-immediate (cddr bytes) nil )
                               (list (format nil "R~a = M[A+~a].b" param2 val) nr-bytes)))
                            ((equal opcode2 OPCI2_LDB_A)
                             (list (format nil "R~a = M[A].b" param2) 2))
                            ((equal opcode2 OPCI2_LDB_RX)
                             (list (format nil "A = M[R~a].b" param2) 2))
                            ((equal opcode2 OPCI2_STB_A_OFFS)
                             (destructuring-bind (val nr-bytes) (disasm-immediate (cddr bytes) nil )
                               (list (format nil "M[A+~a].b = R~a" val param2) nr-bytes)))
                            ((equal opcode2 OPCI2_STB_A)
                             (list (format nil "M[A].b = R~a" param2) 2))
                            ((equal opcode2 OPCI2_STB_RX)
                             (list (format nil "M[R~a].b = A" param2) 2))
                            ((equal opcode2 OPCI2_XOR)
                             (list (format nil "A = A ^ R~a" param2) 2))
                            ((equal opcode2 OPCI2_LDW_A_OFFS)
                             (destructuring-bind (val nr-bytes) (disasm-immediate (cddr bytes) nil )
                               (list (format nil "R~a = M[A+~a].w" param2 val) nr-bytes)))
                            ((equal opcode2 OPCI2_LDW_A)
                             (list (format nil "R~a = M[A].w" param2) 2))
                            ((equal opcode2 OPCI2_LDW_RX)
                             (list (format nil "A = M[R~a].w" param2) 2))
                            ((equal opcode2 OPCI2_STW_A_OFFS)
                             (destructuring-bind (val nr-bytes) (disasm-immediate (cddr bytes) nil )
                               (list (format nil "M[A+~a].w = R~a" val param2) nr-bytes)))
                            ((equal opcode2 OPCI2_STW_A)
                             (list (format nil "M[A].w = R~a" param2) 2))
                            ((equal opcode2 OPCI2_STW_RX)
                             (list (format nil "M[R~a].w = A" param2) 2))
                            ((equal opcode2 OPCI2_NEXT)
                             (cond ((equal param2 OPCI3_EI)
                                    (list (format nil "ei") 2))
                                   ((equal param2 OPCI3_DI)
                                    (list (format nil "di") 2))
                                   (t 
                                    (list (format nil "undefined") 2)))))))
                 ((equal param OPCI_RETI)
                  (list (format nil "reti") 1))
                 ((equal param OPCI_MASKB)
                  (list (format nil "maskb A") 1))
                 ((equal param OPCI_MASKW)
                  (list (format nil "maskw A") 1))
                 ((equal param OPCI_SEXB)
                  (list (format nil "sexb A") 1))
                 ((equal param OPCI_SEXW)
                  (list (format nil "sexw A") 1))
                 ((equal param OPCI_J_A)
                  (list (format nil "j A") 1))
                 ((equal param OPCI_NOP)
                  (list (format nil "nop") 1))))
          )))

(defun disasm (bytes &optional (nr-instr 1000000000))
  (let ((pc 0)
        (prefix-cnt (if (> nr-instr 1) 1 0)))
    (loop while (< pc (list-length bytes))
          while (> nr-instr 0)
          do (destructuring-bind (s nr-bytes) 
               (disassemble-instr (subseq bytes pc))
               (progn
                  (format t "~[~*~:;~d ~]~15a ~{~d~^ ~}~%"
                          prefix-cnt
                          pc s (subseq bytes pc (+ pc nr-bytes)))
                  (setf pc (+ pc nr-bytes))))
          do (setf nr-instr (1- nr-instr)))))

;;;====================================================================================
;;;========================== unit test ===============================================
(deftest test-disasm ()
  (let* ((mcode
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
        (exp-str 
"0 R0 = A          0
1 jsr #1          175 1
3 R0 = A          0
4 R1 = 15         129 15
6 A = sex(11)     155
7 M[R1].l = A     113
8 M[A].l = R1     97
9 M[A+10] = R1    81 10
11 A = M[R1]       65
12 R1 = M[A]       49
13 R1 = M[A+10]    33 10
15 R2 = 1934       130 143 14
18 A = R1          17
19 A = R2          18
20 A = A - R1      193
21 A = R3          19
22 A = A - R3      195
23 jz #-6          165 122
25 j #-6           160 122
")
       (disasm-res (with-output-to-string (*standard-output*) (disasm mcode)))
)
    (check (equal exp-str disasm-res))))

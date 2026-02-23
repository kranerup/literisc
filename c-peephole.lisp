;;; ===========================================================================
;;; ==================== Peephole Optimizer for liteRISC ======================
;;; ===========================================================================
;;; Post-code-generation optimizer that eliminates redundant instruction sequences

(in-package :c-compiler)

;;; ===========================================================================
;;; Peephole Pattern Structure
;;; ===========================================================================

(defstruct peephole-rule
  name           ; string: rule name for debugging
  pattern        ; list: instruction pattern to match (with wildcards)
  replacement)   ; list or function: replacement instructions

;;; Pattern registry - populated at load time
(defvar *peephole-rules* nil)

;;; Maximum passes to prevent infinite loops
(defparameter *peephole-max-passes* 10)

;;; Output-buffer for backward scan: bound to the (reversed) result list
;;; in apply-peephole-rules so backward-scan helpers see already-emitted
;;; instructions rather than the original (possibly stale) code-vec entries.
(defvar *peephole-output-so-far* nil)

;;; ===========================================================================
;;; Pattern Matching
;;; ===========================================================================

(defun is-label-p (instr)
  "Check if instruction is a label (acts as barrier for optimization)"
  (and (consp instr) (eq (first instr) 'label)))

(defun is-comment-p (instr)
  "Check if instruction is a comment"
  (and (consp instr) (eq (first instr) :comment)))

(defun is-jump-p (instr)
  "Check if instruction is any kind of jump"
  (and (consp instr)
       (member (first instr) '(j jz jnz jlt jge jgt jle j-A jal jal-A))))

(defun is-conditional-jump-p (instr)
  "Check if instruction is a conditional jump"
  (and (consp instr)
       (member (first instr) '(jz jnz jlt jge jgt jle))))

(defun wildcard-p (sym)
  "Check if symbol is a wildcard (starts with ?)"
  (and (symbolp sym)
       (> (length (symbol-name sym)) 0)
       (char= (char (symbol-name sym) 0) #\?)))

(defun wildcard-name (sym)
  "Get the name part of a wildcard (e.g., ?r -> :r)"
  (intern (subseq (symbol-name sym) 1) :keyword))

(defun match-element (pattern actual bindings)
  "Match a single pattern element against an actual value.
   Returns (values matched-p new-bindings)"
  (cond
    ;; Wildcard - bind or check consistency
    ((wildcard-p pattern)
     (let* ((name (wildcard-name pattern))
            (existing (getf bindings name)))
       (if existing
           ;; Already bound - check consistency
           (if (equal existing actual)
               (values t bindings)
               (values nil nil))
           ;; New binding
           (values t (list* name actual bindings)))))
    ;; Literal match
    ((equal pattern actual)
     (values t bindings))
    ;; No match
    (t (values nil nil))))

(defun match-instruction (pattern instr bindings)
  "Match an instruction pattern against an actual instruction.
   Pattern like (Rx= ?v ?r) matches (Rx= 0 R0).
   Returns (values matched-p bindings)"
  (when (and (consp pattern) (consp instr)
             (= (length pattern) (length instr)))
    (let ((new-bindings bindings))
      (loop for p in pattern
            for a in instr
            do (multiple-value-bind (matched updated)
                   (match-element p a new-bindings)
                 (unless matched
                   (return-from match-instruction (values nil nil)))
                 (setf new-bindings updated)))
      (values t new-bindings))))

(defun try-match-rule (rule code-vec start-idx)
  "Try to match a rule's pattern at position start-idx in code-vec.
   Returns (values matched-p consumed-count replacement-instrs) or nil"
  (let ((pattern (peephole-rule-pattern rule))
        (len (length code-vec))
        (bindings nil)
        (idx start-idx))
    ;; Try to match each instruction in the pattern
    (dolist (pat-instr pattern)
      (when (>= idx len)
        (return-from try-match-rule nil))
      (let ((actual (aref code-vec idx)))
        ;; Skip comments when matching
        (loop while (and (< idx len) (is-comment-p (aref code-vec idx)))
              do (incf idx))
        (when (>= idx len)
          (return-from try-match-rule nil))
        (setf actual (aref code-vec idx))
        ;; Labels are barriers - don't match across them (except for jump-to-next pattern)
        (when (and (is-label-p actual)
                   (not (and (consp pat-instr) (eq (first pat-instr) 'label))))
          (return-from try-match-rule nil))
        ;; Try to match this instruction
        (multiple-value-bind (matched new-bindings)
            (match-instruction pat-instr actual bindings)
          (unless matched
            (return-from try-match-rule nil))
          (setf bindings new-bindings))
        (incf idx)))
    ;; All pattern instructions matched - compute replacement
    (let* ((consumed (- idx start-idx))
           (end-idx (+ start-idx consumed))
           (replacement-spec (peephole-rule-replacement rule))
           (replacement (if (functionp replacement-spec)
                            ;; Pass code-vec and end-idx so lambdas can do lookahead
                            (funcall replacement-spec bindings code-vec end-idx)
                            (instantiate-replacement replacement-spec bindings))))
      ;; If replacement function returns :no-match, treat as no match
      (if (eq replacement :no-match)
          nil
          (values t consumed replacement)))))

(defun instantiate-replacement (template bindings)
  "Instantiate a replacement template with bound values"
  (when (null template)
    (return-from instantiate-replacement nil))
  (mapcar (lambda (instr-template)
            (mapcar (lambda (elem)
                      (if (wildcard-p elem)
                          (getf bindings (wildcard-name elem))
                          elem))
                    instr-template))
          template))

;;; ===========================================================================
;;; Core Optimizer
;;; ===========================================================================

(defun apply-peephole-rules (code verbose)
  "Single pass applying all rules. Returns (values new-code changed-p)"
  (let* ((result nil)
         (changed nil)
         (code-vec (coerce code 'vector))
         (i 0)
         (len (length code)))
    (loop while (< i len)
          for instr = (aref code-vec i)
          do (cond
               ;; Comments pass through unchanged
               ((is-comment-p instr)
                (push instr result)
                (incf i))
               ;; Try each rule
               (t
                (setf *peephole-output-so-far* result)
                (let ((matched nil))
                  (dolist (rule *peephole-rules*)
                    (multiple-value-bind (did-match consumed replacement)
                        (try-match-rule rule code-vec i)
                      (when did-match
                        (when verbose
                          (format t "Peephole: ~a matched at ~a~%"
                                  (peephole-rule-name rule) i))
                        ;; Add replacement instructions
                        (dolist (r replacement)
                          (push r result))
                        (incf i consumed)
                        (setf changed t)
                        (setf matched t)
                        (return))))
                  ;; No rule matched - keep original instruction
                  (unless matched
                    (push instr result)
                    (incf i))))))
    (values (nreverse result) changed)))

(defun peephole-optimize (code &optional verbose)
  "Apply peephole optimizations to instruction list.
   Runs multiple passes until no more changes or max passes reached."
  (let ((current-code code)
        (pass 0))
    (loop
      (incf pass)
      (when (> pass *peephole-max-passes*)
        (when verbose
          (format t "Peephole: max passes (~a) reached~%" *peephole-max-passes*))
        (return current-code))
      (multiple-value-bind (new-code changed)
          (apply-peephole-rules current-code verbose)
        (unless changed
          (when verbose
            (format t "Peephole: converged after ~a passes~%" pass))
          (return new-code))
        (setf current-code new-code)))))

;;; ===========================================================================
;;; Optimization Rules
;;; ===========================================================================

;;; Clear rules before redefining
(setf *peephole-rules* nil)

;;; ===========================================================================
;;; Lookahead Utility
;;; ===========================================================================

(defun reg-read-after-p (reg code-vec from-idx)
  "Return T if REG is potentially read in any instruction at or after FROM-IDX,
   before the next unconditional write to REG.
   Used by peephole rules to ensure a register is dead before eliminating a write.

   Strategy:
   - Rx=... instructions WRITE to their last argument; if writing REG, stop (reg is
     redefined, so any prior write is invisible to later readers).
   - ...=Rx instructions READ from their last argument; if reading REG, return T.
   - All other instruction forms: if REG appears anywhere, conservatively return T."
  (loop for i from from-idx below (length code-vec)
        for instr = (aref code-vec i)
        when (and (consp instr) (symbolp (car instr)))
        do (let* ((opname (symbol-name (car instr)))
                  (oplen  (length opname))
                  (last-arg (when (cdr instr) (car (last (cdr instr))))))
             (cond
               ;; Rx=... instruction: writes to the last argument
               ((and (>= oplen 3) (string= opname "RX=" :end1 3))
                (when (eq last-arg reg)
                  ;; REG is being overwritten before any read — safe to eliminate
                  (return-from reg-read-after-p nil)))
               ;; ...=Rx instruction: reads from the last argument
               ((and (>= oplen 3) (string= opname "=RX" :start1 (- oplen 3)))
                (when (eq last-arg reg)
                  ;; REG is read — NOT safe to eliminate the earlier write
                  (return-from reg-read-after-p t)))
               ;; Other instruction: conservatively check if REG appears at all
               (t
                (when (member reg (cdr instr))
                  (return-from reg-read-after-p t)))))
        finally (return nil)))

(defun A-dead-before-next-write-p (code-vec from-idx)
  "Return T if the accumulator A is definitely overwritten before being read,
   scanning forward from FROM-IDX.

   Strategy:
   - A=Rx instruction: unconditional A write (doesn't read old A) — A is dead, return T.
   - Label/comment/data directives: skip (cross labels freely; a label's first instruction
     overwrites A on all paths into it when that instruction is A=Rx).
   - Branches and calls (opname starts with J, or JSR): conservatively return NIL.
   - Any other instruction whose opname contains 'A': A might be read, return NIL.
   - Instructions whose opname has no 'A': skip."
  (loop for i from from-idx below (length code-vec)
        for instr = (aref code-vec i)
        when (and (consp instr) (symbolp (car instr)))
        do (let* ((opname (symbol-name (car instr))))
             (cond
               ;; A=Rx: unconditional A write — the earlier A write is dead
               ((string= opname "A=RX")
                (return-from A-dead-before-next-write-p t))
               ;; Pure data/label/comment directives: skip
               ((or (string= opname "LABEL")
                    (string= opname "COMMENT")
                    (string= opname "LALIGN-DWORD")
                    (string= opname "ADWORD")
                    (string= opname "ALIGN-DWORD"))
                nil)
               ;; Branches, jumps, and calls: stop conservatively
               ((or (string= opname "JSR")
                    (and (>= (length opname) 1) (char= (char opname 0) #\J)))
                (return-from A-dead-before-next-write-p nil))
               ;; Any other instruction whose name contains 'A': might read A
               ((find #\A opname)
                (return-from A-dead-before-next-write-p nil))
               ;; Instruction not involving A: skip
               (t nil)))
        finally (return nil)))

(defun A-already-equals-reg-p (r)
  "Return T if A provably holds the same value as register R at the current
   peephole position, by scanning backward through *peephole-output-so-far*
   (the already-emitted instructions, most-recent first) to find a prior
   (Rx=A r) with no intervening write to A or r.

   Using the output buffer (rather than the original code-vec) ensures we
   see only instructions that were actually emitted — not instructions that
   were consumed and replaced by an earlier peephole rule in this same pass.

   Conservative: stops (returns NIL) at labels, branches, calls, or any
   instruction that writes A or r."
  (loop for instr in *peephole-output-so-far*
        when (and (consp instr) (symbolp (car instr)))
        do (let* ((opname (symbol-name (car instr)))
                  (oplen  (length opname))
                  (last-arg (when (cdr instr) (car (last (cdr instr))))))
             (cond
               ;; Rx=A r: A was stored into r here — A still equals r
               ((and (string= opname "RX=A") (eq last-arg r))
                (return-from A-already-equals-reg-p t))
               ;; Any other Rx=... instruction writing r: r has a new value
               ((and (>= oplen 3) (string= opname "RX=" :end1 3)
                     (eq last-arg r))
                (return-from A-already-equals-reg-p nil))
               ;; POP-R r: r overwritten from stack
               ((and (string= opname "POP-R") (eq last-arg r))
                (return-from A-already-equals-reg-p nil))
               ;; Any instruction whose name starts with "A=": writes A
               ((and (>= oplen 2) (char= (char opname 0) #\A)
                     (char= (char opname 1) #\=))
                (return-from A-already-equals-reg-p nil))
               ;; POP-A: writes A from stack
               ((string= opname "POP-A")
                (return-from A-already-equals-reg-p nil))
               ;; Labels: potential jump target — stop conservatively
               ((string= opname "LABEL")
                (return-from A-already-equals-reg-p nil))
               ;; Branches and calls: stop conservatively
               ((or (string= opname "JSR")
                    (and (>= oplen 1) (char= (char opname 0) #\J)))
                (return-from A-already-equals-reg-p nil))
               ;; Comments and data directives: skip
               ((or (string= opname "COMMENT")
                    (string= opname "LALIGN-DWORD")
                    (string= opname "ADWORD")
                    (string= opname "ALIGN-DWORD"))
                nil)
               ;; Everything else (Rx=A other-r, M[A]=Rx, Push-R, etc.): skip
               (t nil)))
        finally (return nil)))

;;; NOTE: Zero-test elimination rule was removed because it's incorrect.
;;; The pattern (Rx= 0 ?r) (A-=Rx ?r) (jz ?label) is NOT redundant.
;;; The A-=Rx instruction is required to SET THE FLAGS based on A's value.
;;; JZ checks the zero flag, not the accumulator directly.

;;; Rule 2: Redundant register round-trip
;;; (Rx=A ?r) (A=Rx ?r) -> (Rx=A ?r)  [when ?r is live]
;;;                      -> {}          [when ?r is dead]
;;; After (Rx=A r), A is unchanged (Rx=A writes only to r). So the immediately
;;; following (A=Rx r) is always redundant — A already equals r.
;;; When r is also dead afterwards, both instructions can be dropped entirely.
;;; NOTE: Must NOT apply to SP because (RX=A SP) updates the stack pointer!
(push (make-peephole-rule
       :name "roundtrip"
       :pattern '((Rx=A ?r) (A=Rx ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (let ((r (getf bindings :r)))
                        (cond
                          ;; Don't optimize SP — stack pointer updates have side effects
                          ((eq r 'SP)
                           (list (list 'Rx=A 'SP) (list 'A=Rx 'SP)))
                          ;; Register is dead: eliminate both (A=Rx r is a no-op, Rx=A r
                          ;; is a dead write)
                          ((not (reg-read-after-p r code-vec end-idx))
                           nil)
                          ;; Register is live: keep only (Rx=A r); the (A=Rx r) is a
                          ;; no-op since Rx=A does not modify A — A already equals r
                          (t
                           (list (list 'Rx=A r)))))))
      *peephole-rules*)

;;; Rule 3: Consecutive loads to same register
;;; (Rx= ?v1 ?r) (Rx= ?v2 ?r) -> (Rx= ?v2 ?r)
(push (make-peephole-rule
       :name "consecutive-load"
       :pattern '((Rx= ?v1 ?r) (Rx= ?v2 ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (list (list 'Rx= (getf bindings :v2) (getf bindings :r)))))
      *peephole-rules*)

;;; Rule 4: Jump to next instruction
;;; (j ?label) (label ?label) -> (label ?label)
(push (make-peephole-rule
       :name "jump-to-next"
       :pattern '((j ?label) (label ?label))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (list (list 'label (getf bindings :label)))))
      *peephole-rules*)

;;; Rule 5: Useless A transfer after load
;;; (A=Rx ?r) (Rx=A ?r) -> (A=Rx ?r)
(push (make-peephole-rule
       :name "useless-transfer"
       :pattern '((A=Rx ?r) (Rx=A ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (list (list 'A=Rx (getf bindings :r)))))
      *peephole-rules*)

;;; Rule 6: Double negation
;;; (A=-A) (A=-A) -> (remove both)
(push (make-peephole-rule
       :name "double-negation"
       :pattern '((A=-A) (A=-A))
       :replacement nil)
      *peephole-rules*)

;;; Rule 7: Load zero then negate (produces zero)
;;; (A= 0) (A=-A) -> (A= 0)
(push (make-peephole-rule
       :name "negate-zero"
       :pattern '((A= 0) (A=-A))
       :replacement '((A= 0)))
      *peephole-rules*)

;;; Rule 8: Redundant load to A from same register
;;; (Rx=A ?r) (A=Rx ?r) when followed by Rx=A ?r -> just Rx=A ?r
;;; This is a stronger version of rule 2

;;; Rule 9: Store and immediate reload from memory at same location (SP-relative)
;;; This is complex and depends on proving no intervening modifications
;;; Skipping for now - would need data flow analysis

;;; Rule 10: Immediate load through temp register
;;; (Rx= ?v ?r1) (A=Rx ?r1) (Rx=A ?r2) -> (Rx= ?v ?r2)
;;; When loading an immediate to a temp, moving to A, then to target,
;;; we can just load directly to target.
;;; NOTE: When r1 == r2, the Rx=A is redundant but A=Rx is still needed
;;; because A may be used later (e.g., for M[A]=Rx). In that case, keep A=Rx.
(push (make-peephole-rule
       :name "immediate-through-temp"
       :pattern '((Rx= ?v ?r1) (A=Rx ?r1) (Rx=A ?r2))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((reg1 (getf bindings :r1))
                            (reg2 (getf bindings :r2))
                            (val (getf bindings :v)))
                        (if (eq reg1 reg2)
                            ;; r1 == r2: keep the A=Rx since A may be used later
                            (list (list 'Rx= val reg1) (list 'A=Rx reg1))
                            ;; r1 != r2: safe to optimize away
                            (list (list 'Rx= val reg2))))))
      *peephole-rules*)

;;; Rule 11: Small immediate load through temp register
;;; (A= ?v) (Rx=A ?r) -> (Rx= ?v ?r)
;;; When A= is used for small immediates (-128 to 127), we can load directly
(push (make-peephole-rule
       :name "small-immediate-through-A"
       :pattern '((A= ?v) (Rx=A ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((val (getf bindings :v))
                            (reg (getf bindings :r)))
                        ;; Don't optimize if target is SP (side effects)
                        (if (eq reg 'SP)
                            (list (list 'A= val) (list 'Rx=A reg))
                            (list (list 'Rx= val reg))))))
      *peephole-rules*)

;;; Rule 12: Redundant reload after store (M[A]=Rx doesn't modify A)
;;; (A=Rx ?r) (M[A]=Rx ?r2) (A=Rx ?r) -> (A=Rx ?r) (M[A]=Rx ?r2)
(push (make-peephole-rule
       :name "redundant-reload-after-store"
       :pattern '((A=Rx ?r) (M[A]=Rx ?r2) (A=Rx ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (list (list 'A=Rx (getf bindings :r))
                            (list 'M[A]=Rx (getf bindings :r2)))))
      *peephole-rules*)

;;; Rule 13: Redundant reload after store with offset (M[A+n]=Rx doesn't modify A)
;;; (A=Rx ?r) (M[A+n]=Rx ?n ?r2) (A=Rx ?r) -> (A=Rx ?r) (M[A+n]=Rx ?n ?r2)
(push (make-peephole-rule
       :name "redundant-reload-after-store-offset"
       :pattern '((A=Rx ?r) (M[A+n]=Rx ?n ?r2) (A=Rx ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (list (list 'A=Rx (getf bindings :r))
                            (list 'M[A+n]=Rx (getf bindings :n) (getf bindings :r2)))))
      *peephole-rules*)

;;; Rule 14: Redundant reload after two stores (common for 64-bit stores)
;;; (A=Rx ?r) (M[A]=Rx ?r2) (A=Rx ?r) (M[A+n]=Rx ?n ?r3) -> remove middle A=Rx
(push (make-peephole-rule
       :name "redundant-reload-between-stores"
       :pattern '((A=Rx ?r) (M[A]=Rx ?r2) (A=Rx ?r) (M[A+n]=Rx ?n ?r3))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (list (list 'A=Rx (getf bindings :r))
                            (list 'M[A]=Rx (getf bindings :r2))
                            (list 'M[A+n]=Rx (getf bindings :n) (getf bindings :r3)))))
      *peephole-rules*)

;;; Rule 15: Redundant reload after memory load (Rx=M[A] doesn't modify A)
;;; (A=Rx ?r) (Rx=M[A] ?r2) (A=Rx ?r) -> (A=Rx ?r) (Rx=M[A] ?r2)
;;; BUT ONLY when ?r != ?r2 - if same register, the third A=Rx copies the loaded value
(push (make-peephole-rule
       :name "redundant-reload-after-load"
       :pattern '((A=Rx ?r) (Rx=M[A] ?r2) (A=Rx ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((reg1 (getf bindings :R))
                            (reg2 (getf bindings :R2)))
                        (if (eq reg1 reg2)
                            ;; Same register - third A=Rx is NOT redundant, don't match
                            :no-match
                            ;; Different registers - third A=Rx is redundant
                            (list (list 'A=Rx reg1)
                                  (list 'Rx=M[A] reg2))))))
      *peephole-rules*)

;;; Rule 16: Redundant reload after memory load with offset
;;; (A=Rx ?r) (Rx=M[A+n] ?n ?r2) (A=Rx ?r) -> (A=Rx ?r) (Rx=M[A+n] ?n ?r2)
;;; BUT ONLY when ?r != ?r2 - if same register, the third A=Rx copies the loaded value
(push (make-peephole-rule
       :name "redundant-reload-after-load-offset"
       :pattern '((A=Rx ?r) (Rx=M[A+n] ?n ?r2) (A=Rx ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((reg1 (getf bindings :R))
                            (reg2 (getf bindings :R2))
                            (offs (getf bindings :N)))
                        (if (eq reg1 reg2)
                            ;; Same register - third A=Rx is NOT redundant, don't match
                            :no-match
                            ;; Different registers - third A=Rx is redundant
                            (list (list 'A=Rx reg1)
                                  (list 'Rx=M[A+n] offs reg2))))))
      *peephole-rules*)

;;; Rule 17: Redundant reload after two loads (common for 64-bit loads)
;;; (A=Rx ?r) (Rx=M[A] ?r2) (A=Rx ?r) (Rx=M[A+n] ?n ?r3) -> remove middle A=Rx
;;; BUT ONLY when ?r != ?r2 - if same register, the middle A=Rx copies the loaded value
(push (make-peephole-rule
       :name "redundant-reload-between-loads"
       :pattern '((A=Rx ?r) (Rx=M[A] ?r2) (A=Rx ?r) (Rx=M[A+n] ?n ?r3))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((reg1 (getf bindings :R))
                            (reg2 (getf bindings :R2))
                            (reg3 (getf bindings :R3))
                            (offs (getf bindings :N)))
                        (if (eq reg1 reg2)
                            ;; Same register - middle A=Rx is NOT redundant, don't match
                            :no-match
                            ;; Different registers - middle A=Rx is redundant
                            (list (list 'A=Rx reg1)
                                  (list 'Rx=M[A] reg2)
                                  (list 'Rx=M[A+n] offs reg3))))))
      *peephole-rules*)

;;; Rule 18: Redundant reload after Rx=A and store
;;; (Rx=A ?r) (M[A]=Rx ?r2) (A=Rx ?r) -> (Rx=A ?r) (M[A]=Rx ?r2)
;;; After Rx=A, A still contains the value, so reloading from Rx is redundant
(push (make-peephole-rule
       :name "redundant-reload-after-store-from-A"
       :pattern '((Rx=A ?r) (M[A]=Rx ?r2) (A=Rx ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((reg (getf bindings :r)))
                        ;; Don't optimize SP - it may have been modified
                        (if (eq reg 'SP)
                            (list (list 'Rx=A reg)
                                  (list 'M[A]=Rx (getf bindings :r2))
                                  (list 'A=Rx reg))
                            (list (list 'Rx=A reg)
                                  (list 'M[A]=Rx (getf bindings :r2)))))))
      *peephole-rules*)

;;; Rule 19: Redundant reload after Rx=A and two stores (64-bit case)
;;; (Rx=A ?r) (M[A]=Rx ?r2) (A=Rx ?r) (M[A+n]=Rx ?n ?r3) -> remove middle A=Rx
(push (make-peephole-rule
       :name "redundant-reload-after-store-from-A-64"
       :pattern '((Rx=A ?r) (M[A]=Rx ?r2) (A=Rx ?r) (M[A+n]=Rx ?n ?r3))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((reg (getf bindings :r)))
                        (if (eq reg 'SP)
                            (list (list 'Rx=A reg)
                                  (list 'M[A]=Rx (getf bindings :r2))
                                  (list 'A=Rx reg)
                                  (list 'M[A+n]=Rx (getf bindings :n) (getf bindings :r3)))
                            (list (list 'Rx=A reg)
                                  (list 'M[A]=Rx (getf bindings :r2))
                                  (list 'M[A+n]=Rx (getf bindings :n) (getf bindings :r3)))))))
      *peephole-rules*)

;;; Rule 20: Redundant copy back to same register after memory load
;;; (Rx=M[A] ?r) (A=Rx ?r) (Rx=A ?r) -> (Rx=M[A] ?r) (A=Rx ?r)
;;; When loading to a register, copying to A, then copying A back to the same register,
;;; the third instruction is redundant since the register already has the value.
(push (make-peephole-rule
       :name "redundant-copy-back-after-load"
       :pattern '((Rx=M[A] ?r) (A=Rx ?r) (Rx=A ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((reg (getf bindings :R)))
                        (list (list 'Rx=M[A] reg)
                              (list 'A=Rx reg)))))
      *peephole-rules*)

;;; Rule 21: Load through temp register to different target
;;; (Rx=M[A] ?r1) (A=Rx ?r1) (Rx=A ?r2) -> (Rx=M[A] ?r2) when r1 != r2
;;; When loading to a temp register, copying to A, then copying A to a different
;;; target register, we can load directly into the target register.
;;; NOTE: When r1 == r2, this rule returns nil to indicate no match, allowing
;;; rule 20 to handle that case.
(push (make-peephole-rule
       :name "load-through-temp-to-target"
       :pattern '((Rx=M[A] ?r1) (A=Rx ?r1) (Rx=A ?r2))
       :replacement (lambda (bindings code-vec end-idx)
                      (declare (ignore code-vec end-idx))
                      (let ((reg1 (getf bindings :R1))
                            (reg2 (getf bindings :R2)))
                        (if (eq reg1 reg2)
                            ;; Same register - return :no-match to let rule 20 handle it
                            :no-match
                            ;; Different registers - load directly into target
                            (list (list 'Rx=M[A] reg2))))))
      *peephole-rules*)

;;; Rule 22: Dead or redundant A load
;;; (A=Rx ?r) -> {} in two cases:
;;;   1. Forward scan: A is provably overwritten before being read.
;;;      Handles consecutive A loads, dead A before label, etc.
;;;   2. Backward scan: A already equals r because a prior (Rx=A r) stored A
;;;      into r with no intervening writes to A or r.
;;;      Handles: (Rx=A r)(non-A/r instrs)(A=Rx r) -> drop the reload.
(push (make-peephole-rule
       :name "dead-or-redundant-A-load"
       :pattern '((A=Rx ?r))
       :replacement (lambda (bindings code-vec end-idx)
                      (let ((r (getf bindings :r)))
                        (if (or (A-dead-before-next-write-p code-vec end-idx)
                                (A-already-equals-reg-p r))
                            nil
                            (list (list 'A=Rx r))))))
      *peephole-rules*)

;;; Reverse the rules list so higher-priority rules are tried first
(setf *peephole-rules* (nreverse *peephole-rules*))

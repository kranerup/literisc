;;; ===========================================================================
;;; ================== Virtual Register Allocator for liteRISC ================
;;; ===========================================================================
;;; Implements linear scan register allocation to map virtual registers (V0, V1, ...)
;;; to physical registers (R0, R1, ...) contiguously from R0 upward.
;;; This minimizes PUSH/POP overhead by avoiding gaps in register usage.

(in-package :c-compiler)

;;; ===========================================================================
;;; Virtual Register Utilities
;;; ===========================================================================

(defun make-vreg (id)
  "Create virtual register symbol V0, V1, ..."
  (intern (format nil "V~d" id) :c-compiler))

(defun vreg-p (sym)
  "Check if symbol is a virtual register"
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 1)
              (char= (char name 0) #\V)
              (every #'digit-char-p (subseq name 1))))))

(defun vreg-id (sym)
  "Extract numeric ID from virtual register"
  (parse-integer (subseq (symbol-name sym) 1)))

(defun physical-reg-p (sym)
  "Check if symbol is a physical register (R0-R15, P0-P3, SP, SRP, A)"
  (member sym '(R0 R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15
                P0 P1 P2 P3 SP SRP A)))

(defun make-physical-reg (index)
  "Create physical register symbol from index (0 -> R0, etc.)"
  (intern (format nil "R~d" index) :c-compiler))

;;; ===========================================================================
;;; Live Interval Data Structure
;;; ===========================================================================

(defstruct live-interval
  vreg        ; virtual register symbol
  start       ; first use (instruction index)
  end)        ; last use (instruction index)

;;; ===========================================================================
;;; Instruction Analysis
;;; ===========================================================================

(defun instruction-vregs (instr)
  "Extract all virtual registers from an instruction"
  (let ((vregs nil))
    (labels ((scan (x)
               (cond
                 ((vreg-p x) (pushnew x vregs))
                 ((consp x) (mapc #'scan x)))))
      (scan instr))
    vregs))

(defun find-loop-back-edges (code)
  "Find loop back-edges in the code.
   Returns a list of (loop-start-idx . loop-end-idx) pairs."
  (let ((labels (make-hash-table :test 'eq))
        (back-edges nil))
    ;; First pass: collect label positions
    (loop for instr in code
          for idx from 0
          when (and (consp instr)
                    (eq (car instr) 'label))
          do (setf (gethash (cadr instr) labels) idx))
    ;; Second pass: find backward jumps
    (loop for instr in code
          for idx from 0
          when (and (consp instr)
                    (eq (car instr) 'j)
                    (symbolp (cadr instr)))
          do (let ((target-idx (gethash (cadr instr) labels)))
               (when (and target-idx (< target-idx idx))
                 ;; This is a backward jump (loop)
                 (push (cons target-idx idx) back-edges))))
    back-edges))

(defun extend-intervals-for-loops (intervals back-edges code)
  "Extend live intervals that are USED inside loops to cover the full loop.
   Only extends intervals for vregs that actually appear in instructions
   within the loop body."
  (when back-edges
    ;; For each loop, find which vregs are used inside it
    (dolist (edge back-edges)
      (let ((loop-start (car edge))
            (loop-end (cdr edge))
            (loop-vregs (make-hash-table :test 'eq)))
        ;; Collect vregs used inside this loop
        (loop for instr in code
              for idx from 0
              when (and (>= idx loop-start) (<= idx loop-end))
              do (dolist (vreg (instruction-vregs instr))
                   (setf (gethash vreg loop-vregs) t)))
        ;; Extend intervals for vregs used in this loop
        (dolist (interval intervals)
          (when (gethash (live-interval-vreg interval) loop-vregs)
            (let ((start (live-interval-start interval))
                  (end (live-interval-end interval)))
              ;; Only extend if interval touches the loop
              (when (and (<= start loop-end)
                         (>= end loop-start))
                (setf (live-interval-start interval) (min start loop-start))
                (setf (live-interval-end interval) (max end loop-end)))))))))
  intervals)

(defun build-live-intervals (code)
  "Build live intervals from instruction list.
   Returns a list of live-interval structs sorted by start position.
   Accounts for loops by extending intervals that are live inside loops."
  (let ((intervals (make-hash-table :test 'eq)))
    ;; First pass: build basic intervals
    (loop for instr in code
          for idx from 0
          do (dolist (vreg (instruction-vregs instr))
               (let ((interval (gethash vreg intervals)))
                 (if interval
                     ;; Update end position
                     (setf (live-interval-end interval) idx)
                     ;; Create new interval
                     (setf (gethash vreg intervals)
                           (make-live-interval :vreg vreg :start idx :end idx))))))

    ;; Convert to list
    (let ((interval-list (loop for v being the hash-values of intervals collect v)))
      ;; Find loops and extend intervals accordingly
      (let ((back-edges (find-loop-back-edges code)))
        (when back-edges
          (extend-intervals-for-loops interval-list back-edges code)))
      ;; Return sorted by start position
      (sort interval-list #'< :key #'live-interval-start))))

;;; ===========================================================================
;;; Linear Scan Register Allocator
;;; ===========================================================================

(defparameter *num-allocatable-regs* 8)  ; R2-R9 available for allocation
                                          ; R0/R1 reserved for spill scratch
                                          ; R10-R13 (P0-P3) are parameter regs
                                          ; R14 (SRP) and R15 (SP) are reserved

(defun linear-scan-allocate (intervals)
  "Allocate physical registers using linear scan algorithm.
   Returns: (values assignment max-reg-used spill-slots)
   - assignment: hash table mapping vreg -> physical register index
   - max-reg-used: highest physical register index used (-1 if none)
   - spill-slots: hash table mapping vreg -> spill slot offset (for spilled vregs)"
  (let ((active nil)              ; currently live intervals (sorted by end point)
        (free-regs (loop for i from 2 below (+ 2 *num-allocatable-regs*) collect i)) ; Start from R2
        (assignment (make-hash-table :test 'eq))  ; vreg -> physical reg index
        (spill-slots (make-hash-table :test 'eq)) ; vreg -> spill offset
        (next-spill-offset 0)
        (max-reg-used -1))

    (dolist (interval intervals)
      ;; Expire old intervals - return their registers to the free pool
      (let ((expired (remove-if-not
                      (lambda (a) (< (live-interval-end a) (live-interval-start interval)))
                      active)))
        (dolist (exp expired)
          (let ((reg (gethash (live-interval-vreg exp) assignment)))
            (when reg
              (push reg free-regs)))
          (setf active (remove exp active))))

      (if free-regs
          ;; Assign lowest available register for better PUSH/POP efficiency
          (let ((reg (apply #'min free-regs)))
            (setf free-regs (remove reg free-regs))
            (setf (gethash (live-interval-vreg interval) assignment) reg)
            (setf max-reg-used (max max-reg-used reg))
            ;; Insert into active, maintaining sort by end position
            (setf active (merge 'list (list interval) active #'<
                                :key #'live-interval-end)))
          ;; All registers in use - spill the interval ending latest
          (let* ((candidates (cons interval active))
                 (spill-candidate (first (sort (copy-list candidates) #'>
                                               :key #'live-interval-end))))
            (if (eq spill-candidate interval)
                ;; Spill current interval
                (progn
                  (setf (gethash (live-interval-vreg interval) spill-slots)
                        next-spill-offset)
                  (incf next-spill-offset 4))
                ;; Spill the longest-living interval, give its register to current
                (let ((reg (gethash (live-interval-vreg spill-candidate) assignment)))
                  (remhash (live-interval-vreg spill-candidate) assignment)
                  (setf (gethash (live-interval-vreg spill-candidate) spill-slots)
                        next-spill-offset)
                  (incf next-spill-offset 4)
                  (setf (gethash (live-interval-vreg interval) assignment) reg)
                  (setf active (remove spill-candidate active))
                  (setf active (merge 'list (list interval) active #'<
                                      :key #'live-interval-end)))))))

    (values assignment max-reg-used spill-slots)))

;;; ===========================================================================
;;; Code Rewriting
;;; ===========================================================================

(defun get-instr-vreg-usage (instr)
  "Analyze an instruction and return which vregs are read and written.
   Returns a property list like (:reads (V1 V2) :writes (V3)).
   Uses string-based opcode matching to classify instructions:
   - Opcodes starting with RX= (e.g. Rx=A, Rx=M[A+n]) write to the register (last arg).
   - Opcodes ending with =RX (e.g. A=Rx, A+=Rx, M[A+n]=Rx) read from the register (last arg).
   - All other instructions are scanned generically for vregs (treated as reads)."
  (let ((reads nil)
        (writes nil))
    (when (consp instr)
      (let* ((op (car instr))
             (opname (when (symbolp op) (symbol-name op)))
             (oplen (when opname (length opname))))
        (cond
          ;; Rx=... instructions: write to register (last element)
          ((and opname (>= oplen 3) (string= opname "RX=" :end1 3))
           (let ((reg (car (last (cdr instr)))))
             (when (vreg-p reg)
               (push reg writes))))
          ;; ...=Rx instructions: read from register (last element)
          ((and opname (>= oplen 3) (string= opname "=RX" :start1 (- oplen 3)))
           (let ((reg (car (last (cdr instr)))))
             (when (vreg-p reg)
               (push reg reads))))
          ;; Everything else: generic scan for vregs (treat as reads)
          (t
           (labels ((scan (x)
                      (when (vreg-p x) (pushnew x reads))))
             (mapc #'scan (cdr instr)))))))
    (list :reads (nreverse reads) :writes (nreverse writes))))

(defun rewrite-with-physical-regs (code assignment spill-slots)
  "Replace virtual registers with physical ones, inserting spill/reload code."
  (let ((spill-vregs (loop for k being the hash-keys of spill-slots collect k)))
    (mapcan
     (lambda (instr)
       (let* ((usage (get-instr-vreg-usage instr))
              (reads (getf usage :reads))
              (writes (getf usage :writes))
              (spilled-reads (intersection reads spill-vregs))
              (spilled-writes (intersection writes spill-vregs))
              (instr-copy (copy-tree instr)))

         (if (not (or spilled-reads spilled-writes))
             ;; Simple case: no spills for this instruction
             (list (substitute-vregs instr-copy assignment))
             ;; Complex case: spills detected
             (let ((pre-instrs nil)
                   (post-instrs nil)
                   (scratch-map (make-hash-table)))
               ;; Handle spilled reads
               (loop for vreg in spilled-reads
                     for i from 0
                     for scratch-reg = (if (= i 0) 'R0 'R1)
                     do
                        (let ((offset (gethash vreg spill-slots)))
                          (push `(Rx=M[SP+n] ,offset ,scratch-reg) pre-instrs)
                          (setf (gethash vreg scratch-map) scratch-reg)))

               ;; Handle spilled writes
               (loop for vreg in spilled-writes
                     for i from 0
                     for scratch-reg = (if (and (null spilled-reads) (= i 0)) 'R0 'R1)
                     do
                        (let ((offset (gethash vreg spill-slots)))
                          (push `(M[SP+n]=Rx ,offset ,scratch-reg) post-instrs)
                          (setf (gethash vreg scratch-map) scratch-reg)))

               ;; Substitute vregs in the instruction copy
               (labels ((replace-vreg (x)
                          (cond
                            ((vreg-p x)
                             (let ((assigned-reg-idx (gethash x assignment))
                                   (scratch-reg (gethash x scratch-map)))
                               (cond
                                 (assigned-reg-idx (make-physical-reg assigned-reg-idx))
                                 (scratch-reg scratch-reg)
                                 (t (error "Unassigned virtual register: ~a" x)))))
                            ((consp x) (mapcar #'replace-vreg x))
                            (t x))))
                 (setf instr-copy (replace-vreg instr-copy)))

               ;; Return the full sequence
               (append (nreverse pre-instrs)
                       (list instr-copy)
                       (nreverse post-instrs))))))
     code)))

(defun substitute-vregs (instr assignment)
  "Substitute non-spilled vregs. Used for the simple path."
  (labels ((replace-vreg (x)
             (cond
               ((vreg-p x)
                (let ((reg-idx (gethash x assignment)))
                  (if reg-idx
                      (make-physical-reg reg-idx)
                      (error "Unassigned virtual register in simple substitution: ~a" x))))
               ((consp x) (mapcar #'replace-vreg x))
               (t x))))
    (replace-vreg instr)))

;;; ===========================================================================
;;; Main Entry Point
;;; ===========================================================================

(define-condition spill-needed (error)
  ((count :initarg :count :reader spill-needed-count))
  (:report (lambda (c s)
             (format s "Register spilling needed: ~a virtual registers would need to be spilled"
                     (spill-needed-count c)))))

(defun allocate-registers (code)
  "Main register allocation entry point.
   Takes code with virtual registers, returns code with physical registers.
   Returns: (values rewritten-code max-reg-used spill-count)
   Signals SPILL-NEEDED condition if there aren't enough registers."
  (let ((intervals (build-live-intervals code)))
    (if (null intervals)
        ;; No virtual registers in code
        (values code -1 0)
        ;; Run linear scan allocation
        (multiple-value-bind (assignment max-reg spill-slots)
            (linear-scan-allocate intervals)
          (let ((spill-count (hash-table-count spill-slots)))
            (when (> spill-count 0)
              ;; Signal error so caller can fall back to non-virtual-reg mode
              (error 'spill-needed :count spill-count))
            (values (rewrite-with-physical-regs code assignment spill-slots)
                    max-reg
                    spill-count))))))

;;; ===========================================================================
;;; Debug/Inspection Utilities
;;; ===========================================================================

(defun print-live-intervals (intervals &optional (stream t))
  "Print live intervals for debugging"
  (format stream "Live Intervals:~%")
  (dolist (int intervals)
    (format stream "  ~a: [~a, ~a]~%"
            (live-interval-vreg int)
            (live-interval-start int)
            (live-interval-end int))))

(defun print-register-assignment (assignment &optional (stream t))
  "Print register assignment for debugging"
  (format stream "Register Assignment:~%")
  (maphash (lambda (vreg reg-idx)
             (format stream "  ~a -> R~d~%" vreg reg-idx))
           assignment))

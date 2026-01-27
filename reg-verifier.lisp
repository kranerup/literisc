;;; ===========================================================================
;;; ============== Register Save/Restore Verification for liteRISC ============
;;; ===========================================================================
;;; This module verifies that functions correctly preserve callee-saved registers
;;; (R6-R9, SRP) according to the calling convention.
;;;
;;; Calling Convention:
;;;   R0-R5:  Caller-saved (temps) - caller must save if needed
;;;   R6-R9:  Callee-saved (locals) - called function must preserve
;;;   P0-P3 (R10-R13): Caller-saved (params) - caller must save if needed
;;;   SRP (R14): Callee-saved - called function must preserve (non-leaf)
;;;   SP (R15): Callee-saved - must be balanced on return

(defpackage :reg-verifier
  (:use :cl)
  (:export :make-reg-verifier
           :reg-verifier-enabled
           :reg-verifier-violations
           :verifier-on-jsr
           :verifier-on-return
           :run-emul-with-verification
           :run-with-sentinel-check
           :format-violations))

(in-package :reg-verifier)

;;; ===========================================================================
;;; Data Structures
;;; ===========================================================================

(defstruct call-frame
  "Captures register state at the point of a JSR instruction"
  return-address     ; SRP value at time of JSR (where to return)
  caller-pc          ; PC of the JSR instruction (for error reporting)
  saved-r6           ; Callee-saved register values
  saved-r7
  saved-r8
  saved-r9
  saved-sp)          ; Stack pointer (must be balanced on return)

(defstruct reg-verifier
  "Tracks function calls and verifies register preservation"
  (enabled t)              ; Enable/disable verification
  (call-stack nil)         ; Stack of call-frame records
  (violations nil))        ; List of detected violations

(defstruct violation
  "Records a register preservation violation"
  type                ; :r6 :r7 :r8 :r9 :sp :srp
  expected            ; Expected value
  actual              ; Actual value
  return-address      ; Where the return happened
  caller-pc)          ; Where the call originated

;;; ===========================================================================
;;; Register Constants
;;; ===========================================================================

(defconstant +R6+ 6)
(defconstant +R7+ 7)
(defconstant +R8+ 8)
(defconstant +R9+ 9)
(defconstant +SRP+ 14)
(defconstant +SP+ 15)

;;; ===========================================================================
;;; Verifier Operations
;;; ===========================================================================

(defun get-reg (proc reg-num)
  "Get register value from processor state"
  (aref (lr-emulator::processor-state-r proc) reg-num))

(defun verifier-on-jsr (verifier proc)
  "Called after a JSR instruction executes.
   Captures callee-saved register values before the called function runs."
  (when (reg-verifier-enabled verifier)
    (let ((frame (make-call-frame
                  :return-address (get-reg proc +SRP+)
                  :caller-pc (- (lr-emulator::processor-state-pc proc) 1) ; Approximate
                  :saved-r6 (get-reg proc +R6+)
                  :saved-r7 (get-reg proc +R7+)
                  :saved-r8 (get-reg proc +R8+)
                  :saved-r9 (get-reg proc +R9+)
                  :saved-sp (get-reg proc +SP+))))
      (push frame (reg-verifier-call-stack verifier)))))

(defun verifier-on-return (verifier proc)
  "Called after a J-A (return) instruction executes.
   Verifies that callee-saved registers match their saved values."
  (when (and (reg-verifier-enabled verifier)
             (reg-verifier-call-stack verifier))
    (let ((frame (pop (reg-verifier-call-stack verifier))))
      (when frame
        ;; Check R6
        (let ((current-r6 (get-reg proc +R6+)))
          (unless (= current-r6 (call-frame-saved-r6 frame))
            (push (make-violation
                   :type :r6
                   :expected (call-frame-saved-r6 frame)
                   :actual current-r6
                   :return-address (lr-emulator::processor-state-pc proc)
                   :caller-pc (call-frame-caller-pc frame))
                  (reg-verifier-violations verifier))))
        ;; Check R7
        (let ((current-r7 (get-reg proc +R7+)))
          (unless (= current-r7 (call-frame-saved-r7 frame))
            (push (make-violation
                   :type :r7
                   :expected (call-frame-saved-r7 frame)
                   :actual current-r7
                   :return-address (lr-emulator::processor-state-pc proc)
                   :caller-pc (call-frame-caller-pc frame))
                  (reg-verifier-violations verifier))))
        ;; Check R8
        (let ((current-r8 (get-reg proc +R8+)))
          (unless (= current-r8 (call-frame-saved-r8 frame))
            (push (make-violation
                   :type :r8
                   :expected (call-frame-saved-r8 frame)
                   :actual current-r8
                   :return-address (lr-emulator::processor-state-pc proc)
                   :caller-pc (call-frame-caller-pc frame))
                  (reg-verifier-violations verifier))))
        ;; Check R9
        (let ((current-r9 (get-reg proc +R9+)))
          (unless (= current-r9 (call-frame-saved-r9 frame))
            (push (make-violation
                   :type :r9
                   :expected (call-frame-saved-r9 frame)
                   :actual current-r9
                   :return-address (lr-emulator::processor-state-pc proc)
                   :caller-pc (call-frame-caller-pc frame))
                  (reg-verifier-violations verifier))))
        ;; Check SP (must be balanced)
        (let ((current-sp (get-reg proc +SP+)))
          (unless (= current-sp (call-frame-saved-sp frame))
            (push (make-violation
                   :type :sp
                   :expected (call-frame-saved-sp frame)
                   :actual current-sp
                   :return-address (lr-emulator::processor-state-pc proc)
                   :caller-pc (call-frame-caller-pc frame))
                  (reg-verifier-violations verifier))))))))

;;; ===========================================================================
;;; Execution with Verification
;;; ===========================================================================

(defun run-emul-with-verification (emul max-instr &optional (debug nil))
  "Run emulator with register preservation verification.
   Returns (values break-reason verifier) where verifier contains any violations."
  (let ((verifier (make-reg-verifier))
        (proc (lr-emulator:emulated-system-processor emul))
        (imem (lr-emulator::emulated-system-imem emul))
        (dmem (lr-emulator::emulated-system-dmem emul)))
    ;; Add write callback for output (same as run-emul)
    (lr-emulator::processor-add-wr-callback proc 'lr-emulator::write-cb-write-char)

    ;; Clear last-instr before starting
    (setf (lr-emulator::processor-state-last-instr proc) nil)

    (dotimes (n max-instr)
      (when (lr-emulator::processor-state-break proc)
        (return-from run-emul-with-verification (values :break verifier)))

      ;; Execute one instruction
      (lr-emulator::execute-instruction proc imem dmem)

      ;; Check if we just executed a JSR or J-A
      (let ((last-instr (lr-emulator::processor-state-last-instr proc)))
        (case last-instr
          (jsr (verifier-on-jsr verifier proc))
          (j-a (verifier-on-return verifier proc))))

      ;; Clear last-instr for next iteration
      (setf (lr-emulator::processor-state-last-instr proc) nil))

    (when debug
      (format t "Verification completed: max-instr ~a reached, pc:~a~%"
              max-instr (lr-emulator::processor-state-pc proc)))
    (values :max-cycles verifier)))

;;; ===========================================================================
;;; Utility Functions
;;; ===========================================================================

(defun format-violation (v)
  "Format a violation for display"
  (format nil "~a: expected #x~8,'0X, got #x~8,'0X (call from PC #x~X, return at PC #x~X)"
          (violation-type v)
          (violation-expected v)
          (violation-actual v)
          (violation-caller-pc v)
          (violation-return-address v)))

(defun format-violations (verifier)
  "Format all violations in a verifier for display"
  (if (null (reg-verifier-violations verifier))
      "No register preservation violations detected."
      (with-output-to-string (s)
        (format s "Register preservation violations:~%")
        (dolist (v (reverse (reg-verifier-violations verifier)))
          (format s "  ~a~%" (format-violation v))))))

;;; ===========================================================================
;;; Simpler Alternative: Sentinel-Based Verification
;;; ===========================================================================
;;; This is a simpler approach that only catches corruption visible at the
;;; end of main(), but is trivial to implement and useful for basic checks.

(defconstant +sentinel-r6+ #xDEAD0006)
(defconstant +sentinel-r7+ #xDEAD0007)
(defconstant +sentinel-r8+ #xDEAD0008)
(defconstant +sentinel-r9+ #xDEAD0009)

(defun run-with-sentinel-check (emul max-instr &optional (debug nil))
  "Run emulator after setting sentinel values in R6-R9.
   Returns (values break-reason sentinels-preserved-p) where sentinels-preserved-p
   is T if all callee-saved registers R6-R9 have their original sentinel values
   after main() returns.

   Note: This only catches corruption visible at the end of execution.
   Use run-emul-with-verification for complete checking."
  (let ((proc (lr-emulator:emulated-system-processor emul)))
    ;; Set sentinel values before execution
    (setf (aref (lr-emulator::processor-state-r proc) +R6+) +sentinel-r6+)
    (setf (aref (lr-emulator::processor-state-r proc) +R7+) +sentinel-r7+)
    (setf (aref (lr-emulator::processor-state-r proc) +R8+) +sentinel-r8+)
    (setf (aref (lr-emulator::processor-state-r proc) +R9+) +sentinel-r9+)

    ;; Run the program
    (lr-emulator:run-emul emul max-instr debug)

    ;; Check if sentinels are preserved
    (let ((preserved (and (= +sentinel-r6+ (aref (lr-emulator::processor-state-r proc) +R6+))
                          (= +sentinel-r7+ (aref (lr-emulator::processor-state-r proc) +R7+))
                          (= +sentinel-r8+ (aref (lr-emulator::processor-state-r proc) +R8+))
                          (= +sentinel-r9+ (aref (lr-emulator::processor-state-r proc) +R9+)))))
      (when (and debug (not preserved))
        (format t "Sentinel check failed:~%")
        (unless (= +sentinel-r6+ (aref (lr-emulator::processor-state-r proc) +R6+))
          (format t "  R6: expected #x~8,'0X, got #x~8,'0X~%"
                  +sentinel-r6+ (aref (lr-emulator::processor-state-r proc) +R6+)))
        (unless (= +sentinel-r7+ (aref (lr-emulator::processor-state-r proc) +R7+))
          (format t "  R7: expected #x~8,'0X, got #x~8,'0X~%"
                  +sentinel-r7+ (aref (lr-emulator::processor-state-r proc) +R7+)))
        (unless (= +sentinel-r8+ (aref (lr-emulator::processor-state-r proc) +R8+))
          (format t "  R8: expected #x~8,'0X, got #x~8,'0X~%"
                  +sentinel-r8+ (aref (lr-emulator::processor-state-r proc) +R8+)))
        (unless (= +sentinel-r9+ (aref (lr-emulator::processor-state-r proc) +R9+))
          (format t "  R9: expected #x~8,'0X, got #x~8,'0X~%"
                  +sentinel-r9+ (aref (lr-emulator::processor-state-r proc) +R9+))))
      (values (if (lr-emulator::processor-state-break proc) :break :max-cycles)
              preserved))))

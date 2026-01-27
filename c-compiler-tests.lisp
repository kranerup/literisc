;;; ===========================================================================
;;; =================== C Compiler Tests for liteRISC =========================
;;; ===========================================================================

(in-package :c-compiler)

;;; ===========================================================================
;;; Test Utilities
;;; ===========================================================================

;;; Test output saving configuration
(defvar *save-test-outputs* nil "When non-nil, save test outputs to this directory")
(defvar *test-output-counter* 0 "Counter for generating unique test filenames")
(defvar *current-test-name* nil "Name of the current test for output filenames")

(defun make-test-output-filename (&key optimize)
  "Generate a unique filename for test output"
  (incf *test-output-counter*)
  ;; Use unit:*test-name* which tracks the current test hierarchy
  (let ((test-name (if unit::*test-name*
                       (format nil "~{~a~^-~}" unit::*test-name*)
                       "test")))
    (format nil "~a/~3,'0d-~a~a.asm"
            *save-test-outputs*
            *test-output-counter*
            (string-downcase (substitute #\- #\Space test-name))
            (if optimize "-opt" ""))))

(defun run-and-get-result (source &key (verbose nil) (max-cycles 10000) (optimize-size nil))
  "Compile, run, and return the result in P0"
  (handler-case
      (let ((result (run-c-program source :verbose verbose :max-cycles max-cycles :optimize-size optimize-size)))
        ;; Save output if enabled
        (when *save-test-outputs*
          (let ((filename (make-test-output-filename)))
            (save-compilation-output source filename :run-result result)))
        result)
    (error (e)
      (format t "Error: ~a~%" e)
      ;; Still try to save on error
      (when *save-test-outputs*
        (let ((filename (make-test-output-filename)))
          (handler-case
              (save-compilation-output source filename :run-result "ERROR")
            (error () nil))))
      nil)))

(defun to-signed-32 (n)
  "Convert unsigned 32-bit to signed"
  (if (>= n #x80000000)
      (- n #x100000000)
      n))

(defun result= (expected actual)
  "Compare expected vs actual, handling signed 32-bit values"
  (= expected (to-signed-32 actual)))

;;; ===========================================================================
;;; Phase 1 Tests: Minimal Compiler
;;; ===========================================================================

(deftest test-return-literal ()
  "Test: int main() { return 42; }"
  (check
    (= 42 (run-and-get-result "int main() { return 42; }"))
    (= 0 (run-and-get-result "int main() { return 0; }"))
    (= 1 (run-and-get-result "int main() { return 1; }"))
    (= 255 (run-and-get-result "int main() { return 255; }"))))

(deftest test-return-negative ()
  "Test: return negative numbers"
  (check
    (result= -1 (run-and-get-result "int main() { return -1; }"
                                    :verbose nil))))

;;; ===========================================================================
;;; Phase 2 Tests: Expressions
;;; ===========================================================================

(deftest test-addition ()
  "Test: addition"
  (check
    (= 5 (run-and-get-result "int main() { return 2 + 3; }"))
    (= 10 (run-and-get-result "int main() { return 1 + 2 + 3 + 4; }"))
    (= 100 (run-and-get-result "int main() { return 50 + 50; }"))))

(deftest test-subtraction ()
  "Test: subtraction"
  (check
    (= 3 (run-and-get-result "int main() { return 5 - 2; }"))
    (= 0 (run-and-get-result "int main() { return 5 - 5; }"))
    (= 10 (run-and-get-result "int main() { return 20 - 5 - 5; }"))))

(deftest test-multiplication ()
  "Test: multiplication"
  (check
    (= 6 (run-and-get-result "int main() { return 2 * 3; }"))
    (= 24 (run-and-get-result "int main() { return 2 * 3 * 4; }"))
    (= 100 (run-and-get-result "int main() { return 10 * 10; }"))))

(deftest test-division ()
  "Test: division"
  (check
    (= 5 (run-and-get-result "int main() { return 10 / 2; }"))
    (= 3 (run-and-get-result "int main() { return 10 / 3; }"))  ; integer division
    (= 2 (run-and-get-result "int main() { return 100 / 50; }"))))

(deftest test-modulo ()
  "Test: modulo"
  (check
    (= 1 (run-and-get-result "int main() { return 10 % 3; }"))
    (= 0 (run-and-get-result "int main() { return 10 % 5; }"))
    (= 2 (run-and-get-result "int main() { return 17 % 5; }"))))

(deftest test-bitwise ()
  "Test: bitwise operations"
  (check
    (= 2 (run-and-get-result "int main() { return 3 & 2; }"))
    (= 3 (run-and-get-result "int main() { return 1 | 2; }"))
    (= 1 (run-and-get-result "int main() { return 3 ^ 2; }"))
    (result= -1 (run-and-get-result "int main() { return ~0; }"))))

(deftest test-shifts ()
  "Test: shift operations"
  (check
    (= 8 (run-and-get-result "int main() { return 2 << 2; }"))
    (= 2 (run-and-get-result "int main() { return 8 >> 2; }"))))

(deftest test-comparisons ()
  "Test: comparison operations"
  (check
    (= 1 (run-and-get-result "int main() { return 5 == 5; }"))
    (= 0 (run-and-get-result "int main() { return 5 == 6; }"))
    (= 1 (run-and-get-result "int main() { return 5 != 6; }"))
    (= 0 (run-and-get-result "int main() { return 5 != 5; }"))
    (= 1 (run-and-get-result "int main() { return 3 < 5; }"))
    (= 0 (run-and-get-result "int main() { return 5 < 3; }"))
    (= 1 (run-and-get-result "int main() { return 5 > 3; }"))
    (= 0 (run-and-get-result "int main() { return 3 > 5; }"))
    (= 1 (run-and-get-result "int main() { return 5 <= 5; }"))
    (= 1 (run-and-get-result "int main() { return 3 <= 5; }"))
    (= 1 (run-and-get-result "int main() { return 5 >= 5; }"))
    (= 1 (run-and-get-result "int main() { return 5 >= 3; }"))))

(deftest test-logical ()
  "Test: logical operations"
  (check
    (= 1 (run-and-get-result "int main() { return 1 && 1; }"))
    (= 0 (run-and-get-result "int main() { return 1 && 0; }"))
    (= 0 (run-and-get-result "int main() { return 0 && 1; }"))
    (= 1 (run-and-get-result "int main() { return 1 || 0; }"))
    (= 1 (run-and-get-result "int main() { return 0 || 1; }"))
    (= 0 (run-and-get-result "int main() { return 0 || 0; }"))
    (= 1 (run-and-get-result "int main() { return !0; }"))
    (= 0 (run-and-get-result "int main() { return !1; }"))
    (= 0 (run-and-get-result "int main() { return !42; }"))))

(deftest test-logical-chaining ()
  "Test: chained && and || operators"
  (check
    ;; Chained AND - all must be true
    (= 1 (run-and-get-result "int main() { return 1 && 1 && 1; }"))
    (= 0 (run-and-get-result "int main() { return 1 && 1 && 0; }"))
    (= 0 (run-and-get-result "int main() { return 1 && 0 && 1; }"))
    (= 0 (run-and-get-result "int main() { return 0 && 1 && 1; }"))
    ;; Chained OR - any must be true
    (= 1 (run-and-get-result "int main() { return 1 || 0 || 0; }"))
    (= 1 (run-and-get-result "int main() { return 0 || 1 || 0; }"))
    (= 1 (run-and-get-result "int main() { return 0 || 0 || 1; }"))
    (= 0 (run-and-get-result "int main() { return 0 || 0 || 0; }"))
    ;; Mixed operators - && has higher precedence than ||
    ;; a || b && c  means  a || (b && c)
    (= 1 (run-and-get-result "int main() { return 1 || 0 && 0; }"))  ; 1 || (0 && 0) = 1 || 0 = 1
    (= 0 (run-and-get-result "int main() { return 0 || 0 && 1; }"))  ; 0 || (0 && 1) = 0 || 0 = 0
    (= 1 (run-and-get-result "int main() { return 0 || 1 && 1; }"))  ; 0 || (1 && 1) = 0 || 1 = 1
    ;; a && b || c  means  (a && b) || c
    (= 1 (run-and-get-result "int main() { return 0 && 0 || 1; }"))  ; (0 && 0) || 1 = 0 || 1 = 1
    (= 0 (run-and-get-result "int main() { return 0 && 1 || 0; }"))  ; (0 && 1) || 0 = 0 || 0 = 0
    (= 1 (run-and-get-result "int main() { return 1 && 1 || 0; }"))  ; (1 && 1) || 0 = 1 || 0 = 1
    ;; Longer chains
    (= 1 (run-and-get-result "int main() { return 1 && 1 && 1 && 1; }"))
    (= 0 (run-and-get-result "int main() { return 1 && 1 && 1 && 0; }"))
    (= 1 (run-and-get-result "int main() { return 0 || 0 || 0 || 1; }"))
    (= 0 (run-and-get-result "int main() { return 0 || 0 || 0 || 0; }"))
    ;; Complex mixed chains
    (= 1 (run-and-get-result "int main() { return 1 || 0 && 0 || 0; }"))  ; 1 || (0&&0) || 0 = 1
    (= 1 (run-and-get-result "int main() { return 0 && 1 || 1 && 1; }"))  ; (0&&1) || (1&&1) = 0 || 1 = 1
    (= 0 (run-and-get-result "int main() { return 0 && 1 || 0 && 1; }"))  ; (0&&1) || (0&&1) = 0 || 0 = 0
    ;; With variables
    (= 1 (run-and-get-result "int main() { int a=1; int b=1; int c=0; return a && b || c; }"))
    (= 0 (run-and-get-result "int main() { int a=0; int b=0; int c=0; return a || b || c; }"))
    (= 1 (run-and-get-result "int main() { int a=5; int b=3; int c=0; return a && b && !c; }"))))

(deftest test-unary ()
  "Test: unary operations"
  (check
    (result= -5 (run-and-get-result "int main() { return -5; }"
                                    :verbose nil))
    (= 5 (run-and-get-result "int main() { return -(-5); }"))))

(deftest test-precedence ()
  "Test: operator precedence"
  (check
    (= 14 (run-and-get-result "int main() { return 2 + 3 * 4; }"))  ; 2 + 12
    (= 20 (run-and-get-result "int main() { return (2 + 3) * 4; }"))
    (= 7 (run-and-get-result "int main() { return 1 + 2 * 3; }"))))  ; 1 + 6

;;; ===========================================================================
;;; Phase 3 Tests: Control Flow
;;; ===========================================================================

(deftest test-if ()
  "Test: if statements"
  (check
    (= 1 (run-and-get-result "int main() { if (1) return 1; return 0; }"))
    (= 0 (run-and-get-result "int main() { if (0) return 1; return 0; }"))
    (= 2 (run-and-get-result "int main() { if (1) return 2; else return 3; }"))
    (= 3 (run-and-get-result "int main() { if (0) return 2; else return 3; }"))))

(deftest test-if-else-chain ()
  "Test: if-else-if chain"
  (check
    (= 1 (run-and-get-result "
int main() {
  int x;
  x = 10;
  if (x == 5) return 0;
  else if (x == 10) return 1;
  else return 2;
}"))))

(deftest test-while ()
  "Test: while loops"
  (check
    (= 10 (run-and-get-result "
int main() {
  int i;
  int sum;
  i = 0;
  sum = 0;
  while (i < 5) {
    sum = sum + i;
    i = i + 1;
  }
  return sum;
}"))))  ; 0+1+2+3+4 = 10

(deftest test-for ()
  "Test: for loops"
  (check
    (= 10 (run-and-get-result "
int main() {
  int sum;
  int i;
  sum = 0;
  for (i = 0; i < 5; i = i + 1) {
    sum = sum + i;
  }
  return sum;
}"))))  ; 0+1+2+3+4 = 10

(deftest test-break ()
  "Test: break statement"
  (check
    (= 3 (run-and-get-result "
int main() {
  int i;
  i = 0;
  while (1) {
    if (i == 3) break;
    i = i + 1;
  }
  return i;
}"))))

(deftest test-continue ()
  "Test: continue statement"
  (check
    (= 25 (run-and-get-result "
int main() {
  int i;
  int sum;
  i = 0;
  sum = 0;
  while (i < 10) {
    i = i + 1;
    if (i % 2 == 0) continue;
    sum = sum + i;
  }
  return sum;
}"))))  ; 1+3+5+7+9 = 25

;;; ===========================================================================
;;; Phase 4 Tests: Variables and Functions
;;; ===========================================================================

(deftest test-local-vars ()
  "Test: local variables"
  (check
    (= 10 (run-and-get-result "
int main() {
  int x;
  x = 10;
  return x;
}"))
    (= 30 (run-and-get-result "
int main() {
  int x;
  int y;
  x = 10;
  y = 20;
  return x + y;
}"))))

(deftest test-function-call ()
  "Test: function calls"
  (check
    (= 42 (run-and-get-result "
int answer() { return 42; }
int main() { return answer(); }"))
    (= 5 (run-and-get-result "
int add(int a, int b) { return a + b; }
int main() { return add(2, 3); }"))))

(deftest test-recursion ()
  "Test: recursive function (factorial)"
  (check
    (= 120 (run-and-get-result "
int fact(int n) {
  if (n <= 1) return 1;
  return n * fact(n - 1);
}
int main() { return fact(5); }"
                               :max-cycles 100000))))

(deftest test-fibonacci ()
  "Test: Fibonacci sequence"
  (check
    (= 55 (run-and-get-result "
int fib(int n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}
int main() { return fib(10); }"
                              :max-cycles 1000000))))

;;; ===========================================================================
;;; Phase 5 Tests: Pointers and Arrays
;;; ===========================================================================

(deftest test-pointers ()
  "Test: pointer operations"
  (check
    (= 42 (run-and-get-result "
int main() {
  int x;
  int *p;
  x = 42;
  p = &x;
  return *p;
}"))))

(deftest test-arrays ()
  "Test: array operations"
  (check
    (= 15 (run-and-get-result "
int main() {
  int arr[5];
  int i;
  int sum;
  for (i = 0; i < 5; i = i + 1) {
    arr[i] = i + 1;
  }
  sum = 0;
  for (i = 0; i < 5; i = i + 1) {
    sum = sum + arr[i];
  }
  return sum;
}"
                              :max-cycles 50000))))  ; 1+2+3+4+5 = 15

;;; ===========================================================================
;;; All Tests
;;; ===========================================================================

(deftest test-phase1 ()
  "Run Phase 1 tests"
  (combine-results
    (test-return-literal)))

(deftest test-phase2 ()
  "Run Phase 2 tests"
  (combine-results
    (test-addition)
    (test-subtraction)
    (test-multiplication)
    (test-division)
    (test-modulo)
    (test-bitwise)
    (test-shifts)
    (test-comparisons)
    (test-logical)
    (test-logical-chaining)
    (test-unary)
    (test-precedence)))

(deftest test-phase3 ()
  "Run Phase 3 tests"
  (combine-results
    (test-if)
    (test-if-else-chain)
    (test-while)
    (test-for)
    (test-break)
    (test-continue)))

(deftest test-phase4 ()
  "Run Phase 4 tests"
  (combine-results
    (test-local-vars)
    (test-function-call)
    (test-recursion)
    (test-fibonacci)))

(deftest test-phase5 ()
  "Run Phase 5 tests"
  (combine-results
    (test-pointers)
    (test-arrays)))

;;; ===========================================================================
;;; Phase 6 Tests: Globals and Polish
;;; ===========================================================================

(deftest test-global-vars ()
  "Test: global variables"
  (check
    (= 42 (run-and-get-result "int x; int main() { x = 42; return x; }"))
    (= 35 (run-and-get-result "
int counter;
void inc(int n) { counter = counter + n; }
int main() {
  counter = 5;
  inc(10);
  inc(20);
  return counter;
}" :max-cycles 10000))))

(deftest test-multiple-globals ()
  "Test: multiple global variables"
  (check
    (= 30 (run-and-get-result "
int a;
int b;
int c;
int main() {
  a = 10;
  b = 20;
  c = a + b;
  return c;
}"))))

(deftest test-phase6 ()
  "Run Phase 6 tests"
  (combine-results
    (test-global-vars)
    (test-multiple-globals)))

;;; ===========================================================================
;;; Phase 7 Tests: Extended Integer Types
;;; ===========================================================================

(deftest test-type-sizes ()
  "Test: sizeof for various types"
  (check
    ;; Basic types
    (= 1 (run-and-get-result "int main() { return sizeof(char); }"))
    (= 2 (run-and-get-result "int main() { return sizeof(short); }"))
    (= 4 (run-and-get-result "int main() { return sizeof(int); }"))
    (= 4 (run-and-get-result "int main() { return sizeof(long); }"))
    ;; C99 fixed-width types
    (= 1 (run-and-get-result "int main() { return sizeof(int8_t); }"))
    (= 1 (run-and-get-result "int main() { return sizeof(uint8_t); }"))
    (= 2 (run-and-get-result "int main() { return sizeof(int16_t); }"))
    (= 2 (run-and-get-result "int main() { return sizeof(uint16_t); }"))
    (= 4 (run-and-get-result "int main() { return sizeof(int32_t); }"))
    (= 4 (run-and-get-result "int main() { return sizeof(uint32_t); }"))
    ;; Unsigned variants
    (= 1 (run-and-get-result "int main() { return sizeof(unsigned char); }"))
    (= 2 (run-and-get-result "int main() { return sizeof(unsigned short); }"))
    (= 4 (run-and-get-result "int main() { return sizeof(unsigned int); }"))
    (= 4 (run-and-get-result "int main() { return sizeof(unsigned long); }"))))

(deftest test-uint8-boundaries ()
  "Test: uint8_t boundary conditions"
  (check
    ;; Max value
    (= 255 (run-and-get-result "
int main() {
  uint8_t x;
  x = 255;
  return x;
}"))
    ;; Overflow wraps
    (= 0 (run-and-get-result "
int main() {
  uint8_t x;
  x = 255;
  x = x + 1;
  return x;
}"))
    ;; Min value
    (= 0 (run-and-get-result "
int main() {
  uint8_t x;
  x = 0;
  return x;
}"))))

(deftest test-int8-boundaries ()
  "Test: int8_t boundary conditions"
  (check
    ;; Positive max
    (= 127 (run-and-get-result "
int main() {
  int8_t x;
  x = 127;
  return x;
}"))
    ;; Negative value
    (result= -1 (run-and-get-result "
int main() {
  int8_t x;
  x = -1;
  return x;
}"))
    ;; Negative 50
    (result= -50 (run-and-get-result "
int main() {
  int8_t x;
  x = -50;
  return x;
}"))))

(deftest test-uint16-boundaries ()
  "Test: uint16_t boundary conditions"
  (check
    ;; Max value
    (= 65535 (run-and-get-result "
int main() {
  uint16_t x;
  x = 65535;
  return x;
}"))
    ;; Overflow wraps
    (= 0 (run-and-get-result "
int main() {
  uint16_t x;
  x = 65535;
  x = x + 1;
  return x;
}"))))

(deftest test-int16-boundaries ()
  "Test: int16_t boundary conditions"
  (check
    ;; Positive max
    (= 32767 (run-and-get-result "
int main() {
  int16_t x;
  x = 32767;
  return x;
}"))
    ;; Negative value
    (result= -1 (run-and-get-result "
int main() {
  int16_t x;
  x = -1;
  return x;
}"))
    ;; Negative 1000
    (result= -1000 (run-and-get-result "
int main() {
  int16_t x;
  x = -1000;
  return x;
}"))))

(deftest test-sign-extension ()
  "Test: sign extension when assigning to larger types"
  (check
    ;; Negative int8_t to int
    (result= -50 (run-and-get-result "
int main() {
  int8_t x;
  int y;
  x = -50;
  y = x;
  return y;
}"))
    ;; Positive int8_t to int (no sign extension needed)
    (= 50 (run-and-get-result "
int main() {
  int8_t x;
  int y;
  x = 50;
  y = x;
  return y;
}"))
    ;; Negative int16_t to int
    (result= -1000 (run-and-get-result "
int main() {
  int16_t x;
  int y;
  x = -1000;
  y = x;
  return y;
}"))))

(deftest test-char-array ()
  "Test: char arrays"
  (check
    (= 3 (run-and-get-result "
int main() {
  char arr[4];
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  return arr[2];
}" :max-cycles 20000))
    ;; Sum of elements
    (= 10 (run-and-get-result "
int main() {
  char arr[4];
  int sum;
  int i;
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  sum = 0;
  for (i = 0; i < 4; i = i + 1) {
    sum = sum + arr[i];
  }
  return sum;
}" :max-cycles 50000))))

(deftest test-short-array ()
  "Test: short arrays"
  (check
    (= 300 (run-and-get-result "
int main() {
  short arr[4];
  arr[0] = 100;
  arr[1] = 200;
  arr[2] = 300;
  arr[3] = 400;
  return arr[2];
}" :max-cycles 20000))
    ;; Sum of elements
    (= 1000 (run-and-get-result "
int main() {
  short arr[4];
  int sum;
  int i;
  arr[0] = 100;
  arr[1] = 200;
  arr[2] = 300;
  arr[3] = 400;
  sum = 0;
  for (i = 0; i < 4; i = i + 1) {
    sum = sum + arr[i];
  }
  return sum;
}" :max-cycles 50000))))

(deftest test-unsigned-comparison ()
  "Test: unsigned value comparisons"
  (check
    ;; uint8_t comparison
    (= 1 (run-and-get-result "
int main() {
  uint8_t a;
  uint8_t b;
  a = 200;
  b = 100;
  return a > b;
}"))
    ;; uint8_t equal
    (= 1 (run-and-get-result "
int main() {
  uint8_t a;
  uint8_t b;
  a = 255;
  b = 255;
  return a == b;
}"))))

(deftest test-mixed-size-arithmetic ()
  "Test: arithmetic with mixed-size types"
  (check
    ;; int8_t + int8_t
    (= 10 (run-and-get-result "
int main() {
  int8_t a;
  int8_t b;
  a = 3;
  b = 7;
  return a + b;
}"))
    ;; int16_t + int16_t
    (= 1500 (run-and-get-result "
int main() {
  int16_t a;
  int16_t b;
  a = 500;
  b = 1000;
  return a + b;
}"))))

(deftest test-phase7 ()
  "Run Phase 7 tests (Extended Integer Types)"
  (combine-results
    (test-type-sizes)
    (test-uint8-boundaries)
    (test-int8-boundaries)
    (test-uint16-boundaries)
    (test-int16-boundaries)
    (test-sign-extension)
    (test-char-array)
    (test-short-array)
    (test-unsigned-comparison)
    (test-mixed-size-arithmetic)))

;;; ===========================================================================
;;; Phase 8 Tests: Function Parameters
;;; ===========================================================================

(deftest test-single-param ()
  "Test: single parameter passing"
  (check
    ;; Identity function
    (= 42 (run-and-get-result "
int id(int x) { return x; }
int main() { return id(42); }"))
    ;; Negate
    (result= -10 (run-and-get-result "
int neg(int x) { return -x; }
int main() { return neg(10); }"))
    ;; Double
    (= 20 (run-and-get-result "
int dbl(int x) { return x + x; }
int main() { return dbl(10); }"))))

(deftest test-two-params ()
  "Test: two parameter passing"
  (check
    ;; Add
    (= 30 (run-and-get-result "
int add(int a, int b) { return a + b; }
int main() { return add(10, 20); }"))
    ;; Subtract (order matters)
    (= 5 (run-and-get-result "
int sub(int a, int b) { return a - b; }
int main() { return sub(15, 10); }"))
    ;; Multiply
    (= 56 (run-and-get-result "
int mul(int a, int b) { return a * b; }
int main() { return mul(7, 8); }"))))

(deftest test-three-params ()
  "Test: three parameter passing"
  (check
    ;; Sum of three
    (= 60 (run-and-get-result "
int sum3(int a, int b, int c) { return a + b + c; }
int main() { return sum3(10, 20, 30); }"))
    ;; Weighted sum
    (= 32 (run-and-get-result "
int weighted(int a, int b, int c) { return a * 1 + b * 2 + c * 3; }
int main() { return weighted(2, 3, 8); }"))  ; 2 + 6 + 24 = 32
    ;; Min of three
    (= 5 (run-and-get-result "
int min3(int a, int b, int c) {
  int m;
  m = a;
  if (b < m) m = b;
  if (c < m) m = c;
  return m;
}
int main() { return min3(10, 5, 15); }"))))

(deftest test-four-params ()
  "Test: four parameters (all in registers)"
  (check
    ;; Sum of four
    (= 100 (run-and-get-result "
int sum4(int a, int b, int c, int d) { return a + b + c + d; }
int main() { return sum4(10, 20, 30, 40); }"))
    ;; Product of differences
    (= 200 (run-and-get-result "
int calc(int a, int b, int c, int d) { return (a - b) * (c - d); }
int main() { return calc(30, 10, 50, 40); }"))  ; (20) * (10) = 200
    ;; Nested expression with all params
    (= 24 (run-and-get-result "
int f(int a, int b, int c, int d) { return a * b + c * d; }
int main() { return f(2, 3, 6, 3); }"))))  ; 6 + 18 = 24

(deftest test-five-plus-params ()
  "Test: five or more parameters (stack parameters)"
  (check
    ;; Five parameters
    (= 150 (run-and-get-result "
int sum5(int a, int b, int c, int d, int e) {
  return a + b + c + d + e;
}
int main() { return sum5(10, 20, 30, 40, 50); }"
                               :max-cycles 20000))
    ;; Six parameters
    (= 210 (run-and-get-result "
int sum6(int a, int b, int c, int d, int e, int f) {
  return a + b + c + d + e + f;
}
int main() { return sum6(10, 20, 30, 40, 50, 60); }"
                               :max-cycles 20000))
    ;; Seven parameters with computation
    (= 28 (run-and-get-result "
int compute(int a, int b, int c, int d, int e, int f, int g) {
  return a + b + c + d + e + f + g;
}
int main() { return compute(1, 2, 3, 4, 5, 6, 7); }"
                              :max-cycles 20000))))

(deftest test-param-modification ()
  "Test: parameters are pass-by-value (modifications don't affect caller)"
  (check
    ;; Modify parameter inside function
    (= 10 (run-and-get-result "
int modify(int x) {
  x = x + 100;
  return x;
}
int main() {
  int a;
  a = 10;
  modify(a);
  return a;
}"))
    ;; Return modified value
    (= 110 (run-and-get-result "
int modify(int x) {
  x = x + 100;
  return x;
}
int main() {
  int a;
  a = 10;
  return modify(a);
}"))))

(deftest test-nested-calls ()
  "Test: nested function calls with parameters"
  (check
    ;; f(g(x))
    (= 24 (run-and-get-result "
int dbl(int x) { return x * 2; }
int triple(int x) { return x * 3; }
int main() { return dbl(triple(4)); }"))  ; triple(4)=12, dbl(12)=24
    ;; f(g(x), h(y))
    (= 35 (run-and-get-result "
int add(int a, int b) { return a + b; }
int dbl(int x) { return x * 2; }
int triple(int x) { return x * 3; }
int main() { return add(dbl(10), triple(5)); }"))  ; 20 + 15 = 35
    ;; Deeply nested
    (= 48 (run-and-get-result "
int dbl(int x) { return x * 2; }
int main() { return dbl(dbl(dbl(6))); }"))))  ; 6->12->24->48

(deftest test-char-params ()
  "Test: char type parameters"
  (check
    ;; Pass char value
    (= 65 (run-and-get-result "
int getval(char c) { return c; }
int main() { return getval(65); }"))
    ;; Char arithmetic in function
    (= 10 (run-and-get-result "
int diff(char a, char b) { return a - b; }
int main() { return diff(75, 65); }"))))

(deftest test-short-params ()
  "Test: short type parameters"
  (check
    ;; Pass short value
    (= 1000 (run-and-get-result "
int getval(short s) { return s; }
int main() { return getval(1000); }"))
    ;; Short arithmetic in function
    (= 30000 (run-and-get-result "
int add_shorts(short a, short b) { return a + b; }
int main() { return add_shorts(10000, 20000); }"))))

(deftest test-mixed-type-params ()
  "Test: mixed type parameters"
  (check
    ;; char, short, int mixed
    (= 111 (run-and-get-result "
int mixed(char a, short b, int c) { return a + b + c; }
int main() { return mixed(1, 10, 100); }"))
    ;; Different order
    (= 321 (run-and-get-result "
int mixed(int a, char b, short c) { return a + b + c; }
int main() { return mixed(300, 1, 20); }"))))

(deftest test-phase8-params ()
  "Run Phase 8 parameter tests"
  (combine-results
    (test-single-param)
    (test-two-params)
    (test-three-params)
    (test-four-params)
    (test-five-plus-params)
    (test-param-modification)
    (test-nested-calls)
    (test-char-params)
    (test-short-params)
    (test-mixed-type-params)))

;;; ===========================================================================
;;; Phase 9 Tests: Pointers and Pointer Arithmetic
;;; ===========================================================================

(deftest test-pointer-basics ()
  "Test: basic pointer operations"
  (check
    ;; Address-of and dereference
    (= 42 (run-and-get-result "
int main() {
  int x;
  int *p;
  x = 42;
  p = &x;
  return *p;
}"))
    ;; Modify through pointer
    (= 100 (run-and-get-result "
int main() {
  int x;
  int *p;
  x = 42;
  p = &x;
  *p = 100;
  return x;
}"))
    ;; Pointer reassignment
    (= 20 (run-and-get-result "
int main() {
  int a;
  int b;
  int *p;
  a = 10;
  b = 20;
  p = &a;
  p = &b;
  return *p;
}"))))

(deftest test-pointer-to-array ()
  "Test: pointers to array elements"
  (check
    ;; Point to first element
    (= 100 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 100;
  arr[1] = 200;
  arr[2] = 300;
  p = &arr[0];
  return *p;
}"))
    ;; Point to middle element
    (= 200 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 100;
  arr[1] = 200;
  arr[2] = 300;
  p = &arr[1];
  return *p;
}"))
    ;; Array name as pointer
    (= 100 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 100;
  p = arr;
  return *p;
}" :max-cycles 20000))))

(deftest test-pointer-arithmetic-int ()
  "Test: pointer arithmetic with int pointers"
  (check
    ;; Pointer + 1
    (= 200 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 100;
  arr[1] = 200;
  arr[2] = 300;
  p = arr;
  p = p + 1;
  return *p;
}" :max-cycles 20000))
    ;; Pointer + 2
    (= 300 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 100;
  arr[1] = 200;
  arr[2] = 300;
  p = arr;
  p = p + 2;
  return *p;
}" :max-cycles 20000))
    ;; Pointer - 1
    (= 200 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 100;
  arr[1] = 200;
  arr[2] = 300;
  p = &arr[2];
  p = p - 1;
  return *p;
}" :max-cycles 20000))))

(deftest test-pointer-arithmetic-char ()
  "Test: pointer arithmetic with char pointers (1-byte stride)"
  (check
    ;; Char pointer + 1
    (= 2 (run-and-get-result "
int main() {
  char arr[4];
  char *p;
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  p = arr;
  p = p + 1;
  return *p;
}" :max-cycles 20000))
    ;; Char pointer + 3
    (= 4 (run-and-get-result "
int main() {
  char arr[4];
  char *p;
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  p = arr;
  p = p + 3;
  return *p;
}" :max-cycles 20000))))

(deftest test-pointer-arithmetic-short ()
  "Test: pointer arithmetic with short pointers (2-byte stride)"
  (check
    ;; Short pointer + 1
    (= 2000 (run-and-get-result "
int main() {
  short arr[4];
  short *p;
  arr[0] = 1000;
  arr[1] = 2000;
  arr[2] = 3000;
  arr[3] = 4000;
  p = arr;
  p = p + 1;
  return *p;
}" :max-cycles 20000))
    ;; Short pointer + 2
    (= 3000 (run-and-get-result "
int main() {
  short arr[4];
  short *p;
  arr[0] = 1000;
  arr[1] = 2000;
  arr[2] = 3000;
  arr[3] = 4000;
  p = arr;
  p = p + 2;
  return *p;
}" :max-cycles 20000))))

(deftest test-pointer-increment ()
  "Test: pointer increment/decrement in loops"
  (check
    ;; Sum array using pointer increment
    (= 15 (run-and-get-result "
int main() {
  int arr[5];
  int *p;
  int sum;
  int i;
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  arr[4] = 5;
  p = arr;
  sum = 0;
  for (i = 0; i < 5; i = i + 1) {
    sum = sum + *p;
    p = p + 1;
  }
  return sum;
}" :max-cycles 50000))))

(deftest test-pointer-comparison ()
  "Test: pointer comparison operations"
  (check
    ;; Pointer equality
    (= 1 (run-and-get-result "
int main() {
  int x;
  int *p;
  int *q;
  p = &x;
  q = &x;
  return p == q;
}"))
    ;; Pointer inequality
    (= 1 (run-and-get-result "
int main() {
  int a;
  int b;
  int *p;
  int *q;
  p = &a;
  q = &b;
  return p != q;
}"))))

(deftest test-pointer-as-param ()
  "Test: pointer as function parameter"
  (check
    ;; Read through pointer param
    (= 42 (run-and-get-result "
int read_ptr(int *p) { return *p; }
int main() {
  int x;
  x = 42;
  return read_ptr(&x);
}"))
    ;; Write through pointer param (out parameter)
    (= 100 (run-and-get-result "
void set_val(int *p, int v) { *p = v; }
int main() {
  int x;
  x = 0;
  set_val(&x, 100);
  return x;
}" :max-cycles 20000))
    ;; Swap using pointers
    (= 20 (run-and-get-result "
void swap(int *a, int *b) {
  int t;
  t = *a;
  *a = *b;
  *b = t;
}
int main() {
  int x;
  int y;
  x = 10;
  y = 20;
  swap(&x, &y);
  return x;
}" :max-cycles 20000))))

(deftest test-array-as-param ()
  "Test: array as function parameter (decays to pointer)"
  (check
    ;; Sum array elements
    (= 15 (run-and-get-result "
int sum_arr(int *arr, int n) {
  int i;
  int s;
  s = 0;
  for (i = 0; i < n; i = i + 1) {
    s = s + arr[i];
  }
  return s;
}
int main() {
  int arr[5];
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  arr[4] = 5;
  return sum_arr(arr, 5);
}" :max-cycles 50000))
    ;; Modify array through pointer param
    (= 100 (run-and-get-result "
void set_first(int *arr, int val) {
  arr[0] = val;
}
int main() {
  int arr[3];
  arr[0] = 0;
  set_first(arr, 100);
  return arr[0];
}" :max-cycles 20000))))

(deftest test-pointer-subscript ()
  "Test: pointer used with subscript notation"
  (check
    ;; p[0] same as *p
    (= 10 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 10;
  arr[1] = 20;
  arr[2] = 30;
  p = arr;
  return p[0];
}" :max-cycles 20000))
    ;; p[1] same as *(p+1)
    (= 20 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 10;
  arr[1] = 20;
  arr[2] = 30;
  p = arr;
  return p[1];
}" :max-cycles 20000))
    ;; p[2]
    (= 30 (run-and-get-result "
int main() {
  int arr[3];
  int *p;
  arr[0] = 10;
  arr[1] = 20;
  arr[2] = 30;
  p = arr;
  return p[2];
}" :max-cycles 20000))))

(deftest test-double-pointer ()
  "Test: pointer to pointer"
  (check
    ;; Basic double pointer
    (= 42 (run-and-get-result "
int main() {
  int x;
  int *p;
  int **pp;
  x = 42;
  p = &x;
  pp = &p;
  return **pp;
}"))
    ;; Modify through double pointer
    (= 100 (run-and-get-result "
int main() {
  int x;
  int *p;
  int **pp;
  x = 0;
  p = &x;
  pp = &p;
  **pp = 100;
  return x;
}"))))

(deftest test-pointer-null ()
  "Test: null pointer and zero comparison"
  (check
    ;; Pointer initialized to zero
    (= 1 (run-and-get-result "
int main() {
  int *p;
  p = 0;
  return p == 0;
}"))
    ;; Non-null pointer
    (= 0 (run-and-get-result "
int main() {
  int x;
  int *p;
  p = &x;
  return p == 0;
}"))))

(deftest test-phase9-pointers ()
  "Run Phase 9 pointer tests"
  (combine-results
    (test-pointer-basics)
    (test-pointer-to-array)
    (test-pointer-arithmetic-int)
    (test-pointer-arithmetic-char)
    (test-pointer-arithmetic-short)
    (test-pointer-increment)
    (test-pointer-comparison)
    (test-pointer-as-param)
    (test-array-as-param)
    (test-pointer-subscript)
    (test-double-pointer)
    (test-pointer-null)))

;;; ===========================================================================
;;; Phase 10 Tests: Constant Folding Optimization
;;; ===========================================================================

(defun run-optimized (source &key (max-cycles 10000) (optimize-size nil))
  "Compile with optimization enabled, run, and return the result"
  (handler-case
      (let* ((asm (compile-c source :optimize t :annotate nil :optimize-size optimize-size))
             (mcode (assemble (strip-asm-comments asm) nil))
             (dmem (lr-emulator:make-dmem #x10000))
             (emul (lr-emulator:make-emulator mcode dmem :shared-mem nil :debug nil)))
        (lr-emulator:run-emul emul max-cycles nil)
        (let ((result (aref (lr-emulator::processor-state-r
                             (lr-emulator:emulated-system-processor emul))
                            10)))
          ;; Save output if enabled
          (when *save-test-outputs*
            (let ((filename (make-test-output-filename :optimize t)))
              (save-compilation-output source filename :run-result result :optimize t)))
          result))
    (error (e)
      (format t "Error in optimized run: ~a~%" e)
      ;; Still try to save on error
      (when *save-test-outputs*
        (let ((filename (make-test-output-filename :optimize t)))
          (handler-case
              (save-compilation-output source filename :run-result "ERROR" :optimize t)
            (error () nil))))
      nil)))

(deftest test-fold-arithmetic ()
  "Test: constant folding of arithmetic operations"
  (check
    ;; Basic arithmetic
    (= 5 (run-optimized "int main() { return 2 + 3; }"))
    (= 200 (run-optimized "int main() { return 10 * 20; }"))
    (= 7 (run-optimized "int main() { return 15 - 8; }"))
    (= 5 (run-optimized "int main() { return 15 / 3; }"))
    (= 2 (run-optimized "int main() { return 17 % 5; }"))
    ;; Nested arithmetic
    (= 14 (run-optimized "int main() { return 2 + 3 * 4; }"))
    (= 20 (run-optimized "int main() { return (2 + 3) * 4; }"))
    (= 26 (run-optimized "int main() { return 2 * 3 + 4 * 5; }"))
    ;; Complex nesting
    (= 14 (run-optimized "int main() { return 2 * (3 + 4); }"))
    (= 47 (run-optimized "int main() { return (1 + 2) * (3 + 4) + (5 + 6) + (7 + 8); }"))))

(deftest test-fold-bitwise ()
  "Test: constant folding of bitwise operations"
  (check
    ;; AND, OR, XOR
    (= 15 (run-optimized "int main() { return 0xFF & 0x0F; }"))
    (= 255 (run-optimized "int main() { return 0xF0 | 0x0F; }"))
    (= 240 (run-optimized "int main() { return 0xFF ^ 0x0F; }"))
    ;; Shifts
    (= 8 (run-optimized "int main() { return 2 << 2; }"))
    (= 4 (run-optimized "int main() { return 16 >> 2; }"))
    ;; Nested bitwise
    (= 7 (run-optimized "int main() { return (0xFF & 0x0F) >> 1; }"))))

(deftest test-fold-comparison ()
  "Test: constant folding of comparison operations"
  (check
    ;; Equality
    (= 1 (run-optimized "int main() { return 5 == 5; }"))
    (= 0 (run-optimized "int main() { return 5 == 6; }"))
    (= 1 (run-optimized "int main() { return 5 != 6; }"))
    (= 0 (run-optimized "int main() { return 5 != 5; }"))
    ;; Relational
    (= 1 (run-optimized "int main() { return 3 < 5; }"))
    (= 0 (run-optimized "int main() { return 5 < 3; }"))
    (= 1 (run-optimized "int main() { return 5 > 3; }"))
    (= 0 (run-optimized "int main() { return 3 > 5; }"))
    (= 1 (run-optimized "int main() { return 5 <= 5; }"))
    (= 1 (run-optimized "int main() { return 5 >= 5; }"))
    ;; Nested comparison in expression
    (= 2 (run-optimized "int main() { return (3 < 5) + (5 > 3); }"))))

(deftest test-fold-logical ()
  "Test: constant folding of logical operations"
  (check
    ;; AND
    (= 1 (run-optimized "int main() { return 1 && 1; }"))
    (= 0 (run-optimized "int main() { return 1 && 0; }"))
    (= 0 (run-optimized "int main() { return 0 && 1; }"))
    (= 0 (run-optimized "int main() { return 0 && 0; }"))
    ;; OR
    (= 1 (run-optimized "int main() { return 1 || 0; }"))
    (= 1 (run-optimized "int main() { return 0 || 1; }"))
    (= 1 (run-optimized "int main() { return 1 || 1; }"))
    (= 0 (run-optimized "int main() { return 0 || 0; }"))
    ;; NOT
    (= 0 (run-optimized "int main() { return !1; }"))
    (= 1 (run-optimized "int main() { return !0; }"))
    (= 0 (run-optimized "int main() { return !!0; }"))
    (= 1 (run-optimized "int main() { return !!42; }"))))

(deftest test-fold-unary ()
  "Test: constant folding of unary operations"
  (check
    ;; Negation
    (result= -5 (run-optimized "int main() { return -5; }"))
    (= 5 (run-optimized "int main() { return -(-5); }"))
    (result= -100 (run-optimized "int main() { return -(50 + 50); }"))
    ;; Bitwise NOT
    (result= -1 (run-optimized "int main() { return ~0; }"))
    (= 0 (run-optimized "int main() { return ~(~0); }"))))

(deftest test-fold-sizeof ()
  "Test: constant folding of sizeof"
  (check
    (= 4 (run-optimized "int main() { return sizeof(int); }"))
    (= 1 (run-optimized "int main() { return sizeof(char); }"))
    (= 2 (run-optimized "int main() { return sizeof(short); }"))
    ;; sizeof in expression
    (= 8 (run-optimized "int main() { return sizeof(int) + sizeof(int); }"))
    (= 4 (run-optimized "int main() { return sizeof(int) * sizeof(char); }"))))

;; Note: Ternary operator test skipped - ternary (?:) not fully implemented in lexer/parser

(deftest test-fold-cast ()
  "Test: constant folding of cast expressions"
  (check
    ;; Cast to int (no change)
    (= 42 (run-optimized "int main() { return (int)42; }"))
    ;; Cast to char (mask to 8 bits)
    (= 255 (run-optimized "int main() { return (char)255; }"))
    (= 0 (run-optimized "int main() { return (char)256; }"))
    ;; Cast of expression result
    (= 44 (run-optimized "int main() { return (int)(42 + 2); }"))))

(deftest test-fold-mixed ()
  "Test: constant folding with mixed expressions (variables and constants)"
  (check
    ;; Constant part should be folded even with variables
    (= 15 (run-optimized "
int main() {
  int x;
  x = 5;
  return x + 10;
}"))
    ;; Entire constant sub-expression should be folded
    (= 25 (run-optimized "
int main() {
  int x;
  x = 5;
  return x + (2 + 3) * 4;
}"))
    ;; Multiple constants in different branches: (2*3) + (4*5) + 5 = 6 + 20 + 5 = 31
    (= 31 (run-optimized "
int main() {
  int x;
  x = 5;
  return (2 * 3) + (4 * 5) + x;
}"))))

(deftest test-fold-preserves-behavior ()
  "Test: optimized code produces same results as unoptimized"
  (check
    ;; Complex expressions
    (= (run-and-get-result "int main() { return 2 + 3 * 4 - 5; }")
       (run-optimized "int main() { return 2 + 3 * 4 - 5; }"))
    (= (run-and-get-result "int main() { return (10 / 3) * 3 + (10 % 3); }")
       (run-optimized "int main() { return (10 / 3) * 3 + (10 % 3); }"))
    ;; Bitwise operations
    (= (run-and-get-result "int main() { return (0xAB << 4) & 0xFF0; }")
       (run-optimized "int main() { return (0xAB << 4) & 0xFF0; }"))
    ;; Mixed with control flow
    (= (run-and-get-result "
int main() {
  int sum;
  int i;
  sum = 0;
  for (i = 0; i < 5; i = i + 1) {
    sum = sum + (2 * 3);
  }
  return sum;
}" :max-cycles 50000)
       (run-optimized "
int main() {
  int sum;
  int i;
  sum = 0;
  for (i = 0; i < 5; i = i + 1) {
    sum = sum + (2 * 3);
  }
  return sum;
}" :max-cycles 50000))))

(deftest test-phase10-constant-folding ()
  "Run Phase 10 constant folding tests"
  (combine-results
    (test-fold-arithmetic)
    (test-fold-bitwise)
    (test-fold-comparison)
    (test-fold-logical)
    (test-fold-unary)
    (test-fold-sizeof)
    ;; test-fold-ternary skipped - ternary operator not implemented in lexer
    (test-fold-cast)
    (test-fold-mixed)
    (test-fold-preserves-behavior)))

;;; ===========================================================================
;;; Phase 11 Tests: Function Inlining
;;; ===========================================================================

(deftest test-inline-explicit ()
  "Test: explicit inline keyword"
  (check
    ;; Simple inline function
    (= 30 (run-optimized "
inline int add(int a, int b) { return a + b; }
int main() { return add(10, 20); }"))
    ;; Inline function with computation
    (= 25 (run-optimized "
inline int square(int x) { return x * x; }
int main() { return square(5); }"))
    ;; Multiple inline calls
    (= 50 (run-optimized "
inline int double_it(int x) { return x + x; }
int main() { return double_it(10) + double_it(15); }"))))

(deftest test-inline-auto ()
  "Test: automatic inlining of small functions"
  (check
    ;; Small function should be auto-inlined
    (= 42 (run-optimized "
int inc(int x) { return x + 1; }
int main() { return inc(41); }"))
    ;; Small function with single return
    (= 10 (run-optimized "
int passthru(int x) { return x; }
int main() { return passthru(10); }"))
    ;; Multiple auto-inlines: add3(7)=10, add2(10)=12, add1(12)=13
    (= 13 (run-optimized "
int add1(int x) { return x + 1; }
int add2(int x) { return x + 2; }
int add3(int x) { return x + 3; }
int main() { return add1(add2(add3(7))); }"))))

(deftest test-inline-multiple-returns ()
  "Test: inlining functions with multiple return statements"
  (check
    ;; If-else with multiple returns
    (= 5 (run-optimized "
inline int my_abs(int x) {
  if (x < 0) return -x;
  return x;
}
int main() { return my_abs(-5); }"))
    ;; Return from if without else
    (= 10 (run-optimized "
inline int my_abs(int x) {
  if (x < 0) return -x;
  return x;
}
int main() { return my_abs(10); }"))
    ;; More complex control flow: sign(10)=1, sign(-10)=-1, sign(0)=0, sign(5)=1 => 1+(-1)+0+1 = 1
    (= 1 (run-optimized "
inline int sign(int x) {
  if (x > 0) return 1;
  if (x < 0) return -1;
  return 0;
}
int main() { return sign(10) + sign(-10) + sign(0) + sign(5); }"))))

(deftest test-inline-recursive-not-inlined ()
  "Test: recursive functions should NOT be inlined"
  (check
    ;; Recursive factorial - should work correctly but not be inlined
    (= 120 (run-optimized "
inline int fact(int n) {
  if (n <= 1) return 1;
  return n * fact(n - 1);
}
int main() { return fact(5); }" :max-cycles 100000))
    ;; Recursive fibonacci
    (= 8 (run-optimized "
inline int fib(int n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}
int main() { return fib(6); }" :max-cycles 1000000))))

(deftest test-inline-with-locals ()
  "Test: inlining functions with local variables"
  (check
    ;; Function with local variable: 10*2 + 10 = 30
    (= 30 (run-optimized "
inline int compute(int x) {
  int temp;
  temp = x * 2;
  return temp + x;
}
int main() { return compute(10); }"))
    ;; Function with multiple locals: sum=10+5=15, diff=10-5=5, 15*5=75
    (= 75 (run-optimized "
inline int calc(int a, int b) {
  int sum;
  int diff;
  sum = a + b;
  diff = a - b;
  return sum * diff;
}
int main() { return calc(10, 5); }" :max-cycles 50000))))

(deftest test-inline-nested-calls ()
  "Test: inlining with nested function calls"
  (check
    ;; Inline calling another inline
    (= 16 (run-optimized "
inline int square(int x) { return x * x; }
inline int quad(int x) { return square(square(x)); }
int main() { return quad(2); }"))  ; 2^4 = 16
    ;; Chain of inlines
    (= 15 (run-optimized "
inline int add5(int x) { return x + 5; }
inline int add10(int x) { return add5(add5(x)); }
int main() { return add10(5); }"))))  ; 5+5+5 = 15

(deftest test-inline-expression-context ()
  "Test: inlined function used in various expression contexts"
  (check
    ;; In binary expression
    (= 25 (run-optimized "
inline int sq(int x) { return x * x; }
int main() { return sq(3) + sq(4); }"))  ; 9 + 16 = 25
    ;; In comparison (ternary not supported, use if-else)
    (= 1 (run-optimized "
inline int sq(int x) { return x * x; }
int main() { if (sq(3) < sq(4)) return 1; return 0; }"))
    ;; As array index
    (= 20 (run-optimized "
inline int idx(int x) { return x + 1; }
int main() {
  int arr[5];
  arr[0] = 10;
  arr[1] = 20;
  arr[2] = 30;
  return arr[idx(0)];
}" :max-cycles 20000))))

(deftest test-inline-preserves-behavior ()
  "Test: inlined code produces same results as non-inlined"
  (check
    ;; Compare optimized vs non-optimized for simple function
    (= (run-and-get-result "
int add(int a, int b) { return a + b; }
int main() { return add(10, 20); }")
       (run-optimized "
int add(int a, int b) { return a + b; }
int main() { return add(10, 20); }"))
    ;; Compare for function with locals
    (= (run-and-get-result "
int compute(int x) {
  int y;
  y = x * 2;
  return y + 1;
}
int main() { return compute(10); }")
       (run-optimized "
int compute(int x) {
  int y;
  y = x * 2;
  return y + 1;
}
int main() { return compute(10); }"))
    ;; Compare for multiple calls
    (= (run-and-get-result "
int f(int x) { return x + 1; }
int main() { return f(1) + f(2) + f(3); }")
       (run-optimized "
int f(int x) { return x + 1; }
int main() { return f(1) + f(2) + f(3); }"))))

(deftest test-inline-void-function ()
  "Test: inlining void functions (side effects only)"
  (check
    ;; Void function modifying through pointer
    (= 100 (run-optimized "
inline void set_val(int *p, int v) { *p = v; }
int main() {
  int x;
  x = 0;
  set_val(&x, 100);
  return x;
}" :max-cycles 20000))))

(deftest test-phase11-inlining ()
  "Run Phase 11 function inlining tests"
  (combine-results
    (test-inline-explicit)
    (test-inline-auto)
    (test-inline-multiple-returns)
    (test-inline-recursive-not-inlined)
    (test-inline-with-locals)
    (test-inline-nested-calls)
    (test-inline-expression-context)
    (test-inline-preserves-behavior)
    (test-inline-void-function)))

(deftest test-c-compiler ()
  "Run all C compiler tests"
  (combine-results
    (test-phase1)
    (test-phase2)
    (test-phase3)
    (test-phase4)
    (test-phase5)
    (test-phase6)
    (test-phase7)
    (test-phase8-params)
    (test-phase9-pointers)
    (test-phase10-constant-folding)
    (test-phase11-inlining)))

(defun test-c-compiler-with-output (&optional (output-dir "/tmp/c-compiler-tests"))
  "Run all C compiler tests and save each test's output to a separate file.
   Creates the output directory if it doesn't exist.
   Returns t if all tests pass, nil otherwise."
  ;; Create output directory
  (ensure-directories-exist (format nil "~a/" output-dir))
  ;; Reset counter and enable saving
  (let ((*save-test-outputs* output-dir)
        (*test-output-counter* 0))
    (format t "~%Saving test outputs to: ~a~%" output-dir)
    (let ((result (test-c-compiler)))
      (format t "~%Saved ~a test outputs to ~a~%" *test-output-counter* output-dir)
      result)))

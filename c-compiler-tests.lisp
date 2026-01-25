;;; ===========================================================================
;;; =================== C Compiler Tests for liteRISC =========================
;;; ===========================================================================

(in-package :c-compiler)

;;; ===========================================================================
;;; Test Utilities
;;; ===========================================================================

(defun run-and-get-result (source &key (verbose nil) (max-cycles 10000))
  "Compile, run, and return the result in P0"
  (handler-case
      (run-c-program source :verbose verbose :max-cycles max-cycles)
    (error (e)
      (format t "Error: ~a~%" e)
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

(deftest test-c-compiler ()
  "Run all C compiler tests"
  (combine-results
    (test-phase1)
    (test-phase2)
    (test-phase3)
    (test-phase4)
    (test-phase5)
    (test-phase6)
    (test-phase7)))

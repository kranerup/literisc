(defpackage :unit
  (:use :cl :macro-utilities)
  (:export :deftest :check :report-results :combine-results :run-tests :list-tests :*all-tests*))
(in-package :unit)

(defvar *test-name* nil)
(defvar *all-tests* (make-hash-table))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other
test functions or use `check' to run individual test cases."
  `(progn
     (setf (gethash ',name *all-tests*) t)
     (defun ,name ,parameters
       (let ((*test-name* (append *test-name* (list ',name))))
         ,@body))))

(defmacro check (&body forms)
  "Run each expression in `forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating `forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by `check'."
  (format t "~:[FAIL~;pass~] ... ~a~%" result *test-name*)
  result)

(defun list-tests ()
  "Return a list of all defined test names."
  (sort (loop for test-name being the hash-keys of *all-tests*
              collect test-name)
        #'string< :key #'symbol-name))

(defun run-tests (&optional test-names)
  "Run tests.
   If TEST-NAMES is provided, run only the tests in the list.
   Otherwise, run all defined tests."
  (let ((tests-to-run (if test-names
                          (if (listp test-names) test-names (list test-names))
                          (loop for test-name being the hash-keys of *all-tests*
                                collect test-name))))
    (let ((pass-count 0)
          (fail-count 0))
      (dolist (test-name tests-to-run)
        (if (gethash test-name *all-tests*)
            (progn
              (format t "Running test: ~a~%" test-name)
              (if (handler-case (funcall test-name)
                    (error (e)
                      (format t "~%FAIL (error) ... ~a~%  ~a~%" test-name e)
                      nil))
                  (incf pass-count)
                  (incf fail-count)))
            (format t "Test not found: ~a~%" test-name)))
      (format t "~%Summary: ~a tests run, ~a passed, ~a failed.~%"
              (+ pass-count fail-count)
              pass-count
              fail-count)
      (= fail-count 0))))

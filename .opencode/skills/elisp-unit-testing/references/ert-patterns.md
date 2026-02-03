# ERT Patterns and Fixtures

## Test Selection

Run specific tests:
```bash
emacs -batch -l ert -l test.el -eval '(ert-run-tests-batch-and-exit "test-prefix")'
```

Run interactively: `M-x ert`, enter regex to filter.

## Fixtures Pattern

### Basic Fixture
```elisp
(defun my-test-fixture (body)
  "Setup and teardown for tests."
  (let ((original-var my-global-var))
    (unwind-protect
        (progn
          (setq my-global-var 'test-value)
          (funcall body))
      (setq my-global-var original-var))))

(ert-deftest test-using-fixture ()
  (my-test-fixture
    (lambda ()
      (should (equal my-global-var 'test-value)))))
```

### Buffer Fixture
```elisp
(defun buffer-fixture (body)
  (let ((buf (generate-new-buffer "*test-buffer*")))
    (unwind-protect
        (with-current-buffer buf
          (funcall body))
      (and buf (kill-buffer buf)))))
```

### File Fixture
```elisp
(defun file-fixture (body)
  (let ((file (make-temp-file "test-" nil ".el")))
    (unwind-protect
        (progn
          (write-region "test content" nil file)
          (funcall body file))
      (delete-file file))))
```

## Expected Failures

Mark tests expected to fail:
```elisp
(ert-deftest test-not-yet-implemented ()
  :expected-result :failed
  (should (feature 'not-yet-implemented)))
```

## Skipping Tests

Conditionally skip:
```elisp
(ert-deftest test-network-feature ()
  (skip-unless (featurep 'make-network-process))
  (should (do-network-thing)))
```

## Running Tests Programmatically

```elisp
;; Run all tests
(ert-run-tests-batch-and-exit)

;; Run specific suite
(ert-run-tests-batch-and-exit "my-prefix")

;; Get results programmatically
(let ((results (ert-run-tests "my-test")))
  (ert-stats-passed results))
```

## Debugging Failed Tests

When a test fails, use `ert` buffer:
- Press `.` on failure to jump to assertion
- Press `t` to re-run test
- Press `r` to re-run all tests

Use `ert-debugger` for stepping:
```elisp
(ert-deftest test-debug ()
  (should (debug-on-entry 'my-function))
  (my-function arg)
  (should (cancel-debug-on-entry 'my-function)))
```
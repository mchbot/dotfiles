---
name: elisp-unit-testing
description: Tests Emacs Lisp code with ERT or Buttercup. Use when writing, debugging, or refactoring Emacs Lisp, setting up test infrastructure, mocking Emacs state, or measuring code coverage.
license: GPLv3
compatibility: opencode
---

STARTER_CHARACTER = 🧪

# Emacs Lisp Unit Testing

## Framework Selection

**ERT** (built-in): Simple, direct, ideal for small projects. Use `(ert-deftest name () ...)` and `(should (equal actual expected))`.

**Buttercup** (external package): BDD-style with `describe`/`it`/`expect`. Better for larger projects, readable output, nested suites.

Choose one and stick with it. Mixing frameworks in same project is confusing.

## Test File Structure

Naming: `test/my-package-test.el` or `tests/test-my-package.el`. Match package prefix.

Header:
```elisp
(require 'my-package)
(when (require 'undercover nil t)
  (undercover "*.el" (:exclude ".dir-locals.el")))
```

Group tests logically by function or feature, not by test type.

## Test Organization

ERT:
```elisp
(ert-deftest my-package-function-basic ()
  "Test basic case"
  (should (equal (my-package-func 1) 2)))

(ert-deftest my-package-function-edge ()
  "Test edge case"
  (should-error (my-package-func nil)))
```

Buttercup:
```elisp
(describe "my-package-function"
  (it "handles basic input"
    (expect (my-package-func 1) :to-equal 2))
  (it "handles edge cases"
    (expect (my-package-func nil) :to-throw)))
```

Name tests descriptively: what they test, not that they test.

## Running Tests

Interactive in Emacs:
- ERT: `M-x ert RET t` (all), `M-x ert RET test-name` (specific)
- Buttercup: `M-x buttercup-run-discover`

Batch mode (CI):
```bash
# ERT
emacs -batch -L . -l ert -l test/my-package-test.el -f ert-run-tests-batch-and-exit

# Buttercup
emacs -batch -L . -f package-initialize -f buttercup-run-discover
```

## Test Isolation

Each test must start clean. Use `with-temp-buffer`, `let` binding, or `unwind-protect` for cleanup.

```elisp
(ert-deftest test-with-cleanup ()
  (let ((original-value my-var))
    (unwind-protect
        (progn
          (setq my-var 'test-value)
          (should (do-something)))
      (setq my-var original-value))))
```

Never depend on previous tests' state.

## Mocking Emacs State

Test pure functions directly. For functions touching Emacs state, isolate them:

**Buffers**: `with-temp-buffer`, `save-excursion`, `save-restriction`

**Windows**: `with-selected-window`, test behavior, not UI

**Global variables**: `let` bindings, `cl-letf` for stubbing

**Commands**: Bind `interactive` form or test at function level

```elisp
(ert-deftest test-buffer-modification ()
  (with-temp-buffer
    (insert "hello")
    (my-transform-function)
    (should (equal (buffer-string) "HELLO"))))
```

## Setup and Teardown

ERT fixtures:
```elisp
(defun my-fixture (body)
  (let ((test-buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (insert "setup data"))
          (funcall body))
      (kill-buffer test-buffer))))

(ert-deftest test-with-fixture ()
  (my-fixture (lambda () ...)))
```

Buttercup hooks:
```elisp
(describe "my-feature"
  (before-all (setq my-test-state 'init))
  (after-all (setq my-test-state nil))
  (before-each (setq my-test-state 'before))
  (after-each (setq my-test-state 'after)))
```

## Coverage

Use `undercover.el` for ERT, or `testcover` built-in.

**Undercover setup**:
```elisp
(when (require 'undercover nil t)
  (undercover "*.el" (:exclude "test/*.el" ".dir-locals.el")))
```

**Viewing coverage**:
- Local: `(undercover "*.el" (:report-format 'text))`
- CI: `(undercover "*.el" (:send-report t))`
- Visual: `coverage-mode` from MELPA

Emacs built-in `testcover`:
```elisp
(require 'testcover)
(testcover-start "my-package.el")
(run-tests)
(testcover-mark-all)  ; Highlights in buffer
```

## Anti-patterns

Don't test implementation details. Test behavior and outcomes.

Don't rely on external state (files, network). Mock or use temp fixtures.

Don't skip edge cases. Document why if truly impossible.

Don't mix concerns. One test = one assertion or closely related group.

Don't write fragile tests. Tests should pass regardless of unrelated code changes.

## Emacs-specific Considerations

Byte-compiled code behaves differently. Test both `.el` and `.elc` if needed.

Macros: Test expansion or use `edebug` instrumentation. Coverage limited for macro bodies.

Advice: Test with and without advice if package uses it.

Async operations: Use `run-at-time` or explicit wait/sync mechanisms.

## Documentation

Keep test comments lean. Good test names are self-documenting.

Document non-obvious setup or cleanup in docstring.

Add examples in docstrings for `describe-test` or similar if applicable.

References:
- ERT patterns and fixtures → See `references/ert-patterns.md`
- Buttercup advanced usage → See `references/buttercup-advanced.md`
- Mocking strategies → See `references/mocking-patterns.md`
- Coverage setup → See `references/coverage-setup.md`
- CI configuration → See `references/ci-configuration.md`

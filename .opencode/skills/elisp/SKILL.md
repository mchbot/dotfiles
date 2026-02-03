---
name: elisp hacker
description: Expert Emacs elisp programmer. Use when working on .el files or asked to write lisp for emacs or any of its variants such as spacemacs
license: GPLv3
compatibility: opencode
---

STARTER_CHARACTER = 🥸

# Guidelines for Writing Elisp Unit Tests

## Overview

When writing unit tests for Emacs Lisp code using the `ert` framework, follow these guidelines to ensure tests are reliable, isolated, and properly cleaned up.

## Basic Structure

```elisp
(require 'ert)
(require 'package-under-test)

(ert-deftest test-function-name ()
  "Brief description of what is being tested"
  ;; Test body
  )
```

## Running Tests

### Use a wrapper script

Save a script like this one as `test-runner.el`. It ensures that all the necessary dependencies are available when running the tests:

```elisp
;; Use this command line to run the tests:
;;
;; emacs --batch -l test-runner.el

;; Make all the spacemacs packages like ivy available
(let ((package-dir "/Users/mch/.emacs.d/elpa/30.1/develop/"))
  (dolist (project (directory-files package-dir t directory-files-no-dot-files-regexp))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

(load "package")               ; Ensure package.el is loaded
(package-initialize)           ; Load installed packages

(add-to-list 'load-path (expand-file-name "~/config/"))

;; Run the tests
(load "tests.el")
(ert-run-tests-batch-and-exit)
```

Run tests from command line:
```bash
emacs --batch -l test-runner.el
```

If the command `emacs` is not found, stop and ask the user to ensure emacs is installed and to create a softlink if necessary. Show this command as an example:
```shell
ln -s /Applications/Emacs.app/Contents/MacOS/Emacs ~/.local/emacs
```

## Process for Adding a Unit Test

When adding a new unit test, follow this process:

1. **Run existing tests to ensure baseline**
   ```bash
   emacs --batch -l test-runner.el
   ```

2. **Add the new test** to `tests.el` following the guidelines above

3. **Run tests again** to verify the new test passes

4. **Fix any errors** until all tests pass

5. **Commit the changes** (if requested)


## Key Practices

### Use `unwind-protect` for Cleanup

Always wrap test setup and execution in `unwind-protect` to ensure cleanup runs even if the test fails.

```elisp
(let ((temp-file (make-temp-file "test-")))
  (unwind-protect
      ;; Test execution
      (progn
        ...)
    ;; Cleanup (always runs)
    (when (file-exists-p temp-file)
      (delete-file temp-file))))
```

### Temporary Files and Directories

Use `make-temp-file` to create temporary resources:
- `make-temp-file "prefix"` → creates a file
- `make-temp-file "prefix" t)` → creates a directory

```elisp
(let ((temp-dir (make-temp-file "test-" t))
      (temp-file (concat temp-dir "/test.org")))
  (unwind-protect
      (progn
        (with-temp-file temp-file
          (insert "test content"))
        ...)
    (delete-directory temp-dir t)))  ; 't' = recursive delete
```

### Testing Functions That Switch Buffers or Windows

Functions that call `find-file`, `switch-to-buffer`, or other window/buffer operations can cause issues in batch mode. Use `save-window-excursion` to isolate side effects.

```elisp
(save-window-excursion
  (function-that-opens-buffer arg)
  (should (get-buffer expected-buffer-name)))
```

### Check Buffer Existence, Not Names

When testing if a buffer was created with a specific name, use `get-buffer` rather than checking `buffer-name` (which returns the current buffer's name).

```elisp
(should (get-buffer "🦉 Expected Title"))
```

### Buffer Cleanup

Kill any buffers created during tests in the cleanup section.

```elisp
(let ((buf (get-buffer "buffer-name")))
  (when buf
    (kill-buffer buf)))
```

### File Path Patterns

When testing functions that expect specific file patterns (e.g., UUID filenames), construct temp files accordingly:

```elisp
(let* ((uuid "12345678-1234-1234-1234-123456789abc")
       (temp-file (concat temp-dir "/" uuid ".org")))
  ...)
```

### Write Temp Files with Content

Use `with-temp-file` to create files with specific content for testing:

```elisp
(with-temp-file temp-file
  (insert "* Heading")
  (insert "\nSome content"))
```

## Common Pitfalls

| Issue | Solution |
|-------|----------|
| `void-function` errors | Add `require` statements for missing packages |
| Tests fail in batch mode | Use `save-window-excursion` for window/buffer ops |
| Temp files not cleaned up | Use `unwind-protect` with explicit cleanup |
| Buffer name checks fail | Use `get-buffer` instead of `buffer-name` |
| UUID pattern matching fails | Create temp files with UUID-like names |


# Coverage Setup for Emacs Lisp

## Undercover.el Setup

### Basic Setup

Add to your Cask file:

```elisp
(source gnu)
(source melpa)

(package-file "my-package.el")

(development
  (depends-on "undercover"))
```

In your test helper (`test/test-helper.el`):

```elisp
(when (require 'undercover nil t)
  (undercover "*.el" (:exclude "test/*.el")))

(require 'my-package)
```

### Configuration Options

```elisp
;; Exclude specific files
(undercover "*.el"
            (:exclude "test/*.el"
                      "my-package-examples.el"
                      ".dir-locals.el"))

;; With report file
(undercover "*.el"
            (:report-file "coverage/coverage.json")
            (:send-report nil))

;; Report format options
(undercover "*.el" (:report-format 'lcov))      ; Default
(undercover "*.el" (:report-format 'simplecov))
(undercover "*.el" (:report-format 'codecov))
(undercover "*.el" (:report-format 'coveralls))
(undercover "*.el" (:report-format 'text))     ; Human readable
```

### Running with Cask

```bash
# Without coverage
cask exec ert-runner

# With coverage (local)
UNDERCOVER_FORCE=true cask exec ert-runner

# With coverage (CI - auto-detects)
cask exec ert-runner
```

### Environment Configuration

```bash
# Force coverage locally
UNDERCOVER_FORCE=true cask exec ert-runner

# Configure via environment
UNDERCOVER_CONFIG='("*.el" (:exclude "test/*.el"))' cask exec ert-runner
```

### Integration with CI Services

**Coveralls**:
```elisp
(undercover "*.el" (:report-format 'lcov))
```

**Codecov**:
```elisp
(undercover "*.el"
            (:report-format 'codecov)
            (:send-report nil))
```
Then in CI:
```bash
bash <(curl -s https://codecov.io/bash)
```

## Testcover (Built-in)

### Basic Usage

```elisp
(require 'testcover)

;; Instrument code
(testcover-start "my-package.el")

;; Run your tests
(my-test-function)

;; Mark coverage in buffer
(testcover-mark-all)  ; Requires open buffer

;; Get next mark
(testcover-next-mark)
```

### In Batch Mode

```bash
emacs -batch \
  -l testcover \
  -l my-package.el \
  -l test-my-package.el \
  -eval '(testcover-start "my-package.el")' \
  -eval '(ert-run-tests-batch-and-exit)' \
  -eval '(testcover-mark-all)'
```

### Understanding Highlights

- **Red**: Form never completely evaluated
- **Brown**: Form always evaluated to same value (insufficient variation testing)

### Advising Coverage

For code that should always return same value:

```elisp
(defun my-constant-value (x)
  (1value x))  ; Tell testcover this is intentional
```

For code that should never return:

```elisp
(defun my-error-function (x)
  (noreturn (error "Must not return")))
```

## Visual Coverage

### Coverage-mode

```elisp
;; Install from MELPA
(package-install 'coverage-mode)

;; Generate coverage report with undercover
(undercover "*.el"
            (:report-format 'simplecov)
            (:send-report nil))

;; After tests, open source file
;; M-x coverage-mode
```

This shows color-coded coverage overlay:
- Red lines: Not covered
- Green lines: Covered

### Coverlay.el (LCOV Overlay)

```bash
;; Install
M-x package-install coverlay

;; Generate LCOV file
undercover "*.el" (:report-file "coverage/lcov.info")

;; Load in Emacs
M-x global-coverlay-mode
```

## Coverage Thresholds

No built-in enforcement, but can check in scripts:

```bash
#!/bin/bash
# run-with-coverage.sh

UNDERCOVER_FORCE=true cask exec ert-runner 2>&1 | tee coverage-output.txt

# Parse and check threshold (example implementation)
# Actual parsing depends on your framework output
if grep -q "Coverage.*%" coverage-output.txt; then
  echo "Coverage report generated"
fi
```

Or using `coveralls` JSON output:

```elisp
;; In test runner
(when (and (boundp 'UNDERCOVER_FORCE) UNDERCOVER_FORCE)
  (let ((coverage (undercover--coverage-report)))
    (when (< (plist-get coverage :percentage) 80)
      (warn "Coverage below 80%%"))))
```

## Best Practices

### File Organization

```
my-package/
├── Cask
├── my-package.el
├── test/
│   ├── test-helper.el    ; Load undercover here
│   └── test-my-package.el
└── coverage/             ; For local reports
    └── .gitkeep
```

### Exclusions

Always exclude:
- Test files (`test/*.el`)
- Example files (`*-examples.el`, `demo.el`)
- Auto-generated files
- Files that don't need coverage

### Continuous Integration

Always measure coverage in CI. Use services like:
- Coveralls (for GitHub, Travis)
- Codecov (supports more platforms)
- Self-hosted with LCOV reports

### Pre-commit Hooks

```bash
#!/bin/bash
# .git/hooks/pre-commit
# Quick coverage check before committing

if git diff --name-only HEAD | grep -q '\.el$'; then
  UNDERCOVER_FORCE=true cask exec ert-runner || exit 1
fi
```

## Troubleshooting

### No Coverage Information

Symptoms: Undercover reports "No coverage information"

Causes:
1. Byte-compiled files present
2. Package loaded before undercover
3. Test runner loads with `-l` instead of `-L`

Solutions:
```bash
# Remove byte-compiled files
find . -name "*.elc" -delete

# Ensure test runner uses -L for load path
# Bad: cask exec ert-runner -l my-package.el
# Good: cask exec ert-runner
```

### Macro Coverage

Macros have limited coverage. Macro body may not be covered.

Solution: Use `def-edebug-spec` for better coverage:

```elisp
(def-edebug-spec my-macro (&rest form))
```

This tells edebug how to instrument your macro.

### Circular Objects

Undercover doesn't support circular objects.

Symptoms: Error about circular references

Solution: Avoid circular structures in test data, or mark code as excluded.

### Slow Coverage

Coverage measurement with edebug is slower than normal execution.

Solutions:
- Only measure coverage in CI or before commits
- Use local reports instead of always sending to service
- Exclude slow modules from coverage

### Performance on Large Projects

For large packages, consider:
- Splitting into smaller modules
- Testing modules separately
- Incremental coverage measurement

### Debug Mode

Enable debug output:

```elisp
(setq undercover-verbose t)
(undercover "*.el")
```

This shows which files are being instrumented and why.
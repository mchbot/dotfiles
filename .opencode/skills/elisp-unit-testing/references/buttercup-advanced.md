# Buttercup Advanced Usage

## Nested Suites

Organize tests hierarchically:

```elisp
(describe "my-package"
  (describe "string functions"
    (describe "uppercase"
      (it "handles ascii"
        (expect (my-upper "hello") :to-equal "HELLO"))
      (it "handles unicode"
        (expect (my-upper "résumé") :to-equal "RÉSUMÉ")))

  (describe "math functions"
    (it "adds numbers"
      (expect (my-add 2 3) :to-equal 5))))
```

## Matchers

### Equality
```elisp
(expect value :to-equal expected)
(expect value :not :to-equal expected)
```

### Type
```elisp
(expect value :to-be-truthy)
(expect value :to-be-falsy)
(expect value :to-be-nil)
```

### Numbers
```elisp
(expect 5 :to-be-greater-than 3)
(expect 5 :to-be-less-than 10)
(expect 5.2 :to-be-close-to 5.0 0.2)
```

### Strings
```elisp
(expect "hello" :to-match "hel+")
(expect "hello" :to-match-regex "h\\w+")
```

### Errors
```elisp
(expect (my-func arg) :to-throw)
(expect (my-func arg) :to-throw 'error-type)
(expect (my-func arg) :to-throw 'error-type "message regex")
```

## Custom Matchers

```elisp
(buttercup-define-matcher :to-have-length (expected-length)
  (lambda (actual)
    (equal (length actual) expected-length)))

;; Usage
(expect (list 1 2 3) :to-have-length 3)
```

## Setup/Teardown Hooks

```elisp
(describe "with hooks"
  (before-all (message "Run once before suite"))
  (after-all (message "Run once after suite"))
  (before-each (message "Run before each test"))
  (after-each (message "Run after each test"))

  (it "has setup run"
    (expect t :to-be-truthy)))
```

## Assumptions

Skip tests conditionally:

```elisp
(describe "conditional tests"
  (it "requires network"
    (assume (and (featurep 'make-network-process) (ignore-errors (network-access-p)))
            "Network not available")
    (expect (http-get "https://example.com") :to-be-truthy)))
```

## Async Testing

```elisp
(describe "async operations"
  (it "waits for callback"
    (let ((done nil))
      (async-func (lambda (result)
                    (setq done result)))
      ;; Wait for done
      (while (not done)
        (sit-for 0.1))
      (expect done :to-be-truthy))))
```

## Filtering Tests

Run specific suites from command line:
```bash
emacs -batch -f buttercup-run-discover --suite "my-package"
```

Run by name:
```bash
emacs -batch -f buttercup-run-discover --name "should handle"
```
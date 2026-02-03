# Mocking Strategies for Emacs Lisp

## Stubbing Functions

Use `cl-letf` to temporarily override functions:

```elisp
(ert-deftest test-with-stub ()
  (cl-letf (((symbol-function 'external-call)
             (lambda (&rest args)
               'mocked-result)))
    (should (equal (my-function) 'expected))))

;; Stub specific arguments
(ert-deftest test-stub-with-args ()
  (cl-letf (((symbol-function 'external-func)
             (lambda (arg)
               (equal arg "expected"))))
    (should (my-function))))
```

## Mocking Variables

```elisp
(ert-deftest test-let-binding ()
  (let ((my-package-config 'test-config))
    (should (equal (my-get-config) 'test-config))))

;; Preserve original value
(ert-deftest test-buffer-local-binding ()
  (with-temp-buffer
    (setq-local my-var 'local-value)
    (should (equal my-var 'local-value))))
```

## Mocking User Input

```elisp
(ert-deftest test-with-mock-input ()
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _)
               "mock-input")))
    (should (equal (my-prompt-function) "processed-mock-input"))))
```

## Mocking External State

### Network
```elisp
(ert-deftest test-without-network ()
  (skip-unless nil)  ; Skip when network unavailable
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (url)
               (with-current-buffer (generate-new-buffer "*mock*")
                 (insert "mock-response")
                 (current-buffer)))))
    (should (equal (my-fetch "url") "parsed-mock-response"))))
```

### File System
```elisp
(ert-deftest test-file-operations ()
  (let ((temp-dir (make-temp-file "test-" t)))
    (unwind-protect
        (progn
          (write-region "content" nil (expand-file-name "test.txt" temp-dir))
          (should (file-exists-p (expand-file-name "test.txt" temp-dir))))
      (delete-directory temp-dir t))))
```

## Mocking Emacs State

### Buffers
```elisp
(ert-deftest test-buffer-state ()
  (with-temp-buffer
    (insert "initial")
    (my-modify-function)
    (should (equal (buffer-string) "modified"))))
```

### Windows
```elisp
(ert-deftest test-window-focus ()
  (let ((orig-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (split-window))
          (should (my-window-focused-function)))
      (select-window orig-window))))
```

### Point/Mark
```elisp
(ert-deftest test-point-movement ()
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (should (equal (line-number-at-pos) 2)))))
```

## Mocking Major/Minor Modes

```elisp
(ert-deftest test-mode-hook ()
  (let ((my-hook-called nil))
    (add-hook 'my-mode-hook (lambda () (setq my-hook-called t)))
    (my-mode)
    (should my-hook-called)))

;; With cleanup
(ert-deftest test-mode-with-cleanup ()
  (let ((my-hook-called nil))
    (unwind-protect
        (progn
          (add-hook 'my-mode-hook (lambda () (setq my-hook-called t)))
          (my-mode))
      (remove-hook 'my-mode-hook (lambda () (setq my-hook-called t))))))
```

## Mocking Advice

```elisp
(ert-deftest test-with-advice ()
  (defun my-advice (orig-func &rest args)
    'advised-result)

  (unwind-protect
      (progn
        (advice-add 'my-function :around #'my-advice)
        (should (equal (my-function) 'advised-result)))
    (advice-remove 'my-function #'my-advice)))
```

## Mocking Time

```elisp
(ert-deftest test-timed-operation ()
  (cl-letf (((symbol-function 'current-time)
             (lambda ()
               (encode-time 0 0 0 1 1 2020))))
    (should (equal (my-timestamp-func) "2020-01-01"))))
```

## Spy Pattern

Track function calls:

```elisp
(ert-deftest test-spy-calls ()
  (let ((calls '()))
    (cl-letf (((symbol-function 'external-func)
               (lambda (&rest args)
                 (push args calls))))
      (my-function)
      (should (equal calls '(("arg1" "arg2")))))))
```
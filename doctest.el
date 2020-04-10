;;; doctest.el --- Doctests for Emacs Lisp -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: Apr 8 2020
;; Keywords: lisp maint docs help
;; URL: https://github.com/riscy/doctest
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.0.0

;;; Commentary:

;; These are like a Python "doctest", but for Emacs Lisp and with an Emacs
;; twist. A doctest is a test written inside a docstring that looks like:
;; >> (+ 1 1)
;; => 2
;;
;; There are benefits:
;; - It's a clean way to test elisp code without any heavy dependencies
;; - It encourages functions that are pure or at least side-effect-free
;; - Your unit tests turn into documentation that your users can read!
;;
;; Type M-x doctest RET to run doctests on an entire buffer.
;; Type M-x doctest-here RET to run the doctest on the current line.
;; Type M-x customize-group RET doctest RET to see customizables.

;;; Code:

(defvar doctest-regexp ">> \\(.*\\)")
(defvar doctest-target "=> ")
(defvar doctest-first-failure nil)
(defvar doctest-num-tests 0)
(defvar doctest-text nil)

(defun doctest (&optional filename)
  "Run doctest on current buffer, or FILENAME if given.
When run interactively, the point will move to the site of the
first test failure (or the first syntax error in a test).

Here's a living example:
>> (cons (list 6 'quoted :symbol 12345 \"Here's a string\") 8310247)
=> ((6 quoted :symbol 12345 \"Here's a string\") . 8310247)"
  (interactive)
  (doctest--reset-state)
  (let ((filename (or filename (buffer-file-name (current-buffer)))))
    (set-buffer (find-file filename))
    (goto-char (point-min))
    (while (ignore-errors (goto-char (doctest--next-test)))
      (doctest-here)
      (setq doctest-num-tests (1+ doctest-num-tests)))
    (if (not doctest-first-failure)
        (message "%s test(s) passed!" doctest-num-tests)
      (goto-char doctest-first-failure)
      (message "%s" doctest-text))))

(defun doctest-here (&optional interactively)
  "Run the test that the point is currently on.
If called INTERACTIVELY, let the user know the test passed and
move the point down two lines (possibly onto the next test)."
  (interactive "p")
  (if (not (looking-at doctest-regexp))
      (message "No doctest here.")
    (let* ((sexp (doctest-unescape (match-string-no-properties 1)))
           (actual-value (eval (car (read-from-string sexp))))
           (target-value (progn (forward-line 1) (doctest--parse-target-value))))
      ;; normalize values to their prin1 representations:
      (setq actual-value (format "%S" actual-value)
            target-value (format "%S" target-value))
      (if interactively
          (doctest--here-interactively sexp actual-value target-value)
        (doctest--here-noninteractively sexp actual-value target-value)))))

(defun doctest--here-interactively (sexp actual-value target-value)
  "Compare ACTUAL-VALUE (generated by SEXP) to TARGET-VALUE.
Let the user know the test passed and move to the next line."
  (if (not (string= actual-value target-value))
      (message "%s => %s but got %s" sexp target-value actual-value)
    (forward-line 1)
    (message "Pass!")))

(defun doctest--here-noninteractively (sexp actual-value target-value)
  "Compare ACTUAL-VALUE (generated by SEXP) to TARGET-VALUE.
Call `doctest--append' to append to the running test output."
  (unless (string= actual-value target-value)
    (setq doctest-first-failure (or doctest-first-failure (point)))
    (doctest--append (format "%s.el#%s: %s => %s but got %s"
                             (file-name-base) (line-number-at-pos)
                             sexp target-value actual-value))))

(defun doctest--reset-state ()
  "Reset doctest's current state."
  (eval-buffer)
  (setq doctest-text nil
        doctest-num-tests 0
        doctest-first-failure nil))

(defun doctest--next-test ()
  "Return the point where the next test begins -- else nil.
>> (eq (doctest--next-test) (point))
=> t"
  (declare (side-effect-free t))
  (let (doctest-point)
    (save-excursion
      (while (and (not doctest-point)
                  (re-search-forward doctest-regexp nil t))
        (and (nth 3 (syntax-ppss))       ; in a string
             (zerop (forward-line 1))    ; ...with a next line
             (nth 3 (syntax-ppss))       ; ...also in a string
             (looking-at doctest-target) ; ...with a target result
             (zerop (forward-line -1))
             (setq doctest-point (point)))))
    doctest-point))

(defun doctest--append (str)
  "Append STR to `doctest-text' with a newline if necessary."
  (setq doctest-text (concat doctest-text (unless doctest-text "\n") str)))

(defun doctest--parse-target-value ()
  "Read the value on the current line as an sexp.
Point must be inside a string, or else we return nil."
  (goto-char (point-at-eol))
  (while (not (or (nth 3 (syntax-ppss)) (bolp)))
    (backward-char 1)) ; back up into the string
  (car
   (read-from-string
    (doctest-unescape
     (buffer-substring (+ (point-at-bol) (length doctest-target)) (point))))))

(defun doctest-unescape (str)
  "Remove all backslashes from STR.
It's open work to parse/handle backslashes cleanly, so ignore them.
>> (doctest-unescape \"back\\\\slash\")
=> \"backslash\""
  (declare (side-effect-free t) (pure t))
  (replace-regexp-in-string "\\\\" "" str))

(provide 'doctest)
;;; doctest.el ends here
;;; crucibler-mode.el --- A major mode for writing Crucible CFGs directly  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Galois, Inc.
;; Portions derived from lisp-mode.el, part of GNU Emacs 26.1, copyright (C) 1985-1986, 1999-2018 Free Software Foundation, Inc.

;; Author: David Thrane Christiansen <dtc@galois.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for working with Crucible CFGs.  It's extremely
;; basic, and will likely not become more fancy over time.

;;; Code:

(defconst crucibler--type-constructors
  (list "Any" "Unit" "Bool" "Nat" "Real" "Integer" "ComplexReal" "Char" "String"
        "Vector" "BitVector" "->" "Maybe" "Variant" "Ref"))

(defconst crucibler--operators
  (list "+" "/" "-" "*" "<" "show" "just" "nothing" "from-just" "equal?" "integer?" "vector" "vector-replicate"
        "vector-size" "vector-get" "vector-set" "vector-cons" "pack" "not" "and" "or" "xor" "mod" "string-append"
        "deref" "ref" "empty-ref" "inj"))

(defconst crucibler--misc-keywords
  (list "defun" "defblock" "defglobal" "start" "the" "if" "funcall" "registers"))

(defconst crucibler--statements
  (list "jump" "return" "branch" "let" "set-global!" "maybe-branch" "tail-call" "error" "output" "print" "set-register!" "case" "set-ref!" "drop-ref!" "assert!"))

(defconst crucibler--ident-regexp "[a-zA-Z<>=+*/!_\\\\?-][a-zA-Z0-9<>=+*/!_\\\\?-]*")

(defun crucibler-indent-method (sym)
  "Calculate how to indent a crucibler form headed by the symbol SYM."
  (cl-case sym
    ((defun) 3)
    ((defblock start the branch maybe-branch let set-register! case) 1)
    ((registers) 'defun)
    (t nil)))


;; This function contains code derived from lisp-mode.el, part of GNU
;; Emacs 26.1, copyright (C) 1985-1986, 1999-2018 Free Software Foundation,
;; Inc.
(defun crucibler-indent-function (indent-point state)
  "The variable `lisp-indent-function' is this function in `crucibler-mode'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
	      (progn (goto-char calculate-lisp-indent-last-sexp)
		     (beginning-of-line)
		     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
	  ;; Indent under the list or under the first sexp on the same
	  ;; line as calculate-lisp-indent-last-sexp.  Note that first
	  ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (crucibler-indent-method (intern-soft function)))
	(cond ((eq method 'defun)
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
	       (funcall method indent-point state)))))))

(let ((crucibler-words (append crucibler--type-constructors crucibler--operators crucibler--misc-keywords crucibler--statements)))
  (defun crucibler-completion-at-point ()
    "Complete for Crucible CFGs."
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (list (car bounds)
              (cdr bounds)
              crucibler-words
              :exclusive 'no)))))

(defvar crucibler-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; Identifier chars
    (cl-loop for ch across "<>=+*/!_\\?-"
             do (modify-syntax-entry ch "_" table))
    ;; Special ident chars
    (cl-loop for ch across "@:$#"
             do (modify-syntax-entry ch "_" table))
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\; "<" table)
    table))

(defun crucibler-mode-setup ()
  "Set the values of various necessary variables."
  (setq-local comment-start ";; ")
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local indent-region-function 'lisp-indent-region)
  (setq-local comment-indent-function #'lisp-comment-indent)
  (setq-local lisp-indent-function 'crucibler-indent-function)
  (setq-local completion-at-point-functions '(crucibler-completion-at-point))

  (setq font-lock-defaults
        `('(
            (,(regexp-opt crucibler--statements 'symbols)
             (0 font-lock-builtin-face))
            (,(regexp-opt crucibler--misc-keywords 'symbols)
             (0 font-lock-keyword-face))
            (,(regexp-opt crucibler--operators 'symbols)
             (0 font-lock-reference-face))
            (,(regexp-opt crucibler--type-constructors 'symbols)
             (0 font-lock-type-face))
            ("[+-]?\\(0x\\|0\\)?[1-9][0-9]*\\(/\\(0x\\|0\\)?[1-9][0-9]*\\)?\\_>"
             (0 font-lock-constant-face))
            (,(concat "\\_<#[tTfT]\\_>")
             (0 font-lock-constant-face))
            (,(concat "\\_<@" crucibler--ident-regexp "\\_>")
             (0 font-lock-function-name-face))
            (,(concat "\\_<$" crucibler--ident-regexp "\\_>")
             (0 font-lock-reference-face))
            (,(concat "\\_<" crucibler--ident-regexp ":\\_>")
             (0 font-lock-function-name-face))))))

(define-derived-mode crucibler-mode prog-mode
  "Crucible"
  "A major mode for editing Crucible test CFGs"
  :syntax-table crucibler-mode-syntax-table
  (crucibler-mode-setup))

;;;###autoload
(push '("\\.cbl$" . crucibler-mode) auto-mode-alist)

(provide 'crucibler-mode)
;;; crucibler.el ends here

;;; twee-chapbook-mode.el --- Major mode for Twee/Twine files in
;;; the Chapbook story format

;; Copyright (C) 2019 Marc-André Goyette
;; Author: Marc-André Goyette <goyette.marcandre@gmail.com>
;; URL: https://github.com/magoyette/twee-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "26"))
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; TODO

;;; Code:

(defconst twee-chapbook-mode--syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?' "." table)

    (modify-syntax-entry ?\" "." table)

    table))

(defconst twee-chapbook-mode--keywords
  '("TODO"))

(defun twee-chapbook-mode--completion-at-point ()
  "Completion function for Twee files for the Chapbook story format."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            twee-chapbook-mode--keywords))))

;; font-lock-warning-face: error or warning
;; font-lock-function-name-face: name of a function
;; font-lock-variable-name-face: name of a variable
;; font-lock-keyword-face: keywords like for or if
;; font-lock-comment-face: comments
;; font-lock-type-face: user-defined data types
;; font-lock-constant-face: names of constants, like ‘NULL’ in C.
;; font-lock-builtin-face: names of built-in functions.
;; font-lock-string-face: for string constants.
;; font-lock-negation-char-face: for easily-overlooked negation characters.
(defvar twee-chapbook-mode--font-lock-keywords
  `(
    ;;;;; Passages

    ("^::[[:space:]]*\\(.*\\)$" 1 font-lock-type-face)

    ;;;;; Modifiers

    ;; Font lock rules for Modifiers must be applied before Links

    ;; Modifier without parameters
    ("^[[]\\([^][\s]+\\)[]]$" 1 font-lock-function-name-face)

    ;; Modifier with parameters
    ("^[[]\\([^][\s]+\\)\s+\\([^][]+\\)[]]$"
     (1 font-lock-function-name-face t)
     (2 font-lock-variable-name-face t))

    ;;;;; Links

    ;; LinkText->PassageNameOrUrl
    ("[[][[]\\([^]]*?\\)\\(->\\||\\)\\([^]]*\\)[]][]]"
     (1 font-lock-string-face t)
     (3 font-lock-type-face t))

    ;; PassageNameOrUrl<-LinkText
    ("[[][[]\\([^]]*?\\)<-\\([^]]*\\)[]][]]"
     (1 font-lock-type-face t)
     (2 font-lock-string-face))

    ;; LinkTextIsEqualToPassageName [Must be last in priority]
    ("[[][[]\\([^]]*\\)[]][]]" 1 font-lock-type-face)

    ;;;;; Variables

    ;; Variable statement
    ("^\\([$_A-Za-z][$_A-Za-z0-9.]*\\):\s" 1 font-lock-variable-name-face)

    ;; String in variable statement
    ;; Regex for strings with support for quoting with \ are quite horrible
    ;; The 't' at the end is necessary to support properly strings like 'A string "value"'
    ("^[$_A-Za-z][$_A-Za-z0-9.]*:.*?\\(\"\\(\\(?:[^\"\\]+\\|\\\\\\(?:.\\|\\)\\)*\\)\"\\)" 1 font-lock-string-face t)
    ("^[$_A-Za-z][$_A-Za-z0-9.]*:.*?\\('\\(\\(?:[^'\\]+\\|\\\\\\(?:.\\|\\)\\)*\\)'\\)" 1 font-lock-string-face t)

    ;; Reference to a variable inside a string in the context of a variable statement
    ;; The 't' at the end allows to apply the syntax highlight on top of the string syntax highlight
    ("^[$_A-Za-z][$_A-Za-z0-9.]*:.*?\\({[$_A-Za-z][$_A-Za-z0-9.]*}\\)" 1 font-lock-variable-name-face t)

    ;; Reference to a variable
    ("\\({[$_A-Za-z][$_A-Za-z0-9.]*}\\)" 1 font-lock-variable-name-face)

    ;; Variables in inserts
    ("{[^}:]*?\\([$_A-Za-z][$_A-Za-z0-9]*\\):\s[^}]*}" 1 font-lock-variable-name-face)

    ("{\\([^}]*\\)}" 1 font-lock-comment-face)
    ))

(defalias 'twee-chapbook-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode twee-chapbook-mode
  twee-chapbook-parent-mode "Twee-Chapbook"

  (set-syntax-table twee-chapbook-mode--syntax-table)

  (set (make-local-variable 'font-lock-defaults)
       '(twee-chapbook-mode--font-lock-keywords nil nil))

  (setq-local completion-ignore-case t)

  (set (make-local-variable 'imenu-generic-expression)
       '(("Links" "[[][[]\\([^]]*\\)[]][]]" 1)
         ("Passages" "^::[[:space:]]*\\(.*\\)$" 1)))

  (add-to-list 'completion-at-point-functions
               'twee-chapbook--completion-at-point))

(provide 'twee-chapbook-mode)
;;; twee-chapbook-mode.el ends here

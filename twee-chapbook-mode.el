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

(require 'font-lock)

(defconst twee-chapbook-mode--syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?' "." table)

    (modify-syntax-entry ?\" "." table)

    table))

(defconst twee-chapbook-mode--keywords
  '(
    ;; Header and Footer
    "config.footer.left"
    "config.footer.center"
    "config.footer.right"
    "config.header.left"
    "config.header.center"
    "config.header.right"
    ;; Web Fonts
    "config.style.googleFont"
    "config.style.typekitFont"
    "config.style.fonts.<font-name>.url"
    "config.style.fonts.<font-name>.name"
    ;; Text Style
    "config.style.header.font"
    "config.style.header.link.font"
    "config.style.header.link.active.font"
    "config.style.page.font"
    "config.style.page.link.font"
    "config.style.page.link.active.font"
    ;; Colors
    "config.style.backdrop"
    "config.style.page.color"
    "config.style.page.link.color"
    "config.style.page.link.lineColor"
    "config.style.page.link.active.color"
    "config.style.page.link.active.lineColor"
    "config.style.page.color"
    "config.style.page.link.color"
    "config.style.page.link.lineColor"
    "config.style.page.link.active.color"
    "config.style.page.link.active.lineColor"
    ;; Page Style
    "config.style.pageStyle"
    "config.body.transition.name"
    "config.body.transition.duration"
    "config.header.transition.name"
    "config.header.transition.duration"
    "config.footer.transition.name"
    "config.footer.transition.duration"
    ;; Forks
    "config.style.page.fork.divider.style"
    "config.style.page.fork.divider.size"
    "config.style.page.fork.divider.color"
))

(defun twee-chapbook-mode--completion-at-point ()
  "Completion function for Twee files for the Chapbook story format."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            twee-chapbook-mode--keywords))))

(defvar twee-chapbook-mode-passage-header-face 'twee-chapbook-mode-passage-header-face
  "Face for Markdown headers in an OpenAPI YAML file.")

(defface twee-chapbook-mode-passage-header-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Face for passage headers in a Twee Chapbook file."
  :group 'faces)

(defconst twee-chapbook-mode--variable-init-regex
  "^\\([$_A-Za-z][$_A-Za-z0-9.]*\\)\\(?::\\|\s(.*?):\\)\s")

(defconst twee-chapbook-mode--parameter-name-regex
  "\s+\\([$_A-Za-z][$_A-Za-z0-9.]*\\):\s")

(defun twee-chapbook-mode--build-string-regex (delimiter)
  "Build a regex that match a string for the provided DELIMITER."
  (format "\\(%s\\(?:[^%s\\]+\\|\\\\\\(?:.\\|\\)\\)*%s\\)"
          delimiter delimiter delimiter))

(defun twee-chapbook-mode--build-string-value-regex (delimiter)
  "Build a regex for a string value within DELIMITER."
  (format "%s%s"
          twee-chapbook-mode--parameter-name-regex
          (twee-chapbook-mode--build-string-regex delimiter)))

(defun twee-chapbook-mode--build-string-constant-regex (delimiter)
  "Build a regex for a string value within DELIMITER."
  (format "\\(,\s+\\|\\[\\|==\s+\\|if\\(?:always\\|never\\)?\s+\\|unless\s+\\|\\(?:link\\|menu\\|input\\)\sfor\s\\)%s"
          (twee-chapbook-mode--build-string-regex delimiter)))

(defun twee-chapbook-mode--build-variable-string-value-regex (delimiter)
  "Build a regex for a string value within DELIMITER."
  (format "%s%s"
          twee-chapbook-mode--variable-init-regex
          (twee-chapbook-mode--build-string-regex delimiter)))

(defun twee-chapbook-mode--build-additional-parameter-name-regex ()
  "Build a regex for a string value"
  (format ",%s"
          twee-chapbook-mode--parameter-name-regex))

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

    ("^::[[:space:]]*\\(.*\\)$" 1 twee-chapbook-mode-passage-header-face)

    ;;;;; Modifiers

    ;; Font lock rules for Modifiers must be applied before Links

    ;; Modifier without parameters
    ("^[[]\\([^][\s]+\\)\\(\s+[^][]+\\)?[]]$" 1 font-lock-function-name-face)

    ;; Modifier without parameters
    ("^[[]\\([$_A-Za-z][$_A-Za-z0-9.]*\\)\s+\\([$_A-Za-z0-9.\s]+\\)[]]$" 2 font-lock-function-name-face)

    ;; Booleans modifiers
    ("^[[]\\(?:if\\(?:always\\|never\\)?\\|unless\\)\s+!?\\([$_A-Za-z][$_A-Za-z0-9.]*\\)\\([]]$\\|;\\)" 1 font-lock-variable-name-face t)

    ("\\([!=]==\\|>=?\\|<=?\\)\s+\\([$_A-Za-z][$_A-Za-z0-9.]*\\)" 2 font-lock-variable-name-face t)
    ("\\([$_A-Za-z][$_A-Za-z0-9.]*\\)\s+\\([!=]==\\|>=?\\|<=?\\)" 1 font-lock-variable-name-face t)

    ;; Additional modifier after a ;
    ("^[[][^;]*;\s+\\([$_A-Za-z0-9.]+\\).*?[]]$" 1 font-lock-function-name-face t)

    ;;;;; Links

    ;; LinkText->PassageNameOrUrl
    ;; or Twine 1's LinkText|PassageNameOrUrl
    ("[[][[]\\([^]]*?\\)\\(->\\||\\)\\([^]]*\\)[]][]]"
     (1 font-lock-string-face t)
     (3 font-lock-type-face t))

    ;; PassageNameOrUrl<-LinkText
    ("[[][[]\\([^]]*?\\)<-\\([^]]*\\)[]][]]"
     (1 font-lock-type-face t)
     (2 font-lock-string-face))

    ;; LinkTextIsEqualToPassageName [Must be last in priority among font lock rules for links]
    ("[[][[]\\([^]]*\\)[]][]]" 1 font-lock-type-face)

    ;;;;; Variables

    ;; Variable statement
    (,(format "%s" twee-chapbook-mode--variable-init-regex) 1 font-lock-variable-name-face)

    ;; Variable statement initialized to another variable
    (,(format "%s\\([$_A-Za-z][$_A-Za-z0-9.]*\\)$" twee-chapbook-mode--variable-init-regex) 2 font-lock-variable-name-face)

    ;; Reference to a variable
    ("\\({[$_A-Za-z][$_A-Za-z0-9.]*}\\)" 1 font-lock-variable-name-face)

    (,(twee-chapbook-mode--build-variable-string-value-regex "\"")
     (2 font-lock-string-face t))
    (,(twee-chapbook-mode--build-variable-string-value-regex "'")
     (2 font-lock-string-face t))

    (,(twee-chapbook-mode--build-string-constant-regex "\"")
     (2 font-lock-string-face t))
    (,(twee-chapbook-mode--build-string-constant-regex "'")
     (2 font-lock-string-face t))

    ;; Reference to a variable inside a string in the context of a variable statement
    ;; Must be after the calls to twee-chapbook-mode--build-variable-string-value-regex
    ("^[$_A-Za-z][$_A-Za-z0-9.]*:.*?\\({[$_A-Za-z][$_A-Za-z0-9.]*}\\)" 1 font-lock-variable-name-face t)

    ;;;;; Inserts

    ;; Highlight the start of the insert like a function name
    ("{\\([^},:'\"\n]*\\)[^}]*}" 1 font-lock-function-name-face)

    ;; Highlight true and false
    ("\\(&&\\|||\\|==\\|:\\)\s+!?\\(true\\|false\\)" 2 font-lock-constant-face t)
    ("\\(true\\|false\\)\\(\s+==\\|\s*\\|\s+||\\|\s+&&]\\)" 1 font-lock-constant-face t)

    (,(twee-chapbook-mode--build-additional-parameter-name-regex)
     (1 font-lock-variable-name-face t))

    ;; Parameter string value in inserts
    (,(twee-chapbook-mode--build-string-value-regex "\"")
     (1 font-lock-variable-name-face t)
     (2 font-lock-string-face t))
    (,(twee-chapbook-mode--build-string-value-regex "'")
     (1 font-lock-variable-name-face t)
     (2 font-lock-string-face t))
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

  (add-to-list 'completion-at-point-functions
               'twee-chapbook-mode--completion-at-point))

(provide 'twee-chapbook-mode)
;;; twee-chapbook-mode.el ends here

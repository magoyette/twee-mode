;;; twee-mode.el --- Minor mode for Twee/Twine files.

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

(require 'outline)

;; Adapted from outshine-narrow-to-subtree in
;; https://github.com/alphapapa/outshine
;; and narrow-or-widen-dwim in
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
;;;###autoload
(defun twee-narrow-to-subtree-or-widen ()
  "Narrow buffer to subtree at point or widen if buffer is narrowed."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((if (outline-on-heading-p)
             (progn
               (outline-mark-subtree)
               (and
                (use-region-p)
                (narrow-to-region (region-beginning) (region-end)))
               (deactivate-mark))
           (message "Not at headline, cannot narrow to subtree")))))

;; Adapted from outline-magic
;; https://github.com/tj64/outline-magic
(defun twee--outline-next-line ()
  "Forward line, but mover over invisible line ends.
Essentially a much simplified version of `next-line'."
  (interactive)
  (beginning-of-line 2)
  (while (and (not (eobp))
              (get-char-property (1- (point)) 'invisible))
    (beginning-of-line 2)))

;; Adapted from outline-magic
;; https://github.com/tj64/outline-magic
;;;###autoload
(defun twee-outline-toggle (&optional arg)
  "Visibility toggling for twee-mode.
- When point is at the beginning of the buffer, or when called with a
  C-u prefix argument, rotate the entire buffer through 2 states:
  1. OVERVIEW: Show only top-level headlines.
  2. SHOW ALL: Show everything.
- When point is at the beginning of a headline, rotate the subtree started
  by this line through 2 different states:
  1. FOLDED:   Only the main headline is shown.
  2. SUBTREE:  Show the entire subtree, including body text.
- When point is not at the beginning of a headline, jump to the current
  headline."
  (interactive "P")
  (setq deactivate-mark t)
  (cond
   ((equal arg '(4))
    ;; Run `twee-outline-toggle' as if at the top of the buffer.
    (save-excursion
      (goto-char (point-min))
      (let ((current-prefix-argument nil))
        (twee-outline-toggle nil))))
   (t
    (cond
     ((bobp) ;; Beginning of buffer: Global cycling
      (cond
       ((eq last-command 'outline-toggle-overview)
        ;; We just showed the table of contents - now show everything
        (outline-show-all)
        (message "SHOW ALL")
        (setq this-command 'outline-toggle-showall))
       (t
        ;; Default action: go to overview
        (let ((toplevel (cond
                         (current-prefix-arg (prefix-numeric-value current-prefix-arg))
                         ((save-excursion (beginning-of-line)
                                          (looking-at outline-regexp))
                          (max 1 (funcall outline-level)))
                         (t 1))))
          (outline-hide-sublevels toplevel))
        (message "OVERVIEW")
        (setq this-command 'outline-toggle-overview))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between two different views
      (outline-back-to-heading)
      (let ((goal-column 0) beg eoh eol eos)
        ;; First, some boundaries
        (save-excursion
          (outline-back-to-heading)           (setq beg (point))
          (save-excursion (twee--outline-next-line) (setq eol (point)))
          (outline-end-of-heading)            (setq eoh (point))
          (outline-end-of-subtree)            (setq eos (point)))
        ;; Find out what to do next and set `this-command'
        (cond
         ((= eos eoh)
          ;; Nothing is hidden behind this heading
          (message "EMPTY ENTRY"))
         ((>= eol eos)
          ;; Entire subtree is hidden in one line: open it
          (outline-show-subtree)
          (message "SUBTREE"))
         (t
          ;; Default action: hide the subtree.
          (outline-hide-subtree)
          (message "FOLDED")))))
     (t
      ;; Not at a headline: Jump to headline.
      (outline-back-to-heading))))))

;;;###autoload
(define-minor-mode twee-mode
  "Minor mode for Twee/Twine files."
  :lighter " twee"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "TAB") 'twee-outline-toggle)
            (define-key map (kbd "C-x n") 'twee-narrow-to-subtree-or-widen)
            map)

  (set (make-local-variable 'imenu-generic-expression)
       '(("Passages" "^::[[:space:]]*\\(.*\\)$" 1)))

  (set (make-local-variable 'outline-regexp) "^::")
  (outline-minor-mode))

(provide 'twee-mode)
;;; twee-mode.el ends here

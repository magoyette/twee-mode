# twee-mode

`twee-mode` is a minor mode for Twee/Twine files.

`twee-mode` is intended to be used with a major mode specific to the Twine story format that provides the syntax highlight.

[twee-chapbook-mode](https://github.com/magoyette/twee-chapbook-mode) is a major mode for the Chapbook story format. Other Twine story formats currently have no Emacs major mode.

This project is now archived and no longer maintained.

## Features

- Display a menu to navigate to a passage header with Imenu
- Toggle the folding of a passage or all passages with `TAB` (like Org mode)
- Toggle narrowing on a passage with `C-x n`
- Move passages up or down with `outline-minor-mode`
- Jump to next or previous passage header with `outline-minor-mode`

## Configuration

The keybindings below are more compact keybindings for useful `outline-minor-mode` commands.

``` emacs-lisp
(define-key twee-mode-map (kbd "M-<up>") 'outline-move-subtree-up)
(define-key twee-mode-map (kbd "M-<down>") 'outline-move-subtree-down)
(define-key twee-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
(define-key twee-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
```

;;; config-meow.el --- Meow editing config -*- lexical-binding: t; -*-

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (add-to-list 'meow-mode-state-list
               '(magit-mode . motion))
  (add-to-list 'meow-mode-state-list
               '(magit-status-mode . motion))
  (setq meow-selection-command-fallback
        '((meow-change . meow-change-char)
          (meow-kill . meow-delete)
          (meow-cancel-selection . ignore)
          (meow-pop-selection . meow-pop-grab)
          (meow-beacon-change . meow-beacon-change-char)))
  (meow-leader-define-key
   ;; spc j/k will run the original command in motion state.
   '("j" . "h-j")
   '("k" . "h-k")
   '("r" . consult-ripgrep)
   '("f" . find-file)
   '("b" . consult-buffer)
   '("u" . magit-status-quick)
   ;; use spc (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("?" . meow-cheatsheet))
  
  (meow-normal-define-key
   ;; Movements
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("e" . meow-next-word)
   '("E" . forward-word)
   '("b" . meow-back-word)
   '("B" . backward-word)

   ;; Lines
   '("x" . meow-line)
   '("X" . meow-line-expand)

   ;; Selection
   '("v" . meow-block)
   '("V" . meow-to-block)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("o" . meow-open-below)
   '("O" . meow-open-above)

   ;; Actions
   '("d" . meow-kill)
   '("r" . meow-replace)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("i" . meow-insert)
   '("c" . meow-change)

   ;; Search / Jump
   '("/" . consult-line)
   '("m" . avy-goto-char-timer)
   '("f" . meow-find)
   '("F" . isearch-forward)

   ;; Undo / Redo
   '("u" . meow-undo)
   '("U" . undo-redo)
     
   ;; Expand
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)

   ;; Slurp & Barf
   '(")" . meow-forward-slurp)
   '("(" . meow-forward-barf)
   '("{" . meow-backward-slurp)
   '("}" . meow-backward-barf)
   
   '("-" . negative-argument)
   '("a" . meow-append)
   '("D" . meow-backward-delete)
   '("g" . meow-join)
   '("n" . meow-search)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("R" . meow-swap-grab)
   '("s" . meow-grab)
   '("t" . meow-till)
   '("?" . comment-dwim)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . meow-cancel-selection)
   '("%" . meow-query-replace)
   '("&" . meow-query-replace-regexp)))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode 1))

(provide 'config-meow)

; W. Batelaan.
; Extends the Skill mode in Emacs to allow
; the sending of Skill expressions and procedures to Opus.

(defun skill-mode-hook ()
  (define-key il-mode-map "\e\C-x"   'skill-eval-procedure)
  (define-key il-mode-map "\C-c\C-r" 'skill-eval-region)
  (define-key il-mode-map "\C-x\C-e" 'skill-eval-last-sexp)
)
(setq skill-mode-hook 'skill-mode-hook)

(defun skill-eval-procedure ()
  "Send the current procedure to Opus."
  (interactive "")
  (save-excursion
     (let ((end (il-end-of-fun)))
      (il-begin-of-fun)
      (skill-eval-region (point) end)
    )
  )
)

(defun skill-eval-region (start end)
  "Send the current region to Opus"
  (interactive "r")
  (write-region start end "~/.dataForOpus")
  (shell-command "$HOME/.gateToOpus.wakeMeUp")
)

(defun skill-eval-last-sexp ()
  "Send the previous sexp to Opus."
  (interactive "")
  (save-excursion
    (il-mark-sexp)
    (skill-eval-region (mark) (point))
  )
)

(defun skill-eval-buffer ()
  "Send the current buffer to Opus."
  (interactive "")
  (save-excursion
    (skill-eval-region (point-min) (point-max))
  )
)


;;; simple-drill.el --- A space repetition package. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Hebi Li

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Space Repetition
;; URL: http://github.com/lihebi/simple-drill.el

;;; Commentary:
;;; Code:

(defun simple-drill-reload ()
  (interactive)
  (simple-drill-ensure-major-mode)
  (let ((pos (point))
        (winpos (window-start)))
    (save-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Simple Drill\n")
        (insert (make-string 30 ?-))
        (insert "\n")
        (insert "hello: 你好\n")
        (insert "world: 世界\n")))
    (set-window-start (selected-window) winpos)
    (goto-char pos)))

(defun simple-drill-ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'simple-drill-mode)
    (signal 'simple-drill-error '("Not a simple drill buffer"))))

(defvar simple-drill-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "g") #'simple-drill-reload)
    map)
  "Parent keymap for all keymaps of modes derived from `simple-drill-mode'.")

(defun simple-drill ()
  "Dummy."
  (interactive)
  (let ((buffer (get-buffer-create "*simple-drill*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (simple-drill-mode)
      (simple-drill-reload))
    ;; view comment
    ;; FIXME display-buffer?
    (pop-to-buffer buffer)))

(define-derived-mode simple-drill-mode special-mode "SimpleDrill"
  "Parent major mode"
  :group 'simple-drill-mode
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(provide 'simple-drill)
;;; simple-drill.el ends here

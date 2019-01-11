;;; simple-drill.el --- A space repetition package. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Hebi Li

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Space Repetition
;; URL: http://github.com/lihebi/simple-drill.el

;;; Commentary:
;;; Code:

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar *simple-drill-words* '())

(defcustom simple-drill-history-file
  (locate-user-emacs-file "simple-drill-history.el")
  "Name of file used to remember which links have been visited.
When nil, visited links are not persisted across sessions."
  :group 'simple-drill
  :type '(choice file (const :tag "None" nil)))

(defun simple-drill-save-history ()
  (with-temp-file simple-drill-history-file
    (pp *simple-drill-words* (current-buffer))))

(defun simple-drill-load-history ()
  (when (not (file-exists-p simple-drill-history-file))
    (simple-drill-save-history))
  (with-temp-buffer
    (insert-file-contents simple-drill-history-file)
    (setq *simple-drill-words* (read (current-buffer)))))

(defface green-on-white
  '((t :foreground "dark green")) "" :group 'simple-drill)
(defface gold-on-white
  '((t :foreground "dark orange")) "" :group 'simple-drill)
(defface red-on-white
  '((t :foreground "red")) "" :group 'simple-drill)

(defun show-word (word meta)
  "Insert in UI the word and its meta data.

WORD is a string, META is a plist."
  (let ((trans (plist-get meta 'trans))
        (note (plist-get meta 'note))
        (date (plist-get meta 'date))
        (score (plist-get meta 'score))
        (level (plist-get meta 'level)))
    (widget-create 'push-button :format "%[[2]%]" :button-face 'green-on-white)
    (widget-create 'push-button :format "%[[1]%]" :button-face 'gold-on-white)
    (widget-create 'push-button :format "%[[0]%]" :button-face 'red-on-white)
    (insert "  ")
    (insert (format "%s: %s   (previous score: %s on %s, level %s)\n"
                    word trans score date level))))

;; (intern "hello")
;; (symbol-name 'hello)

(defun add-word (word trans)
  "Add a new WORD and TRANS pair."
  ;; CAUTION: use `lax-' version because I'm using string as property key
  (if (lax-plist-get *simple-drill-words* word)
      (warn
       (format "Warning: word %s already exists" word))
    (progn
      (setq *simple-drill-words*
            (lax-plist-put *simple-drill-words*
                           word `(trans ,trans
                                        note ""
                                        date ""
                                        score 0
                                        level 0)))
      (simple-drill-save-history))))

(defun test-add-word ()
  (setq *simple-drill-words* '())
  *simple-drill-words*
  (add-word "pretty" "漂亮")
  (add-word "old" "旧")
  (add-word "new" "新")
  (add-word "bad" "坏")
  (add-word "good" "好")
  (add-word "hi" "嗨"))


(defun simple-drill-reload ()
  (interactive)
  (simple-drill-ensure-major-mode)
  (let ((pos (point))
        (winpos (window-start)))
    (save-excursion
      ;; (kill-all-local-variables)
      (simple-drill-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (widget-insert "Simple Drill\n")
        (widget-insert (make-string 30 ?-))
        (widget-insert "\n")
        (let ((word-wid (widget-create 'editable-field
                                       :format "Add word: %v"
                                       :size 13
                                       :notify (lambda (wid &rest ignore)
                                                 "")
                                       ""))
              (trans-wid (widget-create 'editable-field
                                        :format " Trans: %v"
                                        :size 13
                                        :notify (lambda (wid &rest ignore)
                                                  "")
                                        "")))
          (widget-create 'push-button :format "  %[[Add]%]"
                         :notify (lambda (wid &rest ignore)
                                   (let ((word (widget-value word-wid))
                                         (trans (widget-value trans-wid)))
                                     ;; FIXME assert non-empty
                                     (add-word (string-trim word)
                                               (string-trim trans))
                                     ;; refresh
                                     (simple-drill-reload)))))
        (widget-insert "\n")
        (widget-insert (make-string 30 ?-))
        (widget-insert "\n")
        (widget-insert "hello: 你好\n")
        (widget-insert "world: 世界\n")
        (widget-insert (make-string 30 ?-))
        (widget-insert "\n")
        ;; show all
        (mapc (lambda (x)
                (show-word (car x) (cadr x)))
              (seq-partition *simple-drill-words* 2))
        (widget-setup)))
    (set-window-start (selected-window) winpos)
    (goto-char pos)))

(defun simple-drill-ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'simple-drill-mode)
    (signal 'simple-drill-error '("Not a simple drill buffer"))))

(defvar simple-drill-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (set-keymap-parent map widget-keymap)
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
      (simple-drill-mode)
      (simple-drill-load-history)
      (simple-drill-reload))
    ;; FIXME display-buffer?
    (pop-to-buffer buffer)))

(define-derived-mode simple-drill-mode special-mode "SimpleDrill"
  "Parent major mode"
  :group 'simple-drill-mode
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only nil))

(provide 'simple-drill)
;;; simple-drill.el ends here

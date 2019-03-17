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
(defface blue-on-white
  '((t :foreground "blue")) "" :group 'simple-drill)
(defface simple-drill-title
  '((t :height 1.8
       :forground "black")) "" :group 'simple-drill)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-drill-update-word (word score)
  "score can be 0, 1, 2 for hard, so-so, simple."
  ;; get meta list
  (let* ((meta (lax-plist-get *simple-drill-words* word))
         (level (plist-get meta 'level)))
    ;; update
    (let* ((new-level (case score
                        ((0) (max (1- level) 0))
                        ((1) level)
                        ((2) (1+ level))
                        (t (error "error"))))
           ;; construct new meta data
           (new-meta (plist-put
                      (plist-put
                       (plist-put
                        meta 'score score)
                       'level new-level)
                      'date (current-time))))
      ;; FIXME not sure if this is necessary, as plist-put has side effect
      (setq *simple-drill-words*
            (lax-plist-put *simple-drill-words* word new-meta))
      ;; save file
      (simple-drill-save-history)
      (simple-drill-reload))))

(defun my-time-string (time)
  (format "%s (%s)"
          (format-time-string "%m/%d/%y"
                              time)
          (- (time-to-days time)
             (time-to-days (current-time)))))

(defun scheduled-time (time level)
  ;; the time is calculated as: 2^(min level 8)
  (time-add time
            (* (expt 2 (min level 8)) 24 3600)))

(defun meta-get-scheduled-time-relative (meta)
  (relative-day
   (scheduled-time
    (plist-get meta 'date)
    (plist-get meta 'level))))

(defun relative-day (time)
  "Calculate the relative date from TIME to today.

I.e. TIME - today."
  (- (time-to-days time)
     (time-to-days (current-time))))


(defun simple-drill-add-word (word trans &optional date)
  "Add a new WORD and TRANS pair."
  ;; CAUTION: use `lax-' version because I'm using string as property
  ;; key. I can consider using symbols:
  ;;
  ;; (intern "hello")
  ;; (symbol-name 'hello)
  (if (lax-plist-get *simple-drill-words* word)
      (warn
       (format "Warning: word %s already exists" word))
    (progn
      (setq *simple-drill-words*
            (lax-plist-put *simple-drill-words*
                           word `(trans ,trans
                                        note nil
                                        date ,(or date (current-time))
                                        score 0
                                        level 0)))
      (simple-drill-save-history))))

(defun test-simple-drill-add-word ()
  (setq *simple-drill-words* '())
  *simple-drill-words*
  (simple-drill-add-word "pretty" "漂亮")
  (simple-drill-add-word "old" "旧" (encode-time 0 0 0 1 12 2018))
  (simple-drill-add-word "new" "新")
  (simple-drill-add-word "bad" "坏" (encode-time 0 0 0 30 9 2018))
  (simple-drill-add-word "good" "好" (encode-time 0 0 0 30 9 2019))
  (simple-drill-add-word "hi" "嗨"))

(defun partition-words ()
  ;; I want to make three sections: overdue, completed today, old
  ;; FIXME these are duplicated
  (let* ((all-words (seq-partition *simple-drill-words* 2))
         ;; today finished
         (today-words (seq-filter (lambda (x)
                                    (= 0 (relative-day
                                          (plist-get (cadr x) 'date))))
                                  all-words))
         (other-words (seq-filter (lambda (x)
                                    (not
                                     (= 0 (relative-day
                                           (plist-get (cadr x) 'date)))))
                                  all-words))
         ;; sort by schedule
         (other-words-sorted (seq-sort
                              (lambda (x y)
                                (< (meta-get-scheduled-time-relative
                                    (cadr x))
                                   (meta-get-scheduled-time-relative
                                    (cadr y))))
                              other-words))
         ;; overdue and underdue
         (overdued-words (seq-filter
                          (lambda (x)
                            (>= 0 (meta-get-scheduled-time-relative
                                   (cadr x))))
                          other-words-sorted))
         (underdued-words (seq-filter
                           (lambda (x)
                             (< 0 (meta-get-scheduled-time-relative
                                   (cadr x))))
                           other-words-sorted)))
    (list overdued-words today-words underdued-words)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-header ()
  "Show header."
  (insert (format "%-10s %-15s %-8s %-8s %-15s %-15s %-15s\n"
                  "Action" "Word" "OldScore" "Level"
                  "LastDate" "Scheduled"
                  "Translate")))


(defun simple-drill--show-word (word meta)
  "Insert in UI the word and its meta data.

WORD is a string, META is a plist."
  (let ((trans (plist-get meta 'trans))
        (note (plist-get meta 'note))
        (date (plist-get meta 'date))
        (score (plist-get meta 'score))
        (level (plist-get meta 'level)))
    (widget-create 'push-button :format "%[[2]%]"
                   :button-face 'green-on-white
                   :help-echo word
                   :notify (lambda (wid &rest ignore)
                             (simple-drill-update-word word 2)))
    (widget-create 'push-button :format "%[[1]%]"
                   :button-face 'gold-on-white
                   :notify (lambda (wid &rest ignore)
                             (simple-drill-update-word word 1)))
    (widget-create 'push-button :format "%[[0]%]"
                   :button-face 'red-on-white
                   :notify (lambda (wid &rest ignore)
                             (simple-drill-update-word word 0)))
    (insert "  ")
    (insert (format "%-15s" word))
    (insert " ")
    (insert (format "%-8s %-8s %-15s %-15s"
                    score level
                    (my-time-string date)
                    ;; compute scheduled time
                    ;; date + 2^level
                    (my-time-string
                     (scheduled-time date level))))
    (insert " ")
    ;; put the translation at the end, because there is no way to
    ;; control the size of the format string
    (widget-create 'toggle :on trans :off "??"
                   :format "%[%v%]"
                   :indent 8
                   :offset 8
                   nil)
    (insert "\n")))

(defun simple-drill--insert-line ()
  "Insert a horizontal line."
  (insert (make-string 30 ?-))
  (insert "\n"))

(defun simple-drill--show-words (words)
  "Display WORDS in group of 10 with separators."
  (dolist (10-words (seq-partition words 10))
    (mapc (lambda (x)
            (simple-drill--show-word (car x) (cadr x)))
          10-words)
    (simple-drill--insert-line)))

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
        (widget-insert (propertize "Simple Drill\n" 'face 'simple-drill-title))
        (widget-insert (make-string 30 ?-))
        (widget-insert "\n")
        (let ((word-wid (widget-create 'editable-field
                                       :format "Add new word: %v"
                                       :size 13 ""))
              (trans-wid (widget-create 'editable-field
                                        :format " Trans: %v"
                                        :size 13 "")))
          (widget-create 'push-button :format "  %[[Add]%]"
                         :notify (lambda (wid &rest ignore)
                                   (let ((word (widget-value word-wid))
                                         (trans (widget-value trans-wid)))
                                     ;; FIXME assert non-empty
                                     (simple-drill-add-word (string-trim word)
                                                            (string-trim trans))
                                     ;; move to the add-new-word textfield
                                     (widget-backward 2)
                                     ;; refresh
                                     (simple-drill-reload)))))
        (widget-insert "\n")
        (widget-insert (make-string 30 ?-))
        (widget-insert "\n")
        (show-header)
        (widget-insert (make-string 30 ?-))
        (widget-insert "\n")
        (let* ((partitioned (partition-words))
               (overdued-words (car partitioned))
               (today-words (cadr partitioned))
               (underdued-words (caddr partitioned)))
          (insert (propertize "Overdue:\n" 'face 'blue-on-white))
          (simple-drill--show-words overdued-words)
          (insert (propertize "Finished Today:\n" 'face 'blue-on-white))
          (simple-drill--show-words today-words)
          (insert (propertize "Underdue:\n" 'face 'blue-on-white))
          (simple-drill--show-words underdued-words))
        (widget-setup)))
    (set-window-start (selected-window) winpos)
    (goto-char pos)))

(defun simple-drill-ensure-major-mode ()
  "Barf if current buffer is not derived from `hackernews-mode'."
  (unless (derived-mode-p #'simple-drill-mode)
    (signal 'simple-drill-error '("Not a simple drill buffer"))))

(defvar simple-drill-mode-map
  (let ((map (make-keymap)))
    ;; (suppress-keymap map t)
    (set-keymap-parent map widget-keymap)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "g" #'simple-drill-reload)
    (define-key map "q" #'quit-window)
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

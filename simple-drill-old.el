;;; simple-drill.el --- A space repetition package. -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Hebi Li

;; Author: Hebi Li <lihebi.com@gmail.com>
;; Version: 0.1
;; Keywords: Space Repetition
;; URL: http://github.com/lihebi/simple-drill.el

;;; Commentary:
;;; Code:

;; find the :drill: tag
;; get the list of all subtrees
;; for each subtree:
;; get title, rest
;; get property: (past attempts (list (time score)))
;; compute schedule time (or read from precomputed)
;; show all entries of due cards
;; support mark on the interface directly, just as org-agenda

(defun myorg-forward-heading-same-level ()
  "Move 1 heading.  Return t if moved, nil if not."
  (let ((old (point)))
    (org-forward-heading-same-level 1)
    (if (= old (point)) nil t)))

(defun myorg-subtree-points ()
  "Assume the current point is at a org heading."
  (interactive)
  (org-next-visible-heading 1)
  (cons (point)
        (cl-loop while (myorg-forward-heading-same-level)
                 collect (point))))

(defun collect-cards ()
  "Collect cards."
  (let ((org-tags-match-list-sublevels nil))
    (apply #'append
           (org-scan-tags (lambda ()
                            ;; do something here
                            ;; collect title
                            ;; collect property
                            ;; collect body
                            (myorg-subtree-points))
                          (cdr (org-make-tags-matcher "drill"))
                          nil))))

(defun read-card (pos)
  "Read title and body of card."
  (goto-char pos)
  (let ((title (nth 4 (org-heading-components)))
        (body (prog2 (progn (org-narrow-to-subtree)
                            (forward-line))
                  (buffer-substring-no-properties (point) (point-max))
                (widen))))
    (list title body)))


(defun create-interface (cards)
  "Interface.  g to refresh.

CARDS is a list of cards to review."
  (let ((buffer (get-buffer-create "*Simple Drill*"))
        (cards-meta (mapcar #'read-card cards)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Welcome to Simple Drill Buffer\n")
      (insert "----------------------------------\n")
      (insert "\n\n\n")
      ;; (insert (format "%s" cards-meta))
      (mapc (lambda (x)
              (insert
               (format "** TODO %s\n%s\n" (car x) (cadr x))))
            cards-meta)
      (simple-drill-mode)
      (outline-hide-body))
    (display-buffer buffer)))

(defvar simple-drill-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map   [tab]     'outline-toggle-children)
    (define-key map (kbd "n") 'outline-next-heading)
    (define-key map (kbd "p") 'outline-previous-heading)
    ;; (define-key map (kbd "SPC") 'magit-diff-show-or-scroll-up)
    ;; (define-key map (kbd "DEL") 'magit-diff-show-or-scroll-down)
    map)
  "Parent keymap for all keymaps of modes derived from `simple-drill-mode'.")

(defun simple-drill ()
  "Dummy."
  (interactive)
  (create-interface (collect-cards)))

(global-set-key (kbd "C-c h h")
                'simple-drill)


(define-derived-mode simple-drill-mode org-mode "SimpleDrill"
  "Parent major mode"
  :group 'simple-drill-mode
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(provide 'simple-drill)
;;; simple-drill.el ends here

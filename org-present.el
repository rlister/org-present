;; org-present.el -- Minimalist presentation minor-mode for Emacs org-mode.
;; 
;; Copyright (C) 2012 by Ric Lister
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
;;
;; Usage:
;;
;; Add the following to your emacs config:
;;
;;   (add-to-list 'load-path "~/path/to/org-present")
;;   (autoload 'org-present "org-present" nil t)
;;
;;   (add-hook 'org-present-mode-hook
;;             (lambda ()
;;               (org-present-big)
;;               (org-display-inline-images)))
;;  
;;   (add-hook 'org-present-mode-quit-hook
;;             (lambda ()
;;               (org-present-small)
;;               (org-remove-inline-images)))
;;
;; Open an org-mode file with each slide under a top-level heading.
;; Start org-present with org-present-mode, left and right keys will move forward
;; and backward through slides. C-c C-q will quit org-present.
;;
;; This works well with hide-mode-line (http://webonastick.com/emacs-lisp/hide-mode-line.el),
;; which hides the mode-line when only one frame and buffer are open.
;;
;; If you're on a Mac you might also want to look at the fullscreen patch here:
;; http://cloud.github.com/downloads/typester/emacs/feature-fullscreen.patch

(defvar org-present-mode-keymap (make-keymap) "org-present-mode keymap.")

;; left and right page keys
(define-key org-present-mode-keymap [right]         'org-present-next)
(define-key org-present-mode-keymap [left]          'org-present-prev)
(define-key org-present-mode-keymap (kbd "C-c C-=") 'org-present-big)
(define-key org-present-mode-keymap (kbd "C-c C--") 'org-present-small)
(define-key org-present-mode-keymap (kbd "C-c C-q") 'org-present-quit)

;; how much to scale up font size
(defvar org-present-text-scale 5)

(define-minor-mode org-present-mode
  "Minimalist presentation minor mode for org-mode."
  nil
  " OP"
  org-present-mode-keymap)

(make-variable-buffer-local 'org-present-mode)

(defun org-present-top ()
  "Jump to current top-level heading, should be safe outside a heading."
  (interactive)
  (unless (org-at-heading-p) (outline-previous-heading))
  (let ((level (org-current-level)))
    (when (and level (> level 1))
      (outline-up-heading (- level 1))
      (message (number-to-string level)))))

(defun org-present-next ()
  "Jump to next top-level heading."
  (interactive)
  (if (org-current-level)
      (progn
        (org-present-top)
        (widen)
        (org-get-next-sibling))
    (outline-next-heading))
  (org-present-narrow))

(defun org-present-prev ()
  "Jump to previous top-level heading."
  (interactive)
  (if (org-current-level)
      (progn
        (widen)
        (org-present-top)
        (org-get-last-sibling)))
  (org-present-narrow))

(defun org-present-narrow ()
  "Show just current page; in a heading we narrow, else show all headings folded (initial page)."
  (interactive)
  (if (org-current-level)
      (progn
        (org-narrow-to-subtree)
        (show-all))
    (org-cycle `(4))))

(defun org-present-big ()
  "Make font size larger."
  (interactive)
  (text-scale-increase 0)
  (text-scale-increase org-present-text-scale))

(defun org-present-small ()
  "Change font size back to original."
  (interactive)
  (text-scale-increase 0))

(defun org-present-quit ()
  "Quit the minor-mode."
  (interactive)
  (org-present-small)
  (widen)
  (run-hooks 'org-present-mode-quit-hook)
  (setq org-present-mode nil))

(run-hooks 'org-present-mode-hook)

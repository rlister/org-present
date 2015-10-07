;;; org-present.el --- Minimalist presentation minor-mode for Emacs org-mode.
;;
;; Copyright (C) 2012 by Ric Lister
;;
;; Author: Ric Lister
;; Package-Requires: ((org "7")(elnode "0.9"))
;; URL: https://github.com/rlister/org-present
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
;;; Commentary:
;;
;; This is meant to be an extremely minimalist presentation tool for
;; Emacs org-mode.
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

(require 'elnode)

(defvar org-present-mode-keymap (make-keymap) "org-present-mode keymap.")

;; left and right page keys
(define-key org-present-mode-keymap [right]         'org-present-next)
(define-key org-present-mode-keymap [left]          'org-present-prev)
(define-key org-present-mode-keymap (kbd "C-c C-=") 'org-present-big)
(define-key org-present-mode-keymap (kbd "C-c C--") 'org-present-small)
(define-key org-present-mode-keymap (kbd "C-c C-q") 'org-present-quit)
(define-key org-present-mode-keymap (kbd "C-c C-r") 'org-present-read-only)
(define-key org-present-mode-keymap (kbd "C-c C-w") 'org-present-read-write)
(define-key org-present-mode-keymap (kbd "C-c <")   'org-present-beginning)
(define-key org-present-mode-keymap (kbd "C-c >")   'org-present-end)

;; how much to scale up font size
(defvar org-present-text-scale 5)
(defvar org-present-cursor-cache (or cursor-type nil)
  "Holds the user set value of cursor for `org-present-read-only'")
(defvar org-present-overlays-list nil)

;; the HTML displayed in the remote control web page
(defvar org-present-html-template
  "<!doctype html>
   <html>
     <head>
       <meta charset='utf-8' />
       <title>%s</title> <!-- presentation name -->
       <style type='text/css'>
         h1 {
           font-size: 9vmin;
         }
         h2 {
           font-size: 7vmin;
         }
         body {
           font-size: 5vmin;
         }
         .next {
           float: right;
         }
         .prev {
           float: left;
         }
         .logo {
           text-align: center;
         }
         .next, .prev {
           font-size: 12vmin;
         }
         img.icon {
           height: 160px;
         }
       </style>
     </head>
     <body>
       <div class='next'><a href='/next'>Next</a></div>
       <div class='prev'><a href='/prev'>Prev</a></div>
       <div class='logo'><a href='http://orgmode.org/'><img class='icon' src='http://orgmode.org/img/org-mode-unicorn-logo.png' alt='org-mode' /></a></div>
       <hr>
       <h1>%s</h1> <!-- presentation name -->
       <h2>%s</h2> <!-- slide title -->
     </body>
   </html>")

;; which remote control routes should be hooked up to which handlers
(defvar org-present-routes
  '(("^.*//prev$" . org-present-prev-handler)
    ("^.*//next$" . org-present-next-handler)
    ("^.*//$" . org-present-default-handler)))

;; the TCP/IP port on which to listen
(defvar org-present-port 8009)

(define-minor-mode org-present-mode
  "Minimalist presentation minor mode for org-mode."
  :init-value nil
  :lighter " OP"
  :keymap org-present-mode-keymap)

(make-variable-buffer-local 'org-present-mode)

(defun org-present-top ()
  "Jump to current top-level heading, should be safe outside a heading."
  (unless (org-at-heading-p) (outline-previous-heading))
  (let ((level (org-current-level)))
    (when (and level (> level 1))
      (outline-up-heading (- level 1) t)))
  (org-present-remote-set-title))

(defun org-present-next ()
  "Jump to next top-level heading."
  (interactive)
  (widen)
  (if (org-current-level) ;inside any heading
      (progn
        (org-present-top)
        (or
         (org-get-next-sibling) ;next top-level heading
         (org-present-top)))    ;if that was last, go back to top before narrow
    ;; else handle title page before first heading
    (outline-next-heading))
  (org-present-narrow)
  (org-present-remote-set-title))

(defun org-present-prev ()
  "Jump to previous top-level heading."
  (interactive)
  (if (org-current-level)
      (progn
        (widen)
        (org-present-top)
        (org-get-last-sibling)))
  (org-present-narrow)
  (org-present-remote-set-title))

(defun org-present-narrow ()
  "Show just current page; in a heading we narrow, else show title page (before first heading)."
  (if (org-current-level)
      (progn
        (org-narrow-to-subtree)
        (show-all))
    ;; else narrow to area before first heading
    (outline-next-heading)
    (narrow-to-region (point-min) (point))
    (goto-char (point-min))))

(defun org-present-beginning ()
  "Jump to first slide of presentation."
  (interactive)
  (widen)
  (beginning-of-buffer)
  (org-present-narrow))

(defun org-present-end ()
  "Jump to last slide of presentation."
  (interactive)
  (widen)
  (end-of-buffer)
  (org-present-top)
  (org-present-narrow))

(defun org-present-big ()
  "Make font size larger."
  (interactive)
  (text-scale-increase 0)
  (text-scale-increase org-present-text-scale)) ;MAKE THIS BUFFER-LOCAL

(defun org-present-small ()
  "Change font size back to original."
  (interactive)
  (text-scale-increase 0))

(defun org-present-add-overlay (beginning end)
  "Create a single overlay over given region and remember it."
  (let ((overlay (make-overlay beginning end)))
    (push overlay org-present-overlays-list)
    (overlay-put overlay 'invisible 'org-present)))

(defun org-present-show-option (string)
  "Returns non-nil if string is an org-mode exporter option whose value we want to show."
  (save-match-data
    (string-match
     (regexp-opt '("title:" "author:" "date:" "email:"))
     string)))

(defun org-present-add-overlays ()
  "Add overlays for this mode."
  (add-to-invisibility-spec '(org-present))
  (save-excursion
    ;; hide org-mode options starting with #+
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\([^[:space:]]+\\).*" nil t)
      (let ((end (if (org-present-show-option (match-string 2)) 2 0)))
        (org-present-add-overlay (match-beginning 1) (match-end end))))
    ;; hide stars in headings
    (goto-char (point-min))
    (while (re-search-forward "^\\(*+\\)" nil t)
      (org-present-add-overlay (match-beginning 1) (match-end 1)))
    ;; hide emphasis markers
    (goto-char (point-min))
    (while (re-search-forward org-emph-re nil t)
      (org-present-add-overlay (match-beginning 2) (1+ (match-beginning 2)))
      (org-present-add-overlay (1- (match-end 2)) (match-end 2)))))

(defun org-present-rm-overlays ()
  "Remove overlays for this mode."
  (mapc 'delete-overlay org-present-overlays-list)
  (remove-from-invisibility-spec '(org-present)))

(defun org-present-read-only ()
  "Make buffer read-only."
  (interactive)
  (setq buffer-read-only t)
  (setq org-present-cursor-cache cursor-type
        cursor-type nil)
  (define-key org-present-mode-keymap (kbd "SPC") 'org-present-next))

(defun org-present-read-write ()
  "Make buffer read-only."
  (interactive)
  (setq buffer-read-only nil)
  (setq cursor-type org-present-cursor-cache)
  (define-key org-present-mode-keymap (kbd "SPC") 'self-insert-command))

(defun org-present-hide-cursor ()
  "Hide the cursor for current window."
  (interactive)
  (internal-show-cursor (selected-window) nil))

(defun org-present-show-cursor ()
  "Show the cursor for current window."
  (interactive)
  (internal-show-cursor (selected-window) t))

(defun org-present-html ()
  "Build the page HTML from the template and selected variables."
  (format org-present-html-template
          org-present-remote-buffer
          org-present-remote-buffer
          org-present-remote-title))

(defun org-present-prev-handler (httpcon)
  "Call org-present-prev when someone GETs /prev, and return the remote control page."
  (with-current-buffer org-present-remote-buffer (org-present-prev))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (org-present-html)))

(defun org-present-next-handler (httpcon)
  "Call org-present-next when someone GETs /prev, and return the remote control page."
  (with-current-buffer org-present-remote-buffer (org-present-next))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (org-present-html)))

(defun org-present-default-handler (httpcon)
  "Return the remote control page."
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (org-present-html)))

(defun org-present-root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon org-present-routes))

(defun org-present-remote-set-title ()
  "Set the title to display in the remote control."
  (let ((title-text (thing-at-point 'line)))
    (setq org-present-remote-title
          (org-present-trim-string
           (replace-regexp-in-string "^[ \*]" "" title-text)))))

(defun org-present-remote-on (host)
  "Turn the org-present remote control on."
  (interactive "sStart remote control for this buffer on host: ")
  (setq elnode-error-log-to-messages nil)
  (elnode-stop org-present-port)
  (setq org-present-remote-buffer (current-buffer))
  (elnode-start 'org-present-root-handler :port org-present-port :host host))

(defun org-present-remote-off ()
  "Turn the org-present remote control off."
  (interactive)
  (elnode-stop org-present-port)
  (setq org-present-remote-buffer nil))

;; courtesy Xah Lee ( http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html )
(defun org-present-trim-string (string)
  "Remove whitespace (space, tab, emacs newline (LF, ASCII 10)) in beginning and ending of STRING."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;;;###autoload
(defun org-present ()
  "init."
  (interactive)
  (setq org-present-mode t)
  (org-present-add-overlays)
  (org-present-narrow)
  (run-hooks 'org-present-mode-hook)
  (org-present-remote-set-title))

(defun org-present-quit ()
  "Quit the minor-mode."
  (interactive)
  (org-present-small)
  (org-present-rm-overlays)
  (widen)
  ;; Exit from read-only mode before exiting the minor mode
  (when buffer-read-only
    (org-present-read-write))
  (run-hooks 'org-present-mode-quit-hook)
  (setq org-present-mode nil))

(provide 'org-present)
;;; org-present.el ends here

;;; bookmark-view.el --- Bookmark views -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/minad/bookmark-view

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bookmark views

;;; Code:

(require 'bookmark)
(require 'seq)

(defgroup bookmark-view nil
  "Bookmark views."
  :group 'convenience
  :prefix "bookmark-view-")

(defcustom bookmark-view-name-format "V %Y-%m-%d-%H:%M <buffers>"
  "Name format used for default name of new view bookmarks."
  :type 'string)

(defcustom bookmark-view-stack "V *stack*"
  "Name of the default view bookmark stack."
  :type 'string)

(defcustom bookmark-view-filter-function
  #'bookmark-view-filter-default
  "Filter function called for each buffer.
Return t if the current buffer is supposed to be bookmarked."
  :type 'symbol)

(defun bookmark-view-filter-default ()
  "Default filter function called for each buffer.
Return t if the current buffer is supposed to be bookmarked."
  (not (and (eq bookmark-make-record-function #'bookmark-make-record-default)
            (string-match-p "^ " (buffer-name)))))

(defun bookmark-view--make-record ()
  "Return a new bookmark record for the current buffer, which must not have a backing file."
  (if (and (not buffer-file-name)
           (eq bookmark-make-record-function #'bookmark-make-record-default))
      (cons
       (bookmark-buffer-name)
       (list (cons 'buffer (buffer-name))
             (cons 'handler #'bookmark-view-handler-fallback)))
    (bookmark-make-record)))

(defun bookmark-view--buffers (&optional frame)
  "Return list of buffers of FRAME to be bookmarked."
  (seq-filter (lambda (x) (with-current-buffer x (funcall bookmark-view-filter-function)))
              (mapcar #'window-buffer (window-list frame 'no-minibuf))))

(defun bookmark-view--get (&optional frame)
  "Get view state of FRAME as a bookmark record."
  (let ((bufs (bookmark-view--buffers)))
    (list (cons 'buffer (mapcar (lambda (x) (with-current-buffer x (bookmark-view--make-record))) bufs))
          (cons 'filename (format "*View[%s]*" (length bufs)))
          (cons 'window (window-state-get (frame-root-window frame) 'writable))
          (cons 'handler #'bookmark-view-handler))))

(defun bookmark-view--put (state &optional frame)
  "Put view STATE into FRAME, restoring windows and buffers."
  (save-window-excursion
    (dolist (buf (alist-get 'buffer state))
      (condition-case err
          (bookmark-jump buf #'ignore)
        (error (delay-warning 'bookmark-view (format "Error %S when opening %S" err buf))))))
  (window-state-put (alist-get 'window state) (frame-root-window frame)))

(defun bookmark-view-default-name (&optional frame)
  "Default name for current view of FRAME."
  (replace-regexp-in-string
   "<buffers>"
   (mapconcat (lambda (x) (buffer-name x)) (bookmark-view--buffers frame) " ")
   (format-time-string bookmark-view-name-format) t t))

(defun bookmark-view-read (prompt &optional default)
  "Prompting with PROMPT for bookmarked view. Return DEFAULT if user input is empty."
  (completing-read prompt
                   (let ((names (bookmark-view-names)))
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           '(metadata (category . bookmark))
                         (complete-with-action action names str pred))))
                   nil nil nil 'bookmark-history default))

;;;###autoload
(defun bookmark-view-names ()
  "Return a list of names of all view bookmarks."
  (bookmark-maybe-load-default-file)
  (mapcar #'car (seq-filter (lambda (x) (eq #'bookmark-view-handler (alist-get 'handler (cdr x)))) bookmark-alist)))

;;;###autoload
(defun bookmark-view-handler-fallback (bm)
  "Handle buffer bookmark BM, used for buffers without file."
  (let* ((bm (bookmark-get-bookmark-record bm))
         (name (alist-get 'buffer bm)))
    (unless (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (insert (format "bookmark-view: Buffer %s not found" name))))))

;;;###autoload
(defun bookmark-view-handler (bm)
  "Handle view bookmark BM."
  ;; Restore the view in the bookmark-after-jump-hook, since
  ;; it must happen after the execution of the display function.
  (letrec ((hook (lambda ()
                   (setq bookmark-after-jump-hook (delq hook bookmark-after-jump-hook))
                   (bookmark-view--put (bookmark-get-bookmark-record bm)))))
    (push hook bookmark-after-jump-hook)))

;;;###autoload
(defun bookmark-view (name)
  "If view bookmark NAME exists, open it, otherwise save current view under the given NAME."
  (interactive (list (bookmark-view-read "View: " (bookmark-view-default-name))))
  (if (assoc name bookmark-alist)
      (bookmark-view-open name)
    (bookmark-view-save name)))

;;;###autoload
(defun bookmark-view-save (name &optional no-overwrite)
  "Save current view under the given NAME.
If NO-OVERWRITE is non-nil push to the bookmark list without overwriting an already existing bookmark."
  (interactive (list (bookmark-view-read "Save view: " (bookmark-view-default-name))))
  (bookmark-store name (bookmark-view--get) no-overwrite))

;;;###autoload
(defun bookmark-view-open (bm)
  "Open view bookmark BM."
  (interactive (list (bookmark-view-read "Open view: ")))
  (bookmark-jump bm #'ignore))

;;;###autoload
(defun bookmark-view-delete (name)
  "Delete view bookmark NAME."
  (interactive (list (bookmark-view-read "Delete view: ")))
  (bookmark-delete name))

;;;###autoload
(defun bookmark-view-rename (old &optional new)
  "Rename bookmark from OLD name to NEW name."
  (interactive (list (bookmark-view-read "Old view name: ")))
  (bookmark-rename old new))

;;;###autoload
(defun bookmark-view-push (name)
  "Push current view to the bookmark stack NAME."
  (interactive (list bookmark-view-stack))
  (bookmark-view-save name 'no-overwrite))

;;;###autoload
(defun bookmark-view-pop (name)
  "Pop current view from the bookmark stack NAME."
  (interactive (list bookmark-view-stack))
  (bookmark-view-open name)
  (bookmark-delete name))

(provide 'bookmark-view)

;;; bookmark-view.el ends here

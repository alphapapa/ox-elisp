;;; ox-elisp.el --- Org Export Emacs Lisp backend    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Export Org buffers to Emacs Lisp comments.

;;; Code:

;;;; Requirements

(require 'ox)
(require 'ox-ascii)

;;;; Variables


;;;; Customization

(defgroup ox-elisp nil
  "Org Export to Emacs Lisp comments."
  :group 'ox)

(defcustom ox-elisp-semicolons t
  "Prepend semicolons to headings appropriate to their level."
  :type 'boolean)

;;;; Define back-end

(org-export-define-derived-backend 'elisp 'ascii
  :menu-entry
  '(?t 1
       ((?e "As Elisp buffer (UTF-8)"
            (lambda (a s v b)
              (ox-elisp-export-as-elisp a s v b '(:ascii-charset utf-8))))
        (?E "As Elisp buffer (ASCII)"
            (lambda (a s v b)
              (ox-elisp-export-as-elisp a s v b '(:ascii-charset ascii))))))
  :options-alist '((:section-numbers nil "num" nil)
                   (:with-author nil "author" nil)
                   (:with-title nil "title" nil)
                   (:with-toc nil "toc" nil))
  :translate-alist '((headline . ox-elisp-headline)))

;;;; Commands

(defun ox-elisp-export-as-elisp
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as comments in an Emacs Lisp buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Elisp Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'elisp "*Org Elisp Export*"
    async subtreep visible-only body-only ext-plist
    (lambda ()
      (emacs-lisp-mode)
      (comment-region (point-min) (point-max))
      (delete-trailing-whitespace))))

;;;; Functions

(defun ox-elisp-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Elisp comment.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (cl-letf (((symbol-function 'org-ascii--build-title) (symbol-function 'ox-elisp--build-title)))
    (org-ascii-headline headline contents info)))

(defun ox-elisp--build-title
    (element info text-width
             &optional underline notags toc)
  "Format ELEMENT title and return it.

ELEMENT is either an `headline' or `inlinetask' element.  INFO is
a plist used as a communication channel.  TEXT-WIDTH is an
integer representing the maximum length of a line.

When optional argument UNDERLINE is non-nil, underline title,
without the tags, according to `org-ascii-underline'
specifications.

If optional argument NOTAGS is non-nil, no tags will be added to
the title.

When optional argument TOC is non-nil, use optional title if
possible.  It doesn't apply to `inlinetask' elements.

When `ox-elisp-semicolons' is non-nil, prepend semicolons
to headings appropriate to their depth.  This option should not
be set in addition to UNDERLINE."
  (let* ((headlinep (eq (org-element-type element) 'headline))
	 (numbers
	  ;; Numbering is specific to headlines.
	  (and headlinep (org-export-numbered-headline-p element info)
	       ;; All tests passed: build numbering string.
	       (concat
		(mapconcat
		 'number-to-string
		 (org-export-get-headline-number element info) ".")
		" ")))
	 (text
	  (org-trim
	   (org-export-data
	    (if (and toc headlinep) (org-export-get-alt-title element info)
	      (org-element-property :title element))
	    info)))
	 (todo ""
               ;; NOTE: Not using keywords.
	       ;; (and (plist-get info :with-todo-keywords)
	       ;;      (let ((todo (org-element-property :todo-keyword element)))
	       ;;        (and todo (concat (org-export-data todo info) " "))))
               )
	 (tags ""
               ;; NOTE: Not using tags.
               ;; (and (not notags)
               ;;      (plist-get info :with-tags)
               ;;      (let ((tag-list (org-export-get-tags element info)))
               ;;        (and tag-list
               ;;             (format ":%s:"
               ;;                     (mapconcat 'identity tag-list ":")))))
               )
	 (priority ""
                   ;; NOTE: Not using priority.
                   ;; (and (plist-get info :with-priority)
                   ;;      (let ((char (org-element-property :priority element)))
                   ;;        (and char (format "(#%c) " char))))
                   )
	 (first-part (concat numbers todo priority text)))
    (cond ((and underline headlinep)
           ;; Maybe underline text, if ELEMENT type is `headline' and an
           ;; underline character has been defined.
           (concat first-part
                   ;; NOTE: Not using tags.
                   ;; ;; Align tags, if any.
                   ;; (when tags
                   ;;   (format
                   ;;    (format " %%%ds"
                   ;;    	(max (- text-width  (1+ (string-width first-part)))
                   ;;    	     (string-width tags)))
                   ;;    tags))
                   (let ((under-char
                          (nth (1- (org-export-get-relative-level element info))
                               (cdr (assq (plist-get info :ascii-charset)
                                          (plist-get info :ascii-underline))))))
                     (and under-char
                          (concat "\n"
                                  (make-string (/ (string-width first-part)
                                                  (char-width under-char))
                                               under-char))))))
          ((and ox-elisp-semicolons headlinep)
           (let* ((level (org-export-get-relative-level element info))
                  (number (+ level 3))
                  (semicolons (make-string number ?*)))
             (concat semicolons " " first-part))))))

;;;; Footer

(provide 'ox-elisp)

;;; ox-elisp.el ends here

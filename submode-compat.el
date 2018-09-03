;;; submode-compat.el --- Compatibility with older GNU Emacs versions  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Magnus Nyberg

;; Author: Magnus Nyberg <magnus@nexter.se>
;; Keywords: submode, compatibility
;; URL: https://github.com/nxtr/submode

;; This file contains code from GNU Emacs, which is
;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; Backward compatibility definitions for older GNU Emacs versions.

;;; Code:

(defvar-local submode-compat--buffer-is-main-mode nil
  "If non-nil, main-mode for this very buffer.")

;;;; prog-indentation-context

(unless (boundp 'prog-indentation-context)
  (defvar prog-indentation-context nil
    "When non-nil, provides context for indenting embedded code chunks.
There are languages where part of the code is actually written in
a sub language, e.g., a Yacc/Bison or ANTLR grammar can also include
JS or Python code.  This variable enables the primary mode of the
main language to use the indentation engine of the sub-mode for
lines in code chunks written in the sub-mode's language.
When a major mode of such a main language decides to delegate the
indentation of a line/region to the indentation engine of the sub
mode, it should bind this variable to non-nil around the call.
The non-nil value should be a list of the form:
   (FIRST-COLUMN . REST)
FIRST-COLUMN is the column the indentation engine of the sub-mode
should use for top-level language constructs inside the code
chunk (instead of 0).
REST is currently unused."))

;;;; prog-first-column

(unless (fboundp 'prog-first-column)
  (defun prog-first-column ()
    "Return the indentation column normally used for top-level constructs."
    (or (car prog-indentation-context) 0)))

;;;; smie-ident-bob

(when (version< emacs-version "26.1")
  (require 'smie)
  (defun smie-indent-bob ()
  ;; Start the file at column 0.
  (save-excursion
    (forward-comment (- (point)))
    (if (bobp) (prog-first-column)))))

;;;; c-update-modeline

(require 'cc-cmds)

(defun submode-compat--c-update-modeline ()
  (when (and mode-name
             (not (eq mode-name t)))
    (let ((fmt (format "/%s%s%s%s"
                       (if c-electric-flag "l" "")
                       (if (and c-electric-flag c-auto-newline)
                           "a" "")
                       (if c-hungry-delete-key "h" "")
                       (if (and
                            ;; (cc-)subword might not be loaded.
                            (boundp 'c-subword-mode)
                            (symbol-value 'c-subword-mode))
                           ;; FIXME: subword-mode already comes with its
                           ;; own lighter!
                           "w"
                         ""))))
      (setq mode-name
            (cond ((stringp mode-name)
                   (concat (if (string-match "\\(^[^/]*\\)/" mode-name)
                               (match-string 1 mode-name)
                             mode-name)
                           (when (> (length fmt) 1)
                             fmt)))
                  ((listp mode-name)
                   (let ((rname (reverse mode-name)))
                     (if (and (stringp (caar rname))
                              (eq ?/ (string-to-char (caar rname))))
                         (if (> (length fmt) 1)
                             (nreverse (cons (list fmt) (cdr rname)))
                           (nreverse (cdr rname)))
                       (if (> (length fmt) 1)
                           (nreverse (cons (list fmt) rname))
                         (nreverse rname)))))
                  (t mode-name)))
      (force-mode-line-update))))

(advice-add 'c-update-modeline :around
            (lambda (cc-fun &rest args)
              (if submode-compat--buffer-is-main-mode
                  (apply 'submode-compat--c-update-modeline args)
                (apply cc-fun args))))

(provide 'submode-compat)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; submode-compat.el ends here

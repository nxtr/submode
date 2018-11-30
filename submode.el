;;; submode.el --- Manage multiple major modes as submodes  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Magnus Nyberg

;; Author: Magnus Nyberg <magnus@nexter.se>
;; Keywords: utilities, convenience, faces, languages, tools
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

;; Utilities for a major mode to manage multiple major modes as submodes
;; in the same buffer.

;; This library is inspired from the mhtml-mode introduced in Emacs 26.1.

;; Two main functions are introduced `submode-construct-submode'
;; and `submode-construct-main-mode'.

;;;; `submode-construct-submode'

;; Constructs a submode from a major mode with a number of keywords
;; specifying how the major mode will be applied and operate as a submode.

;; Example:

;; (defconst *--the-major-mode-as-submode
;;   (submode-construct-submode
;;    'the-major-mode
;;    :name "(The Major Mode)"
;;    :relative-indent 'block
;;    :syntax-propertize-rules
;;    (syntax-propertize-precompile-rules
;;     ("<start-tag>"
;;      (0 (ignore
;;          (goto-char (match-end 0))
;;          (submode-syntax-propertize *--the-major-mode-as-submode end)))))
;;    :end-tag "<end-tag>"
;;    :syntax-table the-major-mode-syntax-table
;;    :propertize #'the-major-mode-syntax-propertize
;;    :keymap the-major-mode-map))

;; The function will apply the mode and capture buffer-local variables.

;;;; `submode-construct-main-mode'

;; Constructs a main-mode with a colletion of submodes and a number of
;; keywords specifying how submodes will be applied and indented etc.

;; This function is supposed to be called in the corresponding major
;; mode hook to activate the major mode as main mode.

;; Example:

;; (add-hook 'the-main-major-mode-hook
;;  (lambda ()
;;    (submode-construct-main-mode
;;     :name '("The Main Major Mode" (:eval (submode-lighter)))
;;     :indent-basic c-basic-offset
;;     :submodes '(*--the-major-mode-as-submode))))

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'submode-compat))

(cl-defstruct submode--named-init-config
  ;; Name of this mode.  Format according to `mode-line-format'.
  name
  ;; Code to run before construction.
  init
  ;; Code to run after construction.
  config)

(defconst submode--crucial-variable-prefix
  '("comment-" "uncomment-" "electric-indent-" "smie-"
    "forward-sexp-function" "completion-" "major-mode")
  "Regexp matching the prefix of \"crucial\" buffer-locals we want to capture.")

(defconst submode--variable-prefix
  '("font-lock-" "indent-line-function")
  "Regexp matching the prefix of buffer-locals we want to capture.")

(cl-defstruct (submode (:include submode--named-init-config))
  ;; How submode bodies are indented relative to the start-tag position.
  relative-indent
  ;; Syntax propertize rules for start-tag.
  syntax-propertize-rules
  ;; Regexp for submode end-tag.
  end-tag
  ;; Syntax table.
  syntax-table
  ;; Propertize function.
  propertize
  ;; Keymap.
  keymap
  ;; Regexp matching the prefix of \"crucial\" buffer-locals we want to capture
  ;; in addition to `submode--crucial-variable-prefix'.
  crucial-variable-prefix
  ;; Captured locals that are set when entering a region.
  crucial-captured-locals
  ;; Regexp matching the prefix of buffer-locals we want to capture
  ;; in addition to `submode--variable-prefix'.
  variable-prefix
  ;; Other captured local variables; these are not set when entering a
  ;; region but let-bound during certain operations, e.g. indentation.
  captured-locals)

(defun submode-construct-submode (mode &rest args)
  "Constructs a submode from a major mode.
This function computes the buffer-local variables and executes
any init and config code."
  (let ((captured-locals nil)
        (crucial-captured-locals nil)
        (submode (apply #'make-submode args)))
    (with-temp-buffer
      ;; Execute init body.
      (funcall (submode--body-function (submode-init submode)))
      ;; Apply mode.
      (funcall mode)
      ;; Make sure font lock is all set up.
      (font-lock-set-defaults)
      ;; This has to be set to a value other than the main-mode value,
      ;; to avoid recursion.
      (unless (variable-binding-locus 'font-lock-fontify-region-function)
        (setq-local font-lock-fontify-region-function
                    #'font-lock-default-fontify-region))
      ;; Execute config body.
      (funcall (submode--body-function (submode-config submode)))
      ;; Capture local variables.
      (let ((crucial-variable-prefix
             (regexp-opt (append submode--crucial-variable-prefix
                                 (submode-crucial-variable-prefix submode))))
            (variable-prefix
             (regexp-opt (append submode--variable-prefix
                                 (submode-variable-prefix submode)))))
        (dolist (iter (buffer-local-variables))
          (when (string-match crucial-variable-prefix (symbol-name (car iter)))
            (push iter crucial-captured-locals))
          (when (string-match variable-prefix (symbol-name (car iter)))
            (push iter captured-locals))))
      (setf (submode-crucial-captured-locals submode)
            crucial-captured-locals)
      (setf (submode-captured-locals submode) captured-locals))
    submode))

(cl-defstruct (submode-main-mode (:include submode--named-init-config))
  ;; Specifies the indentation for submode `relative-indent'.
  indent
  ;; Function for calculating indentation.
  calc-indent
  ;; Non-nil if `syntax-propertize-function' searches and
  ;; matches should ignore case.
  case-fold-search
  ;; Function called before call of `syntax-propertize-rules-function'
  ;; and `main-mode-syntax-propertize-function' in main-mode.
  pre-syntax-propertize
  ;; Syntax propertize rules for main-mode.
  syntax-propertize-rules
  ;; List of submodes.
  submodes)

(defun submode--body-function (body)
  (when (or (and (consp body) (symbolp (car body)))
            (not (consp body)))
    (setq body (list body)))
  `(lambda ()
     ,@body))

(defvar-local submode--main-mode nil
  "The constructed main-mode.")

(defvar-local submode--main-mode-indent-line-function nil
  "The captured major mode `indent-line-function'.")

(defvar-local submode--main-mode-syntax-propertize-function nil
  "The captured major mode `syntax-propertize-function'.")

(defvar-local submode--syntax-propertize-function nil
  "The constructed `syntax-propertize-function' for main-mode.")

(defvar-local submode--crucial-variables nil
  "List of all crucial variable symbols.")

(defun submode-construct-main-mode (&rest args)
  "Constructs a main-mode with a colletion of submodes.
This function is supposed to be called in the corresponding major mode hook
and will capture essential functions and variables."
  (let ((main-mode (apply #'make-submode-main-mode args)))
    (setq submode--main-mode main-mode)
    ;; Execute init body.
    (funcall (submode--body-function (submode-main-mode-init main-mode)))
    ;; Update mode-name.
    (setq mode-name (submode-main-mode-name main-mode))
    ;; Really run `indent-according-to-mode' on each line
    ;; instead of a submode unaware `indent-region-function',
    ;; e.g. `c-indent-region' for `cc-mode' based main-modes.
    ;; FIXME replace with `indent-region-function' that splits
    ;; the region at mode boundry and indent sub-regions accordingly.
    (setq-local indent-region-function nil)
    ;; Capture and reassign `indent-line-function'.
    (setq submode--main-mode-indent-line-function indent-line-function)
    (setq-local indent-line-function #'submode--indent-line)
    ;; Capture and reassign `syntax-propertize-function'.
    (setq submode--main-mode-syntax-propertize-function
          syntax-propertize-function)
    (setq-local syntax-propertize-function #'submode--syntax-propertize)
    ;; Iterate submodes.
    (let ((submodes (submode-main-mode-submodes main-mode))
          ;; Start with the provided main-mode rules, if any.
          (newrules (submode-main-mode-syntax-propertize-rules main-mode)))
      (setq submodes (cond ((and submodes (atom submodes)) (cons submodes nil))
                           ((listp submodes) (reverse submodes))))
      (while submodes
        (let ((submode (pop submodes)))
          (setq submode (cond ((and (symbolp submode) (boundp submode))
                               (symbol-value submode))
                              ((consp submode) (symbol-value (eval submode)))
                              (t submode)))
          (unless (submode-p submode)
            (error "Wrong type for keyword submodes: %s, %s"
                   'submode-p submode))
          ;; Make captured variables buffer-local.
          (submode--mark-buffer-locals submode)
          (submode--mark-crucial-buffer-locals submode)
          ;; Add submode rules.
          (let ((rules (submode-syntax-propertize-rules submode)))
            (setq rules (nreverse rules))
            (while rules
              (push (pop rules) newrules)))))
      (when newrules
        ;; Make main-mode `syntax-propertize-function'.
        (setq submode--syntax-propertize-function
              (macroexpand-1
               `(syntax-propertize-rules
                 ,@newrules)))))
    ;; Capture font-lock-* variabels..
    (setq-local font-lock-fontify-region-function #'submode--fontify-region)
    (setq-local font-lock-extend-region-functions
                '(submode--extend-font-lock-region))
    ;; Attach this to both pre- and post- hooks just in case
    ;; it ever changes a key binding that might be accessed
    ;; from the menu bar.
    (add-hook 'pre-command-hook #'submode--pre-command nil t)
    (add-hook 'post-command-hook #'submode--pre-command nil t)
    ;; Save crucial variables.
    (setq submode--crucial-variables (delete-dups submode--crucial-variables))
    ;; For compat, make sure this buffer is main-mode.
    (setq submode-compat--buffer-is-main-mode submode--main-mode)
    ;; Execute config body.
    (funcall (submode--body-function (submode-main-mode-config main-mode)))
    ;; Make the syntax machinery discard all information.
    (syntax-ppss-flush-cache -1)
    ;; Re-fontify the buffer.
    (save-excursion
      (font-lock-fontify-region (point-min) (point-max)))))

(defun submode--mark-buffer-locals (submode)
  (dolist (iter (submode-captured-locals submode))
    (make-local-variable (car iter))))

(defun submode--mark-crucial-buffer-locals (submode)
  (dolist (iter (submode-crucial-captured-locals submode))
    (make-local-variable (car iter))
    (push (car iter) submode--crucial-variables)))

(defmacro submode--with-locals (submode &rest body)
  (declare (debug t) (indent 1))
  `(cl-progv (when ,submode
               (mapcar #'car (submode-captured-locals ,submode)))
       (when ,submode
         (mapcar #'cdr (submode-captured-locals ,submode)))
     (cl-progv (when ,submode
                 (mapcar #'car (submode-crucial-captured-locals ,submode)))
         (when ,submode
           (mapcar #'cdr (submode-crucial-captured-locals ,submode)))
       ,@body)))

(defun submode-lighter ()
  "Mode-line lighter indicating the current submode."
  ;; The end of the buffer has no text properties, so
  ;; in this case back up one character, if possible.
  (let* ((where (if (and (eobp) (not (bobp)))
                    (1- (point))
                  (point)))
         (submode (get-text-property where 'submode)))
    (if submode
        (submode-name submode)
      nil)))

(defvar font-lock-beg)
(defvar font-lock-end)

(defun submode--extend-font-lock-region ()
  "Extend the font lock region according to submode needs.

This is used via `font-lock-extend-region-functions'.  It ensures that the
font-lock region is extended to cover either whole lines, or to the spot where
the submode changes, whichever is smallest."
  (let ((orig-beg font-lock-beg)
        (orig-end font-lock-end))
    ;; The logic here may look odd but it is needed to ensure that
    ;; we do the right thing when trying to limit the search.
    (save-excursion
      (goto-char font-lock-beg)
      ;; `previous-single-property-change' starts by looking
      ;; at the previous character, but we're trying to extend
      ;; a region to include just characters with the same
      ;; submode as this character.
      (unless (eobp)
        (forward-char))
      (setq font-lock-beg (previous-single-property-change
                           (point) 'submode nil (line-beginning-position)))
      (unless (eq (get-text-property font-lock-beg 'submode)
                  (get-text-property orig-beg 'submode))
        (cl-incf font-lock-beg))

      (goto-char font-lock-end)
      (unless (bobp)
        (backward-char))
      (setq font-lock-end (next-single-property-change
                           (point) 'submode nil (line-beginning-position 2)))
      (unless (eq (get-text-property font-lock-end 'submode)
                  (get-text-property orig-end 'submode))
        (cl-decf font-lock-end)))
    ;; Also handle the multiline property -- but handle it here,
    ;; and not via `font-lock-extend-region-functions', to avoid
    ;; the situation where the two extension functions disagree.
    ;; See bug#29159.
    (font-lock-extend-region-multiline)
    (or (/= font-lock-beg orig-beg)
        (/= font-lock-end orig-end))))

(defun submode--fontify-one-region (submode beg end &optional loudly)
  (if submode
      (submode--with-locals submode
        (save-restriction
          (font-lock-fontify-region beg end loudly)))
    (font-lock-set-defaults)
    (font-lock-default-fontify-region beg end loudly)))

(defun submode--fontify-region (beg end loudly)
  (syntax-propertize end)
  (let ((orig-beg beg)
        (orig-end end)
        (new-beg beg)
        (new-end end))
    (while (< beg end)
      (let ((submode (get-text-property beg 'submode))
            (this-end (next-single-property-change beg 'submode nil end)))
        (let ((extended (submode--fontify-one-region submode beg this-end
                                                     loudly)))
          ;; If the call extended the region, take note.  We track the
          ;; bounds we were passed and take the union of any extended
          ;; bounds.
          (when (and (consp extended) (eq (car extended) 'jit-lock-bounds))
            (setq new-beg (min new-beg (cadr extended)))
            ;; Make sure that the next region starts where the extension
            ;; of this region ends.
            (setq this-end (cddr extended))
            (setq new-end (max new-end this-end))))
        (setq beg this-end)))
    (when (or (/= orig-beg new-beg)
              (/= orig-end new-end))
      (cons 'jit-lock-bounds (cons new-beg new-end)))))

(defvar-local submode--stashed-crucial-variables nil
  "Alist of stashed values of the crucial variables.")

(defun submode--stash-crucial-variables ()
  (setq submode--stashed-crucial-variables
        (mapcar (lambda (sym)
                  (cons sym (buffer-local-value sym (current-buffer))))
                submode--crucial-variables)))

(defun submode--map-in-crucial-variables (alist)
  (dolist (item alist)
    (set (car item) (cdr item))))

(defvar-local submode--last-submode nil
  "Record the last visited submode.
This is used by `submode--pre-command'.")

(defun submode--pre-command ()
  (let ((submode (get-text-property (point) 'submode)))
    (unless (eq submode submode--last-submode)
      ;; If we're entering a submode, and the previous submode was
      ;; nil, then stash the current values first.  This lets the user
      ;; at least modify some values directly.  FIXME maybe always
      ;; stash into the current mode?
      (when (and submode (not submode--last-submode))
        (submode--stash-crucial-variables))
      (submode--map-in-crucial-variables
       (if submode
           (submode-crucial-captured-locals submode)
         submode--stashed-crucial-variables))
      (setq submode--last-submode submode))))

(defun submode-syntax-propertize (submode end &optional tag-start)
  (save-excursion
    (let ((case-fold-search (submode-main-mode-case-fold-search
                             submode--main-mode)))
      (when (search-forward (submode-end-tag submode) end t)
        (setq end (match-beginning 0)))))
  (set-text-properties (point) end
                       (list 'submode submode
                             'tag-start tag-start
                             'syntax-table (submode-syntax-table submode)
                             ;; We want local-map here so that we act
                             ;; more like the submode and don't
                             ;; override minor mode maps.
                             'local-map (submode-keymap submode)))
  (let ((submode-propertize (submode-propertize submode)))
    (when submode-propertize
      (funcall submode-propertize (point) end)))
  (goto-char end))

(defun submode--syntax-propertize (start end)
  (when submode--main-mode
    ;; First remove our special settings from the affected text.
    ;; They will be re-applied as needed.
    (remove-list-of-text-properties
     start end '(submode tag-start syntax-table local-map))
    (goto-char start)
    ;; Be sure to look back one character, because START won't yet have
    ;; been propertized.
    (unless (bobp)
      (let ((submode (get-text-property (1- (point)) 'submode)))
        (if submode
            (submode-syntax-propertize
             submode end (get-text-property (1- (point)) 'tag-start))
          ;; No submode, so call the main-mode
          ;; `pre-syntax-propertize'.
          (let ((pre-syntax-propertize (submode-main-mode-pre-syntax-propertize
                                        submode--main-mode)))
            (when pre-syntax-propertize
              (funcall pre-syntax-propertize start end))))))
    (let ((case-fold-search (submode-main-mode-case-fold-search
                             submode--main-mode))
          ;; Make sure to handle the situation where
          ;; `submode-syntax-propertize' moved point.
          (pos (point)))
      (when submode--syntax-propertize-function
        (funcall submode--syntax-propertize-function pos end))
      (unless (submode-main-mode-syntax-propertize-rules submode--main-mode)
        ;; Main-mode rules was not added, so call the main-mode
        ;; `syntax-propertize-function'.
        (when submode--main-mode-syntax-propertize-function
          (funcall submode--main-mode-syntax-propertize-function pos end))))))

(defun submode--calc-indent ()
  "Calculate indentation."
  (let ((calc-indent (when submode--main-mode
                       (submode-main-mode-calc-indent
                        submode--main-mode)))
        (indent (when submode--main-mode
                  (submode-main-mode-indent submode--main-mode)))
        (indent-line (if submode--main-mode-indent-line-function
                         submode--main-mode-indent-line-function
                       indent-line-function))
        (inhibit-modification-hooks t))
    (cond (calc-indent (save-excursion
                         (funcall calc-indent)))
          (indent (+ (current-column) indent))
          (indent-line (save-excursion
                         (let ((offset (- (point)
                                          (progn
                                            (back-to-indentation)
                                            (point)))))
                           (let ((curr (current-indentation)))
                             (prog2
                                 (funcall indent-line)
                                 (current-indentation)
                               (indent-line-to curr)
                               (when (> offset 0)
                                 (forward-char offset)))))))
          (t 0))))

(defvar-local submode-region-start nil
  "Submode (region) start position.
For use in a customized buffer `calc-indent' function
or `indent-line-function'.")

(defun submode--indent-line ()
  "Indent the current line as main or submode according to its context."
  (interactive)
  (let ((submode (save-excursion
                   (back-to-indentation)
                   (get-text-property (point) 'submode))))
    (if submode
        (let* ((submode-region-start
                (or (save-excursion
                      (back-to-indentation)
                      (previous-single-property-change (point) 'submode))
                    (point)))
               (tag-start (get-text-property (point) 'tag-start))
               (indent (submode-main-mode-indent submode--main-mode))
               (first-column (save-excursion
                               (pcase (submode-relative-indent submode)
                                 ('block (when indent
                                           (goto-char submode-region-start)
                                           (back-to-indentation))
                                         (submode--calc-indent))
                                 ('t (cond
                                      ((and tag-start indent)
                                       (goto-char tag-start))
                                      (indent (goto-char submode-region-start)))
                                     (submode--calc-indent))
                                 ('nil (cond
                                        (tag-start (goto-char tag-start)
                                                   (current-column))
                                        (indent
                                         (- (progn
                                              (goto-char submode-region-start)
                                              (submode--calc-indent))
                                            indent))
                                        (t (submode--calc-indent))))
                                 ('ignore 0)
                                 (e (error "Unknown :relative-indent %S" e))))))
          (save-restriction
            (let ((pos (point))
                  (region-start-col (save-excursion
                                      (goto-char submode-region-start)
                                      (current-column))))
              (narrow-to-region submode-region-start (point-max))
              (let ((inhibit-modification-hooks t))
                ;; Insert leading spaces for for lineup
                (goto-char submode-region-start)
                (insert-char ?\s region-start-col)
                (goto-char (+ pos region-start-col)))
              ;; Indent line according to submode
              (let ((prog-indentation-context (cons first-column nil)))
                (submode--with-locals submode
                  ;; `indent-line-function' was rebound
                  ;; by submode--with-locals.
                  (when indent-line-function
                    (funcall indent-line-function))))
              ;; Check submode support of `prog-first-column'.
              (when (and (save-excursion
                           (back-to-indentation)
                           (bolp))
                         (/= 0 first-column))
                ;; No support.
                (let ((offset (- (point)
                                 (save-excursion
                                   (back-to-indentation)
                                   (point)))))
                  ;; Fake it!
                  (indent-line-to first-column)
                  (when (> offset 0)
                    (forward-char offset))))
              (let ((inhibit-modification-hooks t))
                ;; Delete leading spaces
                (setq pos (point))
                (goto-char submode-region-start)
                (delete-char region-start-col)
                (goto-char (- pos region-start-col))))))
      ;; Indent according to main-mode.
      (when submode--main-mode-indent-line-function
        (funcall submode--main-mode-indent-line-function)))))

(provide 'submode)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; submode.el ends here

;;; helm-org-ql.el --- Helm commands for org-ql  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/org-ql

;;; Commentary:

;; This library includes Helm commands for `org-ql'.  Note that Helm
;; is not declared as a package dependency, so this does not cause
;; Helm to be installed.  In the future, this file may have its own
;; package recipe, which would allow it to be installed separately and
;; declare a dependency on Helm.

;;; License:

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

;;; Code:

;;;; Requirements

(require 'org)

(require 'org-ql)

;; (require 'helm)
;; (require 'helm-org)

;;;; Compatibility

;; Declare Helm functions since Helm may not be installed.
(declare-function helm "ext:helm")
(declare-function helm-run-after-exit "ext:helm")
(declare-function helm-window "ext:helm-lib")
(declare-function helm-buffer-get "ext:helm-lib")
(declare-function helm-make-source "ext:helm-source")
(declare-function helm-org-goto-marker "ext:helm-org")

;; Silence byte-compiler about variables.
(defvar helm-map)
(defvar helm-pattern)
(defvar helm-input-idle-delay)

;;;; Variables

(defvar helm-org-ql-map
  (let ((map (copy-keymap helm-map))
        (mappings '(
                    "C-x C-s" helm-org-ql-save
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map)
  "Keymap for `helm-org-ql' sessions.
Based on `helm-map'.")

(defvar-local helm-org-ql-buffers-files nil
  "Used for `helm-org-ql-save'.")

;;;; Customization

(defgroup helm-org-ql nil
  "Options for `helm-org-ql'."
  :group 'org-ql)

(defcustom helm-org-ql-reverse-paths t
  "Whether to reverse Org outline paths in `helm-org-ql' results."
  :type 'boolean)

(defcustom helm-org-ql-input-idle-delay 0.25
  "Seconds to wait after typing stops before running query."
  :type 'number)

(defcustom helm-org-ql-actions
  (list (cons "Show heading in source buffer" 'helm-org-ql-show-marker)
        (cons "Show heading in indirect buffer" 'helm-org-ql-show-marker-indirect))
  "Alist of actions for `helm-org-ql' commands."
  :type '(alist :key-type (string :tag "Description")
                :value-type (function :tag "Command")))

;;;; Commands

;;;###autoload
(cl-defun helm-org-ql (buffers-files
                       &key (boolean 'and) (name "helm-org-ql"))
  "Display results in BUFFERS-FILES for an `org-ql' query using Helm.
Interactively, search the current buffer.

NOTE: Atoms in the query are turned into strings where
appropriate, which makes it unnecessary to type quotation marks
around words that are intended to be searched for as indepenent
strings.

All query tokens are wrapped in the operator BOOLEAN (default
`and'; with prefix, `or').

For example, this raw input:

    Emacs git

Is transformed into this query:

    (and \"Emacs\" \"git\")

However, quoted strings remain quoted, so this input:

    \"something else\" (tags \"funny\")

Is transformed into this query:

    (and \"something else\" (tags \"funny\"))"
  (interactive (list (current-buffer)))
  (let ((boolean (if current-prefix-arg 'or boolean))
        (helm-input-idle-delay helm-org-ql-input-idle-delay))
    (helm :prompt (format "Query (boolean %s): " (-> boolean symbol-name upcase))
          :sources
          ;; Expansion of `helm-build-sync-source' macro.
          (helm-make-source name 'helm-source-sync
            :candidates (lambda nil
                          (let* ((query (org-ql--plain-query helm-pattern boolean))
                                 (window-width (window-width (helm-window))))
                            (when query
                              (with-current-buffer (helm-buffer-get)
                                (setq helm-org-ql-buffers-files buffers-files))
                              (ignore-errors
                                ;; Ignore errors that might be caused by partially typed queries.

                                ;; FIXME: This doesn't prevent warnings that are errors occurring during
                                ;; byte-compilation due to partially typed values which can't be correctly
                                ;; pre-processed, e.g. "ts:to=2019-01-0", which can't be parsed into a
                                ;; timestamp.  A "*Compile-Log*" buffer is displayed with "Error: Wrong type
                                ;; argument: integerp, nil".  With my Helm settings, it's hidden as soon as
                                ;; the query is typed correctly, so it's tolerable, but I'd prefer to fix it.
                                ;; I haven't found a way to ignore the error/warning; `with-no-warnings' has
                                ;; no effect, and we're already using `ignore-errors'.  The only solution I
                                ;; can think of would be to ignore the errors/warnings higher up the chain
                                ;; where byte-compilation is actually done, but it might not be a good idea
                                ;; to always ignore such errors/warnings.
                                (org-ql-select buffers-files query
                                  :action (list 'helm-org-ql--heading window-width))))))
            :match #'identity
            :fuzzy-match nil
            :multimatch nil
            :volatile t
            :keymap helm-org-ql-map
            :action helm-org-ql-actions))))

;;;###autoload
(defun helm-org-ql-agenda-files ()
  "Search agenda files with `helm-org-ql', which see."
  (interactive)
  (helm-org-ql (org-agenda-files) :name "helm-org-ql-agenda-files"))

;;;###autoload
(defun helm-org-ql-org-directory ()
  "Search Org files in `org-directory' with `helm-org-ql'."
  (interactive)
  (helm-org-ql (directory-files org-directory 'full
                                (rx ".org" eos))
               :name "helm-org-ql-org-directory"))

(defun helm-org-ql-show-marker (marker)
  "Show heading at MARKER."
  (interactive)
  ;; This function is necessary because `helm-org-goto-marker' calls
  ;; `re-search-backward' to go backward to the start of a heading,
  ;; which, when the marker is already at the desired heading, causes
  ;; it to go to the previous heading.  I don't know why it does that.
  (switch-to-buffer (marker-buffer marker))
  (goto-char marker)
  (org-show-entry))

(defun helm-org-ql-show-marker-indirect (marker)
  "Show heading at MARKER with `org-tree-to-indirect-buffer'."
  (interactive)
  (helm-org-ql-show-marker marker)
  (org-tree-to-indirect-buffer))

(defun helm-org-ql-save ()
  "Show `helm-org-ql' search in an `org-ql-search' buffer."
  (interactive)
  (let ((buffers-files (with-current-buffer (helm-buffer-get)
                         helm-org-ql-buffers-files))
        (query (org-ql--plain-query helm-pattern)))
    (helm-run-after-exit #'org-ql-search buffers-files query)))

;;;; Functions

;; NOTE: This function is superseded by the `peg' implementation in
;; `org-ql--input-query', but I'm leaving it here for now.
;; FIXME: Delete commented code before merging.

;; (defun helm-org-ql--input-to-query (input &optional no-and)
;;   "Return `org-ql' query sexp for string INPUT.
;; Unless NO-AND is non-nil (interactively, with prefix), all query
;; tokens are wrapped in an implied (and) form, and plain
;; symbols (except at the beginning of a sexp) are replaced with
;; strings."
;;   (unless (s-blank-str? input)
;;     (setf input (format "(%s %s)" (if no-and "or" "and") input))
;;     (when-let* ((query (ignore-errors
;;                          ;; Ignore errors in case input is an
;;                          ;; incomplete string or sexp.
;;                          (read input))))
;;       (cl-labels ((rec (form)
;;                        ;; Replace some symbols with strings so users don't
;;                        ;; have to type quotation marks around all strings.
;;                        ;; Not perfect, but should be more useful.
;;                        (pcase-exhaustive form
;;                          ((and (pred atom)
;;                                (let s (symbol-name form))
;;                                (guard (string-match (rx (group (or "clocked" "closed" "planning" "deadline" "scheduled"
;;                                                                    "ts" "ts-active" "ts-inactive" "ts-a" "ts-i"))
;;                                                         ":" (group (0+ anything)))
;;                                                     s))
;;                                (let type (intern (match-string 1 s)))
;;                                (let arg (match-string 2 s)))
;;                           ;; Timestamp-based predicates.  NOTE: Times are not supported, only dates.
;;                           (pcase arg
;;                             ;; The `pcase' `rx let' form is so helpful here!
;;                             ((rx bos eos)
;;                              ;; No date.
;;                              `(,type))
;;                             ((rx bos (let on (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit)) eos)
;;                              ;; One date: :on
;;                              `(,type :on ,on))
;;                             ((rx bos (let from (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit))
;;                                  (repeat 1 2 "-") eos)
;;                              ;; One date: :from
;;                              `(,type :from ,from))
;;                             ((rx bos (repeat 1 2 "-")
;;                                  (let to (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit)) eos)
;;                              ;; One date: :to
;;                              `(,type :to ,to))
;;                             ((rx (let from (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit))
;;                                  "--"
;;                                  (let to (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit))
;;                                  eos)
;;                              ;; Two dates: :from :to.
;;                              `(,type :from ,from :to ,to))))
;;
;;                          ((and (pred atom)
;;                                (let s (symbol-name form))
;;                                (guard (string-match (rx "todo:" (group (0+ anything))) s))
;;                                (let arg (match-string 1 s)))
;;                           ;; To-do predicates.
;;                           (pcase arg
;;                             ;; The `pcase' `rx let' form is so helpful here!
;;                             ((rx bos eos)
;;                              ;; No keyword.
;;                              `(todo))
;;                             ((rx bos (1+ (not (in "|"))) eos)
;;                              ;; One keyword.
;;                              `(todo ,arg))
;;                             ((rx "|")
;;                              ;; Multiple keywords.
;;                              `(todo ,@(s-split "|" arg)))))
;;
;;                          ((and (pred atom)
;;                                (let s (symbol-name form))
;;                                (guard (string-match (rx (group (or "done" "habit")) ":") s)))
;;                           ;; Predicates that take no arguments.
;;                           `(,(intern (match-string 1 s))))
;;
;;                          ((and (pred atom)
;;                                (let s (symbol-name form))
;;                                ;; FIXME: tags& shouldn't use "|" as a separator because it's AND not OR.
;;                                (guard (string-match (rx (group (or "category" "heading" "path" "regexp"
;;                                                                    "tags" "tags-all" "tags&"
;;                                                                    "tags-inherited" "itags"
;;                                                                    "tags-local" "ltags"
;;                                                                    "todo"))
;;                                                         ":" (group (0+ anything)))
;;                                                     s))
;;                                (let type (intern (match-string 1 s)))
;;                                (let args (match-string 2 s)))
;;                           ;; Predicates that take no or one-or-more same-type arguments.
;;                           (pcase args
;;                             ((rx bos eos)
;;                              ;; No keyword.
;;                              `(,type))
;;                             ((rx bos (1+ (not (in ":"))) eos)
;;                              ;; One keyword.
;;                              `(,type ,args))
;;                             ((rx ":")
;;                              ;; Multiple keywords.
;;                              `(,type ,@(s-split ":" args)))))
;;
;;                          ((pred stringp) form)
;;                          (`(deadline auto) form)
;;                          ((or '> '>= '< '<= '=)
;;                           ;; Comparators, probably for (priority).
;;                           form)
;;                          ((guard (string-match (rx bos ":" (1+ anything) ":" eos)
;;                                                (prin1-to-string form)))
;;                           ;; An Org tag, not a Lisp keyword.
;;                           (prin1-to-string form))
;;                          ((pred keywordp) form)
;;                          ((pred numberp) form)
;;                          ((guard (string-prefix-p "!" (prin1-to-string form)))
;;                           ;; Negation of a string.
;;                           `(not ,(substring (prin1-to-string form) 1)))
;;                          ((pred atom) (prin1-to-string form))
;;                          ((pred listp) `(,(car form)
;;                                          ,@(mapcar #'rec (cdr form)))))))
;;         (rec query)))))

(defun helm-org-ql--heading (window-width)
  "Return string for Helm for heading at point.
WINDOW-WIDTH should be the width of the Helm window."
  (font-lock-ensure (point-at-bol) (point-at-eol))
  ;; TODO: It would be better to avoid calculating the prefix and width
  ;; at each heading, but there's no easy way to do that once in each
  ;; buffer, unless we manually called `org-ql' in each buffer, which
  ;; I'd prefer not to do.  Maybe I should add a feature to `org-ql' to
  ;; call a setup function in a buffer before running queries.
  (let* ((prefix (concat (buffer-name) ":"))
         (width (- window-width (length prefix)))
         (path (org-split-string (org-format-outline-path (org-get-outline-path)
                                                          width nil "")
                                 ""))
         (heading (org-get-heading t))
         (path (if helm-org-ql-reverse-paths
                   (concat heading "\\" (s-join "\\" (nreverse path)))
                 (concat (s-join "/" path) "/" heading))))
    (cons (concat prefix path) (point-marker))))

;;;; Footer

(provide 'helm-org-ql)

;;; helm-org-ql.el ends here

;;; casease.el --- ease for cases
;;; -*- lexical-binding: t -*-

;; Author: Shi Tianshu
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (dash "2.12.0") (cl-lib "0.6.1"))
;; Version: 1.0.0
;; URL: https://www.github.com/DogLooksGood/casease
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This minor mode help input symbols in variant cases without pressing the shift key.
;; By inserting a prefix char(see `casease-entry-alist'), your input will be converted to
;; corresponding case automatically. The words can always be separated by whitespaces.
;;
;; The conversion will stop once your cursor leaving the highlight or you give a non alphanum input.
;;
;; Usage:
;;   1. configure entry prefix by customize `casease-entry-alist'.
;;   2. enable caseease-mode.
;;
;; Currently, four cases are supported:
;; - snake_case
;; - PascalCase
;; - camelCase
;; - SCREAM_CASE
;; - kebab-case
;;
;; Performance:
;;   casease add hook to `post-self-insert-hook', the cost for hook function is depending on how complex the rules are, but usually less than 0.1ms.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)

(defvar-local casease-entry-alist nil
  "The entry keys for enabled cases.

Set different values for different buffers.")

(defvar-local casease-separator-key ?,
  "The key used for word separator.")

(defvar-local casease-input-keys
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "Keys to keep us stay in casease state.")

(defvar-local casease-extending-commands '(self-insert-command)
  "Commands to extending conversion area.")

(defvar casease--first-conversion nil
  "Indicating we don't need post-command-hook function when just we started.")

(defvar casease--last-last-input-event nil
  "The previous `last-input-event'.")

(defvar-local casease--activate nil
  "Current casease activate state, nil means disabled.

The enabled value can be one of snake, kebab, camel, screaming and pascal.")

(defvar-local casease--overlay nil
  "The overlay used to indicate the conversion area.")

(defface casease-face
  '((((class color) (background dark))
     (:underline t))
    (((class color) (background light))
     (:underline t)))
  "Casease overlay face.")

(defun casease--make-overlay (beg end)
  "Create overlay from BEG to END.

Saved to `casease--overlay'."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'casease-face)
    (setq casease--overlay ov)))

(defun casease--extend-overlay ()
  "Extend `casease--overlay' to current position."
  (let ((beg (if casease--overlay
                 (overlay-start casease--overlay)
               (1- (point))))
        (end (point)))
    (when casease--overlay (delete-overlay casease--overlay))
    (casease--make-overlay beg end)))

(defun casease--start (activate-case)
  (message "Casease: %s" activate-case)
  (when-let ((beg (match-beginning 1)))
    (delete-region beg (match-end 1)))
  (let ((beg (match-beginning 0))
        (end (point)))
    (casease--make-overlay beg end)
    (add-hook 'post-command-hook #'casease--post-command-function nil t)
    (setq casease--activate activate-case
          casease--first-conversion t)

    ;; cases specific initialization
    (cl-case activate-case
      ((screaming) (casease--init-screaming))
      ((pascal) (casease--init-pascal)))))

(defun casease--extend-snake ()
  "Extend snake case conversion."
  (save-mark-and-excursion
    (let ((beg (overlay-start casease--overlay))
          (end (overlay-end casease--overlay)))
      (goto-char end)
      (while (and (> (point) beg) (equal (char-before) casease-separator-key))
        (delete-char -1)
        (insert "_")
        (forward-char -1)))))

(defun casease--extend-kebab ()
  "Extend kebab case conversion."
  (save-mark-and-excursion
    (let ((beg (overlay-start casease--overlay))
          (end (overlay-end casease--overlay)))
      (goto-char end)
      (while (and (> (point) beg) (equal (char-before) casease-separator-key))
        (delete-char -1)
        (insert "-")
        (forward-char -1)))))

(defun casease--extend-camel ()
  "Extend camel case conversion."
  (when (equal casease--last-last-input-event casease-separator-key)
    (delete-region (- (point) 2) (1- (point)))
    (upcase-char -1)))

(defun casease--init-pascal ()
  (let ((beg (overlay-start casease--overlay)))
    (save-mark-and-excursion
      (goto-char beg)
      (upcase-char 1))))

(defun casease--extend-pascal ()
  "Extend pascal case conversion."
  (when (equal casease--last-last-input-event casease-separator-key)
    (delete-region (- (point) 2) (1- (point)))
    (upcase-char -1))
  (upcase-region (overlay-start casease--overlay) (1+ (overlay-start casease--overlay))))

(defun casease--init-screaming ()
  (let ((beg (overlay-start casease--overlay))
        (end (overlay-end casease--overlay)))
    (goto-char beg)
    (while (< (point) end)
      (cond
       ((equal (char-after) casease-separator-key)
        (delete-char 1)
        (insert "_"))
       ((equal (char-after) ?-)
        (delete-char 1)
        (insert "_"))
       (t
        (upcase-char 1)
        (forward-char 1))))))

(defun casease--extend-screaming ()
  "Extend screaming case conversion."
  (let ((beg (overlay-start casease--overlay)))
    (cond
     ((equal (char-before) casease-separator-key)
      (delete-char -1)
      (insert "_"))
     ((equal (char-before) ?-)
      (delete-char -1)
      (insert "_"))
     (t
      (upcase-char -1)))))

(defun casease--end ()
  "End conversion."
  (remove-hook 'post-command-hook #'casease--post-command-function t)
  (when casease--overlay
    (delete-overlay casease--overlay))
  (setq casease--activate nil
        casease--overlay nil
        casease--last-last-input-event nil))

(defun casease--looking-back-prefix (the-case)
  (let ((case-fold-search nil))
    (when-let ((res (alist-get the-case casease-entry-alist)))
      (-any-p (lambda (re) (looking-back re 2)) res))))

(defun casease--post-self-insert-function ()
  "Hook function for `post-self-insert-hook'."
  (let* ((is-input-key (member last-input-event casease-input-keys)))
    (cond
     ;; ((and casease--activate (equal last-input-event casease-separator-key)))

     ((and casease--activate (not is-input-key))
      (casease--end))

     ((equal casease--activate 'snake)
      (casease--extend-snake))

     ((equal casease--activate 'kebab)
      (casease--extend-kebab))

     ((equal casease--activate 'camel)
      (casease--extend-camel))

     ((equal casease--activate 'pascal)
      (casease--extend-pascal))

     ((equal casease--activate 'screaming)
      (casease--extend-screaming))

     (t
      (let ((case-fold-search nil))
        (-some->> casease-entry-alist
          (-first
           (-lambda ((the-case . res))
             (-any-p (lambda (re) (looking-back re 2)) res)))
          car
          casease--start)))))
  (setq casease--last-last-input-event last-input-event))

(defun casease--post-command-function ()
  "Hook function for `post-command-hook'.

This is only available during the conversion."
  (if casease--first-conversion
      (setq casease--first-conversion nil)

    (when (member this-command casease-extending-commands)
        (casease--extend-overlay))

    (when (and casease--overlay
               (or (equal this-command 'keyboard-quit)
                   (let ((beg (overlay-start casease--overlay))
                         (end (overlay-end casease--overlay)))
                     (or (< (point) beg)
                         (> (point) end)
                         (= beg end)))))
      (casease--end))

    (unless (member this-command casease-extending-commands)
      (setq casease--last-last-input-event nil))))

(defun casease--init ()
  "Enable casease mode."
  (add-hook 'post-self-insert-hook #'casease--post-self-insert-function nil t))

(defun casease--uninit ()
  "Disable casease mode."
  (remove-hook 'post-self-insert-hook #'casease--post-self-insert-function t)
  (remove-hook 'post-command-hook #'casease--post-command-function t)
  (when casease--overlay
    (delete-overlay casease--overlay))
  (setq casease--activate nil
        casease--overlay nil
        casease--last-last-input-event nil))

(defmacro casease-setup (&rest args)
  "Setup casease with SEPARATOR, ENTRIES and HOOK.

:hook is the hook name that casease used for setup.

:separator is the key to separate words during input.
This is optional, when not provided, the default `casease-separator-key' will be used.
This separator will also be added to `casease-input-keys'.

:inputs is a list of keys, they are add to buffer local `casease-input-keys'.
The original keys contains [a-z][0-9], you don't have to specify.

:commands is a list of command names, those will extend the conversion area.
The original commands contains `self-insert-command', you don't have to specify.

:entries is the rules for starting conversion.

Example usage:
\(casease-setup
   :hook python-mode-hook
   :separator ?,
   :inputs (?/)
   :commands (cljr-slash)
   :entries
   ((pascal \"\\(,\\)[a-z]\")
    (camel \"(\\.\")
    (kebab \"[a-z]\")))


In this example, we will use kebab case by default, enable pascal case with prefix comma.
And \"(.\" is leading to an camel case.

We also add / character as extra keys those continue the conversion,
and we don't let cljr-slash command quit the conversion."
  (let* ((hook (plist-get args :hook))
         (separator (plist-get args :separator))
         (inputs (plist-get args :inputs))
         (commands (plist-get args :commands))
         (entries (plist-get args :entries))
         (hook-fn (intern (concat "casease-setup-for-" (symbol-name hook)))))
    `(progn
       (defun ,hook-fn ()
         (when ,separator
           (setq-local casease-separator-key ,separator)
           (add-to-list 'casease-input-keys ,separator))
         (setq-local casease-entry-alist (quote ,entries))
         (dolist (k (quote ,inputs)) (add-to-list 'casease-input-keys k))
         (dolist (c (quote ,commands)) (add-to-list 'casease-extending-commands c))
         (casease-mode t))
       (add-hook (quote ,hook) (quote ,hook-fn)))))

(define-minor-mode casease-mode
  "This minor mode help input symbols in variant cases without pressing the shift key.
By inserting a prefix char(see `casease-entry-alist'), your input will be converted to
corresponding case automatically. The words can always be separated by whitespaces.

The conversion will fininshed once your cursor leaving the highlight or you give a non alphanum input."
  nil
  "casease"
  nil
  (if casease-mode
      (casease--init)
    (casease--uninit)))

(provide 'casease)

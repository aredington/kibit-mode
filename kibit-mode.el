;;; kibit-mode.el --- Enhance clojure-mode with Kibit analysis

;; Copyright (C) 2012 Alex Redington <http://www.holychao.com>
;; Authors: Alex Redington
;; Created: 2012
;; Version: 0.1
;; Keywords: clojure kibit
;; Package-Requires: ((clojure-mode "1.11.5")
;;                    (mode-compile "2.29"))

;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.


;;; Documentation:
;;
;; This minor mode acts as a compilation mechanism for interactively replacing
;; clojure s-expressions by their more idiomatic representations. It provides
;; the following capabilities:
;;
;;  * Run a check over the currently open file (bound to `\C-c \C-n`). This will
;;    open a compilation mode buffer showing the kibit replacements.
;;
;;  * Implement a suggested replacement (bound to `r`). This will destroy the
;;    extant formatting when the replacement is inserted

;;; Dependencies:
;; This minor mode depends on `mode-compile` and `clojure-mode`.

;;; Change Log:
;;
;; 0.1 - First cut of kibit-mode

;;; Code:

(require 'clojure-mode)

(defconst kibit-mode-keymap (make-sparse-keymap) "Keymap used in kibit mode")

(define-key kibit-mode-keymap (kbd "C-c C-n") 'kibit-check)

(defgroup kibit-mode nil
  "Kibit minor mode.")

(eval-and-compile
  (defvar kibit-mode-path
    (let ((path (or (locate-library "kibit-mode") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the kibit-mode package.
This is used to execute the supporting kibit analysis execution environment.
The default value is automatically computed from the location of the
Emacs Lisp package."))


;;;###autoload
(define-minor-mode kibit-mode
  "Minor mode for kibit compilation support"
  :lighter " kibit"
  :keymap  kibit-mode-keymap)

(defun kibit-check ()
  "Runs the current file through kibit check"
  (interactive)
  (cd kibit-mode-path)
  (compile (concat "lein run -m kibit-mode.core "
                   (buffer-file-name))))

(add-to-list 'compilation-error-regexp-alist-alist
	     '(kibit-mode "\\([0-9A-Za-z_./\:-]+\\.clj\\):\\([0-9]+\\):" 1 2))
(add-to-list 'compilation-error-regexp-alist 'kibit-mode)

(provide 'kibit-mode)
;;; kibit-mode.el ends here

;;; npm-mode.el --- minor mode for working with npm projects

;; Version: 0.4.1
;; Author: Alex Chesters  <???>
;;         Allen Gooch    <allen.gooch@gmail.com> (refactor)
;; Url: https://github.com/mojochao/npm-mode
;; Keywords: project, javascript, node, npm
;; Package-Requires: ((emacs "23.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package allows you to easily work with npm projects. Its API
;; provides many useful functions, as well as a minor mode providing a
;; convenient command keymap for interactive use.
;;
;; | command                       | keymap       | description                          |
;; |-------------------------------|--------------|--------------------------------------|
;; | npm-mode-npm-init             | <kbd>n</kbd> | initialize new project               |
;; | npm-mode-npm-install          | <kbd>i</kbd> | install project dependencies         |
;; | npm-mode-npm-install-save     | <kbd>s</kbd> | install new project dependency       |
;; | npm-mode-npm-install-save-dev | <kbd>d</kbd> | install new project dev dependency   |
;; | npm-mode-npm-uninstall        | <kbd>u</kbd> | uninstall project dependency         |
;; | npm-mode-npm-run              | <kbd>r</kbd> | run project script                   |
;; | npm-mode-visit-project-file   | <kbd>v</kbd> | visit project file                   |
;; |                               | <kbd>?</kbd> | display keymap commands              |

;;; Credit:

;; This package began as a fork of the https://github.com/AlexChesters/emacs-npm repo.
;; Many thanks to Alex.

;;; Code:

(require 'json)

(defvar npm-mode--project-file-name "package.json"
  "The name of npm project files.")

(defvar npm-mode--project-file nil
  "The current project file.")

(defvar npm-mode--no-project-file-found "error: no project file found"
  "Error message for missing project file.")

(defvar npm-mode--buffer-name "*npm-mode*"
  "Name of npm mode buffers.")

(defun npm-mode--get-project-property (prop)
  "Get the given PROP from the current project file."
  (setq npm-mode--project-file (npm-mode--find-file npm-mode--project-file-name))
  (unless npm-mode--project-file
    (error npm-mode--no-project-file-found))
  (let* ((json-object-type 'hash-table)
          (json-contents
            (shell-command-to-string (concat "cat " npm-mode--project-file)))
          (json-hash (json-read-from-string json-contents))
          (commands (list)))
    (maphash (lambda (key value) (setq commands (append commands (list (list key (format "%s %s" "npm" key)))))) (gethash prop json-hash))
    commands))

(defun npm-mode--get-project-scripts ()
  "Get a list of project scripts."
  (npm-mode--get-project-property "scripts"))

(defun npm-mode--get-project-dependencies ()
  "Get a list of project dependencies."
  (npm-mode--get-project-property "dependencies"))

(defun npm-mode--find-file (file-to-find &optional starting-path)
  "Recursively search parent directories for FILE-TO-FIND from STARTING-PATH.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found.

By default, it uses the `default-directory` as a starting point unless stated
otherwise through the use of STARTING-PATH.

This function is taken from
http://www.emacswiki.org/emacs/EmacsTags#tags"
  (cl-labels
    ((find-file-r (path)
       (let* ((parent (file-name-directory path))
               (possible-file (concat parent file-to-find)))
         (cond
           ((file-exists-p possible-file) possible-file) ; Found
           ;; The parent of ~ is nil and the parent of / is itself.
           ;; Thus the terminating condition for not finding the file
           ;; accounts for both.
           ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
           (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r (if starting-path starting-path default-directory))))

(defun npm-mode-npm-init ()
  "Run the npm init command."
  (interactive)
  (ansi-term (getenv "SHELL") "npm-mode-npm-init")
  (comint-send-string "*npm-mode-npm-init*" "npm init\n"))

(defun npm-mode-npm-install ()
  "Run the 'npm install' command."
  (interactive)
  (ansi-term (getenv "SHELL") "npm-mode-npm-install")
  (comint-send-string "*npm-mode-npm-install*" "npm install\n"))

(defun npm-mode-npm-install-save (dependency)
  "Run the 'npm install --save' command."
  (interactive "sEnter package name: ")
  (message "Running npm install %s --save" dependency)
  (ansi-term (getenv "SHELL") "npm-mode-npm-install-save")
  (comint-send-string "*npm-mode-npm-install-save*" (format "npm install %s --save\n" dependency)))

(defun npm-mode-npm-install-save-dev (dependency)
  "Run the 'npm install --save-dev' command."
  (interactive "sEnter package name: ")
  (message "Running npm install %s --save-dev" dependency)
  (ansi-term (getenv "SHELL") "npm-mode-npm-install-save-dev")
  (comint-send-string "*npm-mode-npm-install-save-dev*" (format "npm install %s --save-dev\n" dependency)))

(defun npm-mode-npm-uninstall ()
  "Run the 'npm uninstall' command."
  (interactive)
  (let ((command
          (ido-completing-read
            "Uninstall dependency: " (npm-mode--get-project-dependencies))))
    (message "Uninstalling: %s" command)
    (switch-to-buffer npm-mode--buffer-name command)
    (erase-buffer)
    (ansi-term (getenv "SHELL") "npm-mode-npm-uninstall")
    (comint-send-string "*npm-mode-npm-uninstall*" (format "npm uninstall --save %s\n" command))))
  
(defun npm-mode-npm-run ()
  "Run the 'npm run' command on a project script."
  (interactive)
  (let ((command
          (ido-completing-read
            "Run command: " (npm-mode--get-project-scripts))))
    (message "Running npm script: %s" command)
    (switch-to-buffer npm-mode--buffer-name command)
    (erase-buffer)
    (ansi-term (getenv "SHELL") "npm-mode-npm-run")
    (comint-send-string "*npm-mode-npm-run*" (format "npm run-script %s\n" command))))
  
(defun npm-mode-visit-project-file ()
  "Visit the project file."
  (interactive)
  (setq npm-mode--project-file (npm-mode--find-file npm-mode--project-file-name))
  (if npm-mode--project-file
    (find-file npm-mode--project-file)
    (error npm-mode--no-project-file-found)))

(defgroup npm-mode nil
  "Customization group for `npm-mode'."
  :group 'convenience)

(defcustom npm-mode-command-prefix "C-c n"
  "Prefix for `npm-mode'."
  :group 'npm-mode)

(defvar npm-mode-command-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'npm-mode-npm-init)
    (define-key map "i" 'npm-mode-npm-install)
    (define-key map "s" 'npm-mode-npm-install-save)
    (define-key map "d" 'npm-mode-npm-install-save-dev)
    (define-key map "u" 'npm-mode-npm-uninstall)
    (define-key map "r" 'npm-mode-npm-run)
    (define-key map "v" 'npm-mode-visit-project-file)
    map)
  "Keymap for `npm-mode' commands.")

(defvar npm-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd npm-mode-command-prefix) npm-mode-command-keymap)
    map)
  "Keymap for `npm-mode'.")

;;;###autoload
(define-minor-mode npm-mode
  "Minor mode for working with npm projects."
  nil
  " NPM"
  npm-mode-keymap
  :group 'npm-mode)

;;;###autoload
(define-globalized-minor-mode npm-global-mode
  npm-mode
  npm-mode)

(provide 'npm-mode)
;;; npm-mode.el ends here

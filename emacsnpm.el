;;; emacsnpm.el --- Easy interaction with npm in emacs

;;; Commentary:

;; This package is intended to provide easy interaction with NPM in Emacs

;;; Code:

(require 'json)

(defvar emacsnpm-package-file nil
  "The appropriate package.json file for a user's project.")

(defun emacsnpm-get-hash-from-package (hash)
  "Get the given HASH from the package.json file."
  (setq emacsnpm-package-file (emacsnpm-find-file "package.json"))
  (unless emacsnpm-package-file
    (error "ERROR: Couldn't find a package.json in your current or parent directory"))
  (let* ((json-object-type 'hash-table)
          (json-contents
            (shell-command-to-string (concat "cat " emacsnpm-package-file)))
          (json-hash (json-read-from-string json-contents))
          (commands (list)))
    (maphash (lambda (key value) (setq commands (append commands (list (list key (format "%s %s" "npm" key)))))) (gethash hash json-hash))
    commands))

(defun emacsnpm-get-scripts ()
  "Parsing the appropriate package.json file and returning a list of npm commands as found in the 'scripts' property in the package.json ."
  (emacsnpm-get-hash-from-package "scripts"))

(defun emacsnpm-get-dependencies ()
  "Parsing the appropriate package.json file and returning a list of npm commands as found in the 'dependencies' property in the package.json ."
  (emacsnpm-get-hash-from-package "dependencies"))

(defun emacsnpm-find-file (file-to-find &optional starting-path)
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

(defun emacsnpm-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun ordinary-insertion-filter (proc string)
  "Given a PROC, a STRING is passed through which then has colors applied to it."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (ansi-color-apply-on-region (process-mark proc) (point))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun emacsnpm-exec ()
  "Call any of the available commands defined in the script object of the package.json ."
  (interactive)
  (let ((command
          (ido-completing-read
            "Run command: " (emacsnpm-get-scripts))))
    (message "Running npm script: %s" command)
    (switch-to-buffer "emacsnpm" command)
    (erase-buffer)
    (ansi-term (getenv "SHELL") "emacsnpm-exec")
    (comint-send-string "*emacsnpm-exec*" (format "npm run-script %s\n" command))))
  
(defun emacsnpm-open-package ()
  "Open the appropriate package.json file."
  (interactive)
  (setq emacsnpm-package-file (emacsnpm-find-file "package.json"))
  (if emacsnpm-package-file
    (find-file emacsnpm-package-file)
    (error "ERROR: Couldn't find a package.json in your current or parent directory")))

(defun emacsnpm-init ()
  "Run the npm init command."
  (interactive)
  (ansi-term (getenv "SHELL") "emacsnpm-init")
  (comint-send-string "*emacsnpm-init*" "npm init\n"))

(defun emacsnpm-install ()
  "Run the npm install command."
  (interactive)
  (ansi-term (getenv "SHELL") "emacsnpm-install")
  (comint-send-string "*emacsnpm-install*" "npm install\n"))

(defun emacsnpm-uninstall ()
  "Run the npm uninstall command."
  (interactive)
  (let ((command
          (ido-completing-read
            "Uninstall dependency: " (emacsnpm-get-dependencies))))
    (message "Uninstalling: %s" command)
    (switch-to-buffer "emacsnpm" command)
    (erase-buffer)
    (ansi-term (getenv "SHELL") "emacsnpm-uninstall")
    (comint-send-string "*emacsnpm-uninstall*" (format "npm uninstall --save %s\n" command))))
  
(defun emacsnpm-save (dependency)
  "Install and save a DEPENDENCY."
  (interactive "sEnter package name: ")
  (message "Running npm install %s --save" dependency)
  (ansi-term (getenv "SHELL") "emacsnpm-save")
  (comint-send-string "*emacsnpm-save*" (format "npm install %s --save\n" dependency)))

(defun emacsnpm-save-dev (dependency)
  "Install and save a dev DEPENDENCY."
  (interactive "sEnter package name: ")
  (message "Running npm install %s --save-dev" dependency)
  (ansi-term (getenv "SHELL") "emacsnpm-save-dev")
  (comint-send-string "*emacsnpm-save-dev*" (format "npm install %s --save-dev\n" dependency)))

(defgroup emacsnpm nil
  "Customization group for `emacsnpm'."
  :group 'convenience)

(defcustom emacsnpm-keymap-prefix "C-c n"
  "Prefix for `emacsnpm'."
  :group 'emacsnpm)

(defvar emacsnpm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'emacsnpm-init)	         ; mnemonic 'new'
    (define-key map "e" 'emacsnpm-open-package)	 ; mnemonic 'edit'
    (define-key map "i" 'emacsnpm-install)       ; mnemonic 'install'
    (define-key map "r" 'emacsnpm-exec)		 ; mnemonic 'run'
    (define-key map "s" 'emacsnpm-save)		 ; mnemonic same as npm -S opt
    (define-key map "d" 'emacsnpm-save-dev)	 ; mnemonic same as npm -D opt
    (define-key map "u" 'emacsnpm-uninstall)	 ; mnemonic 'uninstall'
    map)
  "Keymap for `emacsnpm' commands.")

(defvar emacsnpm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd emacsnpm-keymap-prefix) emacsnpm-command-map)
    map)
  "Keymap for `emacsnpm'.")

;;;###autoload
(define-minor-mode emacsnpm-mode
  "Minor mode for integration with npm."
  nil
  " npm"
  emacsnpm-mode-map
  :group 'docker)

;;;###autoload
(define-globalized-minor-mode emacsnpm-global-mode
  emacsnpm-mode
  emacsnpm-mode)

(provide 'emacsnpm)
;;; emacsnpm.el ends here

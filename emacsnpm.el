;;; emacsnpm.el --- Easy interaction with npm in emacs

;;; Commentary:

;;; Code:

(require 'json)

(defvar emacsnpm-package-file nil)
(defvar emacsnpm-packgageJSON nil)

(defun emacsnpm-parse ()
  "Parsing the package.json ."
  (interactive)
  (setq emacsnpm-package-file (emacsnpm-find-file "package.json"))
  (message emacsnpm-package-file)
  (setq emacsnpm-packgageJSON (concat (emacsnpm-string-from-file emacsnpm-package-file)))
  (let* ((json-object-type 'hash-table)
          (json-contents
            (shell-command-to-string (concat "cat " emacsnpm-package-file)))
          (json-hash (json-read-from-string json-contents)))
    (message (gethash "main" json-hash)))
  )
  
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

(defun emacsnpm-start ()
  "Run the npm start command."
  (interactive)
  (shell-command "npm start &")
  )

(defun emacsnpm-test ()
  "Run the npm test command."
  (interactive)
  (shell-command "npm test &")
  )

(defun emacsnpm-stop ()
  "Run the npm stop command."
  (interactive)
  (shell-command "npm stop &")
  )

(provide 'emacsnpm)
;;; emacsnpm.el ends here

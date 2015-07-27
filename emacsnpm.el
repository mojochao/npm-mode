;;; emacsnpm.el --- Easy interaction with npm in emacs

;;; Commentary:

;;; Code:

(defvar emacsnpm-package nil)

(defun emacsnpm-parse ()
  "Parsing the package.json ."
  (interactive)
  (setq emacsnpm-package (find-file-upwards "package.json"))
  (message emacsnpm-package)
  )

(defun find-file-upwards (file-to-find &optional starting-path)
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

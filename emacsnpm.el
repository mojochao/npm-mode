;;; emacsnpm.el --- Easy interaction with npm in emacs

;;; Commentary:

;;; Code:

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

(provide 'emacsnpm)
;;; emacsnpm.el ends here

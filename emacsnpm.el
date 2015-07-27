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

(defun emacsnpm-stop ()
  "Run the npm stop command."
  (interactive)
  (shell-command "npm stop &")
  )

(provide 'emacsnpm)
;;; emacsnpm.el ends here

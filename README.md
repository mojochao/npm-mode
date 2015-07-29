# Emacs-npm #

# Setup #

Clone this repo:

> `git clone https://github.com/AlexChesters/emacs-npm.git`

Add it to your Emacs' `load-path`:

> `(add-to-list load-path '/path/to/emacs-npm/')`

# Usage #

# Opening your `package.json` #

From anywhere inside a directory which contains a `package.json` run the following command:

> `emacsnpm-open-package`

# Running npm-init #
From anywhere inside a directory in which you wish to initialise npm run the following command:

> `emacsnpm-init`

This will open the `package.json` contained in your directory

# TODO #

* Automatically build a list of available npm commands from a `package.json`
* Host project on MELPA

# Emacs-npm

# Setup

Clone this repo:

`git clone https://github.com/AlexChesters/emacs-npm.git`

Add it to your Emacs' `load-path`:

`(add-to-list load-path '/path/to/emacs-npm/')`

# Usage

# Opening your `package.json`

From anywhere inside a directory which contains a `package.json` run the following command:

#### `emacsnpm-open-package`

# Running npm-init 
From anywhere inside a directory in which you wish to initialise npm run the following command:

#### `emacsnpm-init`

This will open the `package.json` contained in your directory

# Running npm commands
From anywhere inside a directory that contains a `package.json` run the following command:

#### `emacsnpm-exec`

This will provide you with a list of possible commands which are populated from the script object in the `package.json`.

# Saving npm dependencies
From anywhere inside a directory that contains a `package.json` run the following command:

#### `emacsnpm-save`

This will run prompt you for the name of a package and run `npm install --save` for that package

# TODO 

* Host project on MELPA

# Emacs-npm

# Setup

Clone this repo:

`git clone https://github.com/AlexChesters/emacs-npm.git`

Add it to your Emacs' `load-path`:

`(add-to-list load-path '/path/to/emacs-npm/')`

Enable the mode globally:

`(emacsnpm-global-mode)`

## Commands

### `emacsnpm-global-mode`

Running <kbd>M-x emacsnpm-global-mode</kbd> creates keybindings to the
various emacsnpm commands. The keymap prefix is `C-c n` by default and can be
changed with <kbd>M-x customize-variable emacsnpm-keymap-prefix</kbd>.

| command               | Keymap       | Description                  |
|-----------------------|--------------|------------------------------|
| emacsnpm-init         | <kbd>n</kbd> | Initialize new package.json  |
| emacsnpm-open-package | <kbd>e</kbd> | Edit package.json            |
| emacsnpm-install      | <kbd>i</kbd> | Install dependencies         |
| emacsnpm-exec         | <kbd>r</kbd> | Run script                   |
| emacsnpm-save         | <kbd>s</kbd> | Install dependency           |
| emacsnpm-save-dev     | <kbd>d</kbd> | Install dev dependency       |
| emacsnpm-uninstall    | <kbd>u</kbd> | Uninstall project package    |
|                       | <kbd>?</kbd> | Display keymap commands      |

You can also call the commands directly.

### `emacsnpm-init`

Running <kbd>C-c n n</kbd> will create a new npm project in the current directory.

### `emacsnpm-open-package`

Running <kbd>C-c n e</kbd> in an npm project directory will open the project 
package.json file in a buffer for editing

### `emacsnpm-install`

Running <kbd>C-c n i</kbd> in an npm project directory will install the project
dependencies.

### `emacsnpm-exec`

Running <kbd>C-c n r</kbd> in an npm project directory will prompt for the
name of a script to run and will run it. Completion support is provided.

#### `emacsnpm-save`

Running <kbd>C-c n s</kbd> in an npm project directory will prompt for the
name of a package to install and install it as a project dependency.

#### `emacsnpm-save-dev`

Running <kbd>C-c n s</kbd> in an npm project directory will prompt for the
name of a package to install and install it as a project dev dependency.

#### `emacsnpm-uninstall`

Running <kbd>C-c n s</kbd> in an npm project directory will prompt for the
name of a package to uninstall and uninstall it as a project dependency.

# TODO 

* Host project on MELPA

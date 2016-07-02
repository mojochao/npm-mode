# npm-mode

This repository provides an [Emacs](https://www.gnu.org/software/emacs/) minor 
mode for working with [NPM](https://www.npmjs.com/) projects.

## Installation

The recommended way to install npm-mode.el is through [MELPA](https://github.com/milkypostman/melpa).

Otherwise, clone this repo:

`git clone https://github.com/mojochao/npm-mode.git /your/path/here`

In your emacs configuration, add the repo to the load path and require it:

```
(add-to-list load-path '/your/path/here/npm-mode')
(require 'npm-mode)
```

## Configuration

This package provides a minor mode which can be activated with:

`(npm-mode)`

It can also be activated globally with:

`(npm-global-mode)`

## Commands

### npm-global-mode ###

Running <kbd>M-x npm-global-mode</kbd> creates keybindings to the
various npm-mode commands. The mode command prefix is <bkd>C-c n</kbd> by default and 
can be changed with <kbd>M-x customize-variable npm-mode-keymap-prefix</kbd>.

| command                       | keymap       | description                    |
|-------------------------------|--------------|--------------------------------|
| npm-mode/npm-init             | <kbd>n</kbd> | Initialize new project         |
| npm-mode/npm-install          | <kbd>i</kbd> | Install project dependencies   |
| npm-mode/npm-install-save     | <kbd>s</kbd> | Install new dependency         |
| npm-mode/npm-install-save-dev | <kbd>d</kbd> | Install new dev dependency     |
| npm-mode/npm-uninstall        | <kbd>u</kbd> | Uninstall project dependency   |
| npm-mode/npm-run              | <kbd>r</kbd> | Run project script             |
| npm-mode/visit-project-file   | <kbd>v</kbd> | Visit project file             |
|                               | <kbd>?</kbd> | Display keymap commands        |

You can also call the commands directly.

### npm-mode/npm-init

Running <kbd>C-c n n</kbd> will create a new project in the current directory.

### npm-mode/npm-install

Running <kbd>C-c n i</kbd> in a project directory will install all project
dependencies.

### npm-mode/npm-install-save

Running <kbd>C-c n s</kbd> in a project directory will prompt for the name of a
package to install and will install it as a project dependency.

### npm-mode/npm-install-save-dev

Running <kbd>C-c n d</kbd> in a project directory will prompt for the name of a
to install and will install it as a project dev dependency.

### npm-mode/npm-uninstall

Running <kbd>C-c n u</kbd> in a project directory will prompt for the name of a
package to uninstall and will uninstall it and remove it from project dependencies.

### npm-mode/npm-run

Running <kbd>C-c n r</kbd> in a project directory will prompt for the name of a
script to run and will run it. Completion support is provided.

### npm-mode/visit-project-file

Running <kbd>C-c n v</kbd> in a project directory will visit the project file
in a buffer.

## TODO

Add npm-mode/npm-debug command that will run a project script under the node 
debugger, and bind it to <kbd>C-c n d</kbd> in mode command keymap.

## Acknowledgements

This repo is a based on a fork of https://github.com/AlexChesters/emacs-npm, 
and its history is preserved.  Many thanks to Alex for his creation.

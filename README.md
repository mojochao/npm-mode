# npm-mode

This repository provides an [Emacs](https://www.gnu.org/software/emacs/) minor 
mode for working with [NPM](https://www.npmjs.com/) projects.

## Installation

### Package Manager

The recommended way to install npm-mode.el is through the package manager
and [MELPA](https://github.com/milkypostman/melpa).

### Manual Installation

Start by cloning the package repository.

`git clone https://github.com/mojochao/npm-mode.git /your/path/here`

Finish by loading the package in your emacs configuration.

```
(add-to-list load-path '/your/path/here/npm-mode')
(require 'npm-mode)
```

## Configuration

This package provides a minor mode which can be activated with:

`(npm-mode)`

It can also be activated globally with:

`(npm-global-mode)`

You can automatically activate the mode for a project by adding the mode to 
the `.dir-locals.el` file in the root directory of a project.

```
((nil . ((mode . npm))))
```

Now when you visit any file in the project, `npm-mode` will be activated.

## Commands

### npm-global-mode ###

Running <kbd>M-x npm-global-mode</kbd> creates keybindings to the
various npm-mode commands. The mode command prefix is <kbd>C-c n</kbd> by default and 
can be changed with <kbd>M-x customize-variable npm-mode-keymap-prefix</kbd>.

| command                       | keymap       | description                      |
|-------------------------------|--------------|----------------------------------|
| npm-mode-npm-init             | <kbd>n</kbd> | Initialize new project           |
| npm-mode-npm-install          | <kbd>i</kbd> | Install all project dependencies |
| npm-mode-npm-install-save     | <kbd>s</kbd> | Add new project dependency       |
| npm-mode-npm-install-save-dev | <kbd>d</kbd> | Add new project dev dependency   |
| npm-mode-npm-uninstall        | <kbd>u</kbd> | Remove project dependency        |
| npm-mode-npm-run              | <kbd>r</kbd> | Run project script               |
| npm-mode-visit-project-file   | <kbd>v</kbd> | Visit project package.json file  |
|                               | <kbd>?</kbd> | Display keymap commands          |

You can also call the commands directly.

### npm-mode-npm-init

Running <kbd>C-c n n</kbd> will create a new project in the current directory.

### npm-mode-npm-install

Running <kbd>C-c n i</kbd> in a project directory will install all project
dependencies.

### npm-mode-npm-install-save

Running <kbd>C-c n s</kbd> in a project directory will prompt for the name of a
package to install and will install it as a project dependency.

### npm-mode-npm-install-save-dev

Running <kbd>C-c n d</kbd> in a project directory will prompt for the name of a
to install and will install it as a project dev dependency.

### npm-mode-npm-uninstall

Running <kbd>C-c n u</kbd> in a project directory will prompt for the name of a
package to uninstall and will uninstall it and remove it from project dependencies.

### npm-mode-npm-run

Running <kbd>C-c n r</kbd> in a project directory will prompt for the name of a
script to run and will run it. Completion support is provided.

### npm-mode-visit-project-file

Running <kbd>C-c n v</kbd> in a project directory will visit the project file
in a buffer.

## Acknowledgements

This repo is a refactor of a fork of https://github.com/AlexChesters/emacs-npm, 
and its history has been preserved.  Many thanks to Alex for his creation.

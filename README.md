# npm-mode

This repository provides an [Emacs](https://www.gnu.org/software/emacs/) minor 
mode for working with [NPM](https://www.npmjs.com/) projects.

## Installation

### Package Manager

The recommended way to install npm-mode.el is through the package manager
and [MELPA](https://github.com/milkypostman/melpa).

### Manual Installation

Start by cloning the package repository.

`$ git clone https://github.com/mojochao/npm-mode.git /your/path/here`

Finish by loading the package in your emacs configuration.

```
(add-to-list 'load-path "/your/path/here/npm-mode")
(require 'npm-mode)
```

## Configuration

This package provides a minor mode which can be activated with:

`(npm-mode)`

Once activated the minor mode ke

## Global Activation

The minor mode can be activated globally with:

`(npm-global-mode)`

### Project Activation

The mode can be activated on a per-project basis using directory local
variables. The easiest way to do this is to use the
`add-dir-local-variable` from the root directory of a project.

For example, if you visit the `README.md` file in the root directory
of a project, and run the <kbd>M-x add-dir-local-variable</kbd><kbd>
command, Emacs will prompt you for `Mode or subdirectory (default
markdown mode): `, to which you should enter <kbd>nil</kbd>. Next, you
will be prompted for `Add directory-local variable: `, to which you
should enter <kbd>mode</kbd>.  Finally, you will be prompted for `Add
mode with value (default markdown mode): `, to which you should enter
<kbd>npm-mode</kbd>.

> Note that the defaults were markdown-mode because we invoked the 
> command while visiting a markdown file. Defaults will use the
> major mode of the buffer when invoking the command.

If you look at the generated `.dir-locals.el` file you can see the 
generated configuration:

```
$ cat /your/project/root/.dir-locals.el
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (mode . npm)))
$
```

If you later wish to disable npm-mode for the project, run the 
`delete-dir-local-variable` command similarly.

## Command Keymap

This mode provides a command keymap, whose default <kbd>C-c n</kbd>
prefix can be changed with <kbd>M-x customize-variable
npm-mode-keymap-prefix</kbd>.

## Usage

Once the mode has been activated, you can use its commands.  Invoking
<kbd>C-c n</kbd> provides access to the following commands. 

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

This repo is a rewrite of a fork of https://github.com/AlexChesters/emacs-npm, 
and its history has been preserved.  Many thanks to Alex for his creation.

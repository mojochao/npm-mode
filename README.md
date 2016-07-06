# npm-mode

This is *npm-mode*, an [Emacs](https://www.gnu.org/software/emacs/)
minor mode for working with [NPM](https://www.npmjs.com/) projects.

## Installation

### Package Manager

The recommended way to install *npm-mode* is through the package
manager and [MELPA](https://github.com/milkypostman/melpa).

### Manual Installation

Start by cloning the *npm-mode* package repository:

`$ git clone https://github.com/mojochao/npm-mode.git /your/path/here`

Finish by loading the *npm-mode* package in your emacs configuration:

```
(add-to-list 'load-path "/your/path/here/npm-mode")
(require 'npm-mode)
```

## Configuration

The *npm-mode* package provides a minor mode which can be activated
with:

`(npm-mode)`

### Global Activation

The mode can be activated globally with:

`(npm-global-mode)`

### Project Activation

The mode can be activated on a per-project basis using directory local
variables by running `add-dir-local-variable` from the root directory
of a project.

For example, visit the `README.md` file in the root directory of a
project, and run <kbd>M-x add-dir-local-variable</kbd>.  Emacs will
prompt you for `Mode or subdirectory:`, to which you should enter
`nil`.

Next, you will be prompted for `Add directory-local variable:`, to
which you should enter `mode`.

Next, you will be prompted for `Add mode with value:`, to which you
should enter `npm-mode`.

Next, you will be placed in a new `.dir-locals.el` buffer containing
the directory local variables for the project, including the added
`npm-mode` configuration.

```
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (mode . npm)))
```

Finally, save the buffer contents with <kbd>C-x C-s</kbd>, and your
project is configured for `npm-mode`.

Now when you visit any file under the project directory, *npm-mode*
will be activated and its commands made available.

### Command Keymap Prefix

The mode provides a command keymap, whose default <kbd>C-c n</kbd>
prefix can be configured with <kbd>M-x customize-variable
npm-mode-keymap-prefix</kbd>.

## Usage

### Command Keymap

Once *npm-mode* has been activated, its commands are accessed by the
<kbd>C-c n</kbd> command prefix:

| command                       | keymap       | description                         |
|-------------------------------|--------------|-------------------------------------|
| npm-mode-npm-init             | <kbd>n</kbd> | Initialize new project              |
| npm-mode-npm-install          | <kbd>i</kbd> | Install all project dependencies    |
| npm-mode-npm-install-save     | <kbd>s</kbd> | Add new project dependency          |
| npm-mode-npm-install-save-dev | <kbd>d</kbd> | Add new project dev dependency      |
| npm-mode-npm-uninstall        | <kbd>u</kbd> | Remove project dependency           |
| npm-mode-npm-list             | <kbd>l</kbd> | List installed project dependencies |
| npm-mode-npm-run              | <kbd>r</kbd> | Run project script                  |
| npm-mode-visit-project-file   | <kbd>v</kbd> | Visit project package.json file     |
|                               | <kbd>?</kbd> | Display keymap commands             |

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

### npm-mode-npm-list

Running <kbd>C-c n l</kbd> in a project directory will list the project.

### npm-mode-npm-run

Running <kbd>C-c n r</kbd> in a project directory will prompt for the name of a
script to run and will run it. Completion support is provided.

### npm-mode-visit-project-file

Running <kbd>C-c n v</kbd> in a project directory will visit the project file
in a buffer.

## Acknowledgements

This repo is a rewrite of a fork of https://github.com/AlexChesters/emacs-npm, 
and its history has been preserved.  Many thanks to Alex for his contribution.

## TODO

### Add run in debugger support

Add `npm-mode-npm-run-debug` command that will run a project script
under the node debugger, and bind it to <kbd>C-c n d</kbd> in mode
command keymap.

### Add ability to specialize command invocation in keymap.

Some commands, like `npm install --depth=0`, provide options to alter
the behavior of the command being run.  Investigate how to use
magit-popups to provide more flexible command execution.

### Add completing read support for installing packages

It would be great if we could do completing read on packages to
install that would list NPM packages available for install, much
like the Emacs package manager does with `package-install` command.

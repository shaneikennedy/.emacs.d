# My Emacs

### Setup

```sh
$ mkdir ~/emacs
$ git clone git@github.com:shaneikennedy/.emacs.d.git ~/emacs/emacs-shaneikennedy
$ ln -s ~/emacs/emacs-shaneikennedy ~/.emacs.d
```
### Requirements

- emacs >= 26

### Installing
```
$ brew cask install emacs
```

### Editing
This is an evil editor (vim bindings). This setup is configured mostly for python, javascript, and haskell and attempts to use the Language Server Protocol when possible to give an IDE-like completion experience.

### C++ Mode
This conf uses [ccls](https://github.com/MaskRay/ccls) for formatting. For best results, generate a `compile_commands.json` file with cmake for your project. This can be done using `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..`. Then you can move this file to the project root and gitignore it. This allows CCLS to find and index all the deps for the project.

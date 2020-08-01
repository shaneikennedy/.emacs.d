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
The RTags submodule needs to be initialized and built, then the lsp can be started as a background process.

``` sh
$ git submodule update --init
$ cd rtags
$ cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
$ make
```

Following this, the background process is started using:

``` sh
# Start the RTags daemon (rdm)
$ ./bin/rdm &

# Index the RTags project, and wait until rdm is silent
$ ./bin/rc -J .
```

For more information, refer to the [Rtags Github repo](https://github.com/Andersbakken/rtags).

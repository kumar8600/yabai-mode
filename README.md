cc-check-and-completion
========================

Minor mode for auto configuration of C/C++ syntax checking and completion plugins with [cmake][].

This mode get the compile options from build system [cmake][] and
configure syntax checking and completion plugins. Not only that, you can
generate build tree and compile and run it. So, you can use emacs like IDE
by only put source codes and build configuration file `CMakeFile.txt`.

This mode can works with below.

Build systems (This is requirements):

  - [cmake][] (with (UNIX/MinGW/MSYS) Makefile generator)

Syntax check packages:

  - [flycheck][]
  ..- >= 0.16

Completion (like Intellisense) packages:

  - [company][]
  ..- >= 0.5
  - [emacs-clang-complete-async][]

Installation
------------

Basic setup:

   (add-to-list 'load-path
                "~/path-to-cc-check-and-completion-utility")
   (require 'cc-check-and-completion-utility)

   (add-hook 'c-mode-hook 'cccc-mode)
   (add-hook 'c++-mode-hook 'cccc-mode)

Usage
-----

#### Basic

1. Put source codes and `CMakeLists.txt` on your source tree.
2. Create build-tree named `build`. (If you don't do it, this package asks you where is build-tree.)
3. Just open your source code.

(If valid `CMakeLists.txt` is exists at upper or current directory, it will work.
If you don't have build tree, this plugin ask you where build tree create. Build tree's name
must `build`. If you want to change this name, customize variable `cccc/cmake-build-tree-name`.)

#### Detail

Compile option analysis can be controlled with `cccc/load-options` and
`cccc/reload-options`. But you probably don't need call them directly. While cccc-mode
is enabled, those commands automatically called when you open C/C++ sources and
someone modified build configuration file.

You can generate build tree with `cccc/generate-build-files`.
You can compile build tree with `cccc/compile`.
You can run executable file with `cccc/run`.

If you want to compile and run, I recommend `cccc/compile-and-run`.

#### Acceptable trees

If `CMakeFiles.txt` and build-tree `build` are exists at upper or current directory where source is opened exists,
it is acceptable tree.

Example 1:

```
.
|-- build
`-- src
  |-- CMakeFiles.txt
  |-- blah.cpp
  |-- blah.h
  `-- blahblah
    |-- blahblah.cc
    .
    .
```

Example 2:

```
.
|-- build
|-- CMakeFiles.txt
|-- blah.cpp
.
.
```


Customization
-------------

There are customizable user options.

- `cccc/cmake-build-tree-name`
..- CMake's build tree name used when build-tree searching and creating.
..- Default value is `"build"`.

- `cccc/with-checker-flycheck`
..- Set non-nil if you want to use [flycheck][] as checker.
..- Default value is `t`.

- `cccc/with-completer-company`
..- Set non-nil if you want to use [company][] as checker.
..- Default value is `t`.

- `cccc/with-completer-ac-clang-async`
..- Set non-nil if you want to use [emacs-clang-complete-async][] as checker.
..- Default value is `t`.

License
-------

see `./LICENSE`


[flycheck]: https://github.com/flycheck/flycheck
[company]: http://company-mode.github.io/
[cmake]: http://www.cmake.org/
[clang]: http://clang.llvm.org/
[emacs-clang-complete-async]: https://github.com/Golevka/emacs-clang-complete-async

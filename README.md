YABAI - Yet Another Build-system Attentive Integration Mode
========================

Emacs minor mode for C/C++ syntax checking and completion with any build systems without any configuration in many cases.

This mode get the compiler options from build system ([CMake][] is default) and
integrate syntax checking and completion packages. Not only that, you can
compile and run it. What you shuold do is to put source codes and build configuration file (e.g. `CMakeLists.txt`).

*(YABAI (ヤバイ) is the japanese slang means dangerous, awful and amazing etc...)*

Compatible build systems
---------------------------------

  - [CMake][] (>= 2.8.5) with generator
    - [Make][GNU Make] **(default)**
	  -  works in UNIX / MinGW / MSYS
	- [Ninja][]
  - Without CMake ([Bear][] is required)
    - Make
	- Ninja
  - [JSON Compilation Database Format][clang-json] (check and completion only.)

(*At least one is required.*)

Packages can be integrated
-------------------------------

##### Syntax check packages:

  - [Flycheck][] (>= 0.16)

##### Completion (like Intellisense) packages:

  - [Company][] (>= 0.5)
  - [emacs-clang-complete-async][]

Set up
------------

Basic setup:

```
(add-to-list 'load-path "~/path-to-*")

(require 'company)
(require 'flycheck)
(require 'yabai)

;; enable Company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; enable Flycheck mode in all buffers
(add-hook 'after-init-hook #'global-flycheck-mode)

;; enable YABAI mode in C/C++ buffers
(add-hook 'c-mode-hook   'yabai-mode)
(add-hook 'c++-mode-hook 'yabai-mode)
```

Basic Usage (CMake with Make)
-------------

1. Put source codes and `CMakeLists.txt` on your source tree.
2. Create build-tree named `emacs-build`. (If you don't do it, this package asks you where do I create build-tree.)
3. Just open your source code. Enjoy!

Advanced Usage
-------------

Compiler option analysis can be controlled with `yabai/load-options` and
`yabai/reload-options`. But you probably don't need to call them directly. While cccc-mode
is enabled, those commands automatically called when you opened C/C++ sources or
someone modified build configuration file. If something went wrong, please call `yabai/reload-options`.

You can generate build tree with `yabai/generate-build-files`.  
You can compile build tree with `yabai/compile`.  
You can run executable file with `yabai/run`.  

If you want to compile and run, I recommend `yabai/compile-and-run`.

If you want to use other build system, please see below chapter [Customization](#customization).

##### CMake

If valid `CMakeLists.txt` is exists at source code's directory or upper, it will work.
If you don't have build tree, this plugin ask you where build tree create. Build tree's name must
be `emacs-build`. If you want to change this name, customize variable `yabai/cmake-build-tree-name`.

###### CMake Acceptable trees

If `CMakeLists.txt` and build-tree `emacs-build` are exists at upper or current directory where source is opened exists,
it is acceptable tree.

Example 1:

```
.
|-- emacs-build
`-- src
  |-- CMakeLists.txt
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
|-- emacs-build
|-- CMakeLists.txt
|-- blah.cpp
.
.
```

Customization
-------------

There are customizable user options.

- `yabai/build-system`
  - Build system used for all integration.
  - You can choose `cmake`, `make`, `ninja` and `json-compilation`.
  - `make`, `ninja` and `json-compilation` mean **without** CMake.
    - But `make` and `ninja` require [Bear][].
  - Default value is `cmake`.

- `yabai/cmake-generator`
  - CMake's build system generator.
  - You can select `make` or `ninja`.
    - When you select `make` in windows, automatically detect environment
	  and select `MinGW Makefiles` or `MSYS Makefiles` generators.
  - Default value is `make`.

- `yabai/cmake-build-tree-name`
  - CMake's build tree name used when build-tree searching and creating.
  - Default value is `"emacs-build"`.

- `yabai/with-checker-flycheck`
  - Set non-nil if you want to use [Flycheck][] as checker.
  - Default value is `t`.

- `yabai/with-completer-company`
  - Set non-nil if you want to use [Company][] as checker.
  - Default value is `t`.

- `yabai/with-completer-ac-clang-async`
  - Set non-nil if you want to use [emacs-clang-complete-async][] as checker.
  - Default value is `t`.

License
-------

see `./LICENSE`

Similar packages
----------------

- [cpputils-cmake][]
- [emacs-cmake-project][]
- [irony-mode][]
- [rtags][]

[Flycheck]: https://github.com/flycheck/flycheck
[Company]: http://company-mode.github.io/
[CMake]: http://www.cmake.org/
[GNU Make]: http://www.gnu.org/software/make/
[Ninja]: http://martine.github.io/ninja/
[Bear]: https://github.com/rizsotto/Bear
[Clang]: http://clang.llvm.org/
[clang-json]: http://clang.llvm.org/docs/JSONCompilationDatabase.html
[emacs-clang-complete-async]: https://github.com/Golevka/emacs-clang-complete-async

[cpputils-cmake]: https://github.com/redguardtoo/cpputils-cmake
[emacs-cmake-project]: https://github.com/alamaison/emacs-cmake-project
[irony-mode]: https://github.com/Sarcasm/irony-mode
[rtags]: https://github.com/Andersbakken/rtags/

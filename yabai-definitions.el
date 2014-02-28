;;; yabai-definitions.el --- yabai-mode backend for implementation definitions

;; Author: kumar8600 <kumar8600@gmail.com>

;; Copyright (c) 2014 by kumar8600
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:

;; * Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer.

;; * Redistributions in binary form must reproduce the above copyright notice, this
;;   list of conditions and the following disclaimer in the documentation and/or
;;   other materials provided with the distribution.

;; * Neither the name of the kumar8600 nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defmacro yabai/def-option-var (symbol value doc-string &rest rest)
  "Define custom variable grouped in yabai-options.

Variable SYMBOL with VALUE documented with DOC-STRING and REST for defcustom."
  `(defcustom ,symbol ,value
     ,doc-string
     :group 'yabai-options
     ,@rest))

;;;; Flags to enable cooperating with checker and completer ========================

;;;; Checker
(yabai/def-option-var yabai/with-checker-flycheck t
		     "Set non-nil if you want to use flycheck as checker.

See URL `https://github.com/flycheck/flycheck'"
		     :type 'boolean)

;;;; Completer
(yabai/def-option-var yabai/with-completer-company t
		     "Set non-nil if you want to use company as checker.

See URL `http://company-mode.github.io/'"
		     :type 'boolean)

(yabai/def-option-var yabai/with-completer-ac-clang-async t
		     "Set non-nil if you want to use auto-complete-clang-async as checker.

See URL `https://github.com/Golevka/emacs-clang-complete-async'"
		     :type 'boolean)

;;;; Build system ========================================================================

(yabai/def-option-var yabai/build-system 'cmake
		     "Build system used in this package.

You can choose `cmake', `make', `ninja' and `json-compilation'.

- `cmake' means CMake.
  - See URL `http://www.cmake.org/'.
  - You can choose generator with customizable variable `yabai/cmake-generator'.
- `make' means Make without CMake but with Bear (Build EAR).
  - See URL `http://www.gnu.org/software/make/'.
  - See URL `https://github.com/rizsotto/Bear'.
- `ninja' means Make without CMake but with Bear (Build EAR).
  - See URL `http://martine.github.io/ninja/'
  - See URL `https://github.com/rizsotto/Bear'.
- `json-compilation' means JSON Compilation Database Format Specification.
  - See URL `http://clang.llvm.org/docs/JSONCompilationDatabase.html'."
		     :type 'symbol
		     :options '(cmake bear json))

;;;; compiler options =====================================================================
(cl-defstruct yabai/options
  "A struct describing compiler options"
  (definitions nil           "Additional preprocessor definitions for the compiler.")
  (include-paths nil         "A list of include search path for the compiler.")
  (includes nil              "A list of additional include files for the compiler.")
  (warnings '("all" "extra") "A list of additional warnings to enable in the compiler.")
  (language-standard nil     "The language standard to use in the compiler.")
  (standard-library nil	     "Whether to disable RTTI in the compiler.")
  (ms-extension nil          "Whether to enable Microsoft extensions in the compiler.")
  (no-rtti nil               "Whether to disable RTTI in the compiler."))

;; Example
;;   (make-yabai/options :definitions nil
;; 			:include-paths '("../")
;; 			:includes nil
;; 			:warnings '("all" "extra")
;; 			:language-standard "c++11"
;; 			:standard-library nil
;; 			:ms-extension nil
;; 			:no-rtti nil))

;;;; Variables to additional setting compiler options manually
;; (yabai/def-option-var yabai/definitions nil
;; 		     "Additional preprocessor definitions for the compiler.")

;; (yabai/def-option-var yabai/include-path nil
;; 		     "A list of include search path for the compiler.")

;; (yabai/def-option-var yabai/includes nil
;; 		     "A list of additional include files for the compiler.")

;; (yabai/def-option-var yabai/language-standard nil
;; 		     "The language standard to use in the compiler.")

;; (yabai/def-option-var yabai/ms-extensions nil
;; 		     "Whether to enable Microsoft extensions in the compiler.")

;; (yabai/def-option-var yabai/no-rtti nil
;; 		     "Whether to disable RTTI in the compiler.")

;; (yabai/def-option-var yabai/standard-library nil
;; 		     "The standard library to use for the compiler.")

;; (yabai/def-option-var yabai/warnings '("all" "extra")
;; 		     "A list of additional warnings to enable in the compiler.")

;;;; Const Variables ===============================================================
(defconst yabai/build-buffer-name "*yabai-build*")

;;;; Variables for implementation ==================================================
(defvar-local yabai/stored-compiler-options nil)
(defvar-local yabai/path-source-tree nil)
(defvar-local yabai/path-build-config-file nil)
(defvar-local yabai/time-last-mod-build-config-file nil)
(defvar-local yabai/build-system-in-local-buffer nil)

;; process=>buffer hash table made to use buffer local variables in async sentinel.
(defvar yabai/process->buffer-table (make-hash-table :test 'equal))
(defun yabai/puthash-process->buffer (key val)
  "Associate KEY with VAL for global process->buffer-table."
  (puthash key val yabai/process->buffer-table))
(defun yabai/gethash-process->buffer (key)
  "Get KEY's value from global process->buffer-table."
  (gethash key yabai/process->buffer-table))

;; Lighter
(defconst yabai/mode-line-lighter " yabai"
  "The default lighter for yabai-mode.")

;;;; Build system and integration definitions.
(defvar yabai/build-system-definitions nil)
(defvar yabai/integration-definitions nil)


(provide 'yabai-definitions)

;;; yabai-definitions.el ends here

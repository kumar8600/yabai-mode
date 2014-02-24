;;; yabai-build-bear.el --- yabai-mode build system back-end for Bear

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

(require 'yabai-utilities)
(require 'yabai-build-json-compilation)

;;;; =============================================================================================
;;;; Pre process
;;;; =============================================================================================
(defun yabai/bear-pre-process (build-system build-config source-tree finish-func)
  "Build with Bear to generate compilation database.

BUILD-SYSTEM, BUILD-CONFIG, SOURCE-TREE and FINISH-FUNC are arguments."
  (yabai/bear-compile build-system build-config finish-func))

;;;; =============================================================================================
;;;; Compile option Guessing
;;;; =============================================================================================
(defun yabai/bear-guess-options (build-config-path src-file-path)
  "Guess options by JSON database is Bear outputed.

BUILD-SYSTEM, BUILD-CONFIG-PATH and SRC-FILE-PATH are arguments."
  (yabai/json-compilation-get-options (file-name-nondirectory build-config-path)
				      src-file-path))

;;;; =============================================================================================
;;;; Compile
;;;; =============================================================================================
(defun yabai/bear-compile (build-system build-config finish-func)
  "Compile with Bear BUILD-SYSTEM configured with where BUILD-CONFIG.

Finally, FINISH-FUNC will be called."
  (yabai/compile-impl (format "cd %s && bear -- %s"
			       (file-name-directory build-config)
			       (cl-case build-system
				 ('make "make")
				 ('ninja "ninja")
				 (otherwise (error "A unknown build system is set"))))
		       finish-func))

;;;; =============================================================================================
;;;; Makefile Integration
;;;; =============================================================================================
(yabai/define-build-system 'make
			   "Makefile"
			   (lambda (build-config source-tree finish-func)
			     (yabai/bear-pre-process 'make build-config source-tree finish-func))
			   #'yabai/bear-guess-options
			   (lambda (build-config finish-func)
			     (yabai/bear-compile 'make build-config finish-func)))

;;;; =============================================================================================
;;;; Ninja Integration
;;;; =============================================================================================
(yabai/define-build-system 'ninja
			   "Makefile"
			   (lambda (build-config source-tree finish-func)
			     (yabai/bear-pre-process 'ninja build-config source-tree finish-func))
			   #'yabai/bear-guess-options
			   (lambda (build-config finish-func)
			     (yabai/bear-compile 'ninja build-config finish-func)))


(provide 'yabai-build-bear)

;;; yabai-build-bear.el ends here

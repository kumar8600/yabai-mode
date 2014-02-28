;;; yabai-build-json-compilation.el --- yabai-mode build system back-end for JSON compilation database

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
(require 'async)
(require 'json)

;;;; =============================================================================================
;;;; Compile option parsing
;;;; =============================================================================================
(defun yabai/json-compilation-read-and-find (compile-commands-json-path src-file-path finish-func)
  "Parse JSON from COMPILE-COMMANDS-JSON-PATH.  And find SRC-FILE-PATH.

Finally, FINISH-FUNC is called."
  (async-start
   `(lambda ()
     (require 'json)
     (require 'cl-lib)
     ,(async-inject-variables "\\(compile-commands-json-path\\|src-file-path\\|finish-func\\)")
     ;; parse JSON file.
     (let ((db-arr (json-read-file compile-commands-json-path)))
       ;; find src-file's first compilation data.
       (cons src-file-path (cl-find-if (lambda (item)
					 (let ((file-path (cdr (assoc 'file item))))
					   (if (string= (directory-file-name (expand-file-name file-path))
							(directory-file-name (expand-file-name src-file-path)))
					       item
					     nil)))
				       db-arr))))
   finish-func))

(defun yabai/json-get-db-path (build-config build-tree)
  "From BUILD-CONFIG and BUILD-TREE, return path to JSON compilation db."
  build-config)

(defun yabai/json-compilation-compile (build-config finish-func)
  "Compile from JSON Compilation Database at BUILD-CONFIG.

Finally, FINISH-FUNC is called."
  (error "Sorry, not implemented"))

;;;; =============================================================================================
;;;; Integrate
;;;; =============================================================================================
(yabai/define-build-system 'json-compilation
			   "compile_commands.json"
			   nil
			   #'yabai/json-get-db-path
			   #'yabai/json-compilation-compile
			   nil)

(provide 'yabai-build-json-compilation)

;;; yabai-build-json-compilation.el ends here

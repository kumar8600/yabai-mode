;;; cccc-build-json.el --- cccc-mode build system back-end for JSON compilation database

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

;;;; =============================================================================================
;;;; Compile option parsing
;;;; =============================================================================================
(defun cccc/json-compilation-read-file (compile-commands-json-path)
  "Parse JSON from COMPILE-COMMANDS-JSON-PATH."
    (json-read-file compile-commands-json-path))

(defun cccc/json-compilation-find-compile-info (src-file-path info-alists)
  "Find a compile info take source SRC-FILE-PATH from INFO-ALISTS."
  (cl-find-if (lambda (item)
		(let ((file-path (cdr (assoc 'file item))))
		  (if (cccc/file-name-equal file-path
					    src-file-path)
		      item
		    nil)))
	      info-alists))

(defun cccc/json-compilation-get-options (src-file-path compile-commands-json-path)
  "Parse and get compile options of SRC-FILE-PATH from COMPILE-COMMANDS-JSON-PATH."
  (let ((compile-info-alists (cccc/json-compilation-read-file compile-commands-json-path)))
    ;; find compile info for SRC-FILE-PATH.
    (let ((compile-info-alist (or (cccc/json-compilation-find-compile-info src-file-path
									   compile-info-alists)
				  (progn (when (cl-find (downcase (file-name-extension src-file-path))
							'("cpp" "cc" "c")
							:test #'string=)
					   (message "Hint: you should add this file for `CMakeFiles.txt'. I'll recommend to do `M-x cccc/open-build-config-file'"))
					 ;; If SRC-FILE-PATH not found in COMPILE-INFO-ALISTS, use first item.
					 (aref compile-info-alists 0)))))
      ;; convert compile info to cccc/options.
      (let ((compile-command (cdr (assoc 'command compile-info-alist)))
	    (compile-dir (cdr (assoc 'directory compile-info-alist))))
	(cccc/string-to-compile-option-object (if (eq system-type 'windows-nt)
						  (cccc/msbuild-insert-rsp compile-command
									   (file-name-directory
									    compile-commands-json-path))
						compile-command)
					      compile-dir)))))


(provide 'cccc-build-json)

;;; cccc-build-json.el ends here

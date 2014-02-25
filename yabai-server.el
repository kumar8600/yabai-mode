;;; yabai-server.el --- yabai-mode back-end server script for asynchronous compiler option parsing

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
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'cl-lib)
(require 'json)

(require 'yabai-utilities)

;;;;====================================================================================
;;;; Variables
;;;;====================================================================================
(defvar yabai/compilation-path->compilation-database (make-hash-table)
   "Hash table take path to JSON compilation database string as key.

Value are array of compiler options.")

;;;;====================================================================================
;;;; Utilities
;;;;====================================================================================
(defun yabai/json-compilation-find-compile-info (src-file-path db-arr)
  "Find a compile info take source SRC-FILE-PATH from DB-ARR."
  (cl-find-if (lambda (item)
		(let ((file-path (cdr (assoc 'file item))))
		  (if (yabai/file-name-equal file-path
					    src-file-path)
		      item
		    nil)))
	      db-arr))

(defun yabai/json-compilation-alist-to-compiler-option-object (alist)
  "Convert JSON compilation database's item ALIST to yabai/options object."
  (let ((compile-command (cdr (assoc 'command alist)))
	(compile-dir (cdr (assoc 'directory alist))))
    (yabai/string-to-compiler-option-object compile-command
					    compile-dir)))

(defun yabai/server-add-database (database-path)
  "Add JSON compilation database at DATABASE-PATH to hashtable."
  (let ((db-arr (json-read-file database-path)))
    (puthash database-path db-arr yabai/compilation-path->compilation-database))
  "add-database-ok")
    
(defun yabai/server-get-options (src-file-path database-path)
  "Get compiler options object for SRC-FILE-PATH and DATABASE-PATH from hash-table."
  (let ((db-arr (gethash database-path yabai/compilation-path->compilation-database)))
    (let ((compilation-data-alist (or (yabai/json-compilation-find-compile-info src-file-path
										db-arr)
				      (error "Compiler option not found"))))
      ;; If system is windows, there is rsp.
      (when (eq system-type 'windows-nt)
	(add-to-list 'compilation-data-alist (cons 'compile
						   (yabai/msbuild-insert-rsp (assoc 'command compilation-data-alist)
									     (file-name-directory database-path)))))
      (yabai/json-compilation-alist-to-compiler-option-object compilation-data-alist))))

;;;;====================================================================================
;;;; Main loop
;;;;====================================================================================
(defun yabai/server-mainloop ()
  "Receive a list and do it and print alist of buffer and yabai/options."
  (let ((lst (car (read-from-string (read-string "")))))
    (let ((file-name (car lst))
	  (func-symbol (cadr lst))
	  (arg-lst (cddr lst)))
      (print (cons file-name (ignore-errors
			       (apply (cl-case func-symbol
					('add-database #'yabai/server-add-database)
					('get-options  #'yabai/server-get-options))
				      arg-lst)))))))

;;;;====================================================================================
;;;; Entry point
;;;;====================================================================================
(while t
  (yabai/server-mainloop))

;;; yabai-server.el ends here

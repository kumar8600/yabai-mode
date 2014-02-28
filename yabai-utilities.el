;;; yabai-utilities.el --- yabai-mode backend for implementation utility

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

(require 'yabai-definitions)

;;;; Utilities =====================================================================
(defconst yabai/expand-file-name/regexp "^/\\([c-z]\\)\\(/\\|$\\)")
(defun yabai/expand-file-name (filename &optional directory)
  "Expand file name with 'expand-file-name.  With considering windows makefile.

FILENAME DIRECTORY are passed for 'expand-file-name."
  (when (eq system-type 'windows-nt)
    (setq filename (replace-regexp-in-string yabai/expand-file-name/regexp
					     "\\1:/"
					     filename))
    (when directory
      (setq directory (replace-regexp-in-string yabai/expand-file-name/regexp
						"\\1:/"
						directory))))
  (expand-file-name filename directory))

(defun yabai/get-time-last-modified (filepath)
  "Get the time last modified of file at FILEPATH."
  (nth 5 (file-attributes filepath)))

(defun yabai/file-name-equal (a b)
  "Compare file-name A with B."
  (cl-flet ((format (fname)
		    (directory-file-name
		     (expand-file-name fname))))
    (string= (format a)
	     (format b))))
;;;; =======================================================================================
;;;; Build system defining API
;;;; =======================================================================================
(defun yabai/define-build-system (name-symbol config-file pre-process database-path-getter compilation &optional build-tree-getter)
  "Add build system (i.e. CMake) named NAME-SYMBOL.

CONFIG-FILE is string describing name of default build configuration file
of the build system (e.g. CMakeLists.txt).
PRE-PROCESS is called before to get compiler options by server.
Database compilation server is passed database path from DATABASE-PATH-GETTER.
If there are no PRE-PROCESS, please pass nil.
When build, COMPILATION is called.
When run, RUNNNING-DIR-FUNC is called to get path to build tree.
If nil, BUILD-TREE-GETTER is path to source-tree.

PRE-PROCESS is function takes arguments
 `build-config' `source-tree' `finish-func'.
DATABASE-PATH-GETTER is function takes arguments
 `build-config' `build-tree'.
COMPILATION is function takes arguments
 `build-config' `finish-func'.
BUILD-TREE-GETTER is function takes no arguments.

`build-config' is string describing file path to build config file
 (i.e CMakeLists.txt).
`source-tree' is string describing file path to directory source tree begin.
`finish-func' is function you must call when your function is finished.
 (Be careful)."

  (let ((func-alist (list (cons 'pre-process    (or pre-process
						    #'yabai/dummy-pre-process))
			  (cons 'config-file          config-file)
			  (cons 'database-path-getter database-path-getter)
			  (cons 'compilation          compilation)
			  (cons 'build-tree-getter    (or build-tree-getter
							  (lambda ()
							    (eval 'yabai/path-source-tree)))))))
    (add-to-list 'yabai/build-system-definitions (cons name-symbol func-alist))))

;;;; Utilities
(defun yabai/build-system-get-config-file-name (target)
  "Get TARGET's config file name from BUILD-SYSTEMS."
  (cdr (assoc 'config-file (cdr (assoc target yabai/build-system-definitions)))))

(defun yabai/build-system-get-pre-process-func (target)
  "Get TARGET's pre process function from BUILD-SYSTEMS."
  (cdr (assoc 'pre-process (cdr (assoc target yabai/build-system-definitions)))))

(defun yabai/build-system-get-database-path-getter-func (target)
  "Get TARGET's database path getter function from BUILD-SYSTEMS."
  (cdr (assoc 'options-getter (cdr (assoc target yabai/build-system-definitions)))))

(defun yabai/build-system-get-compilation-func (target)
  "Get TARGET's compilation function from BUILD-SYSTEMS."
  (cdr (assoc 'compilation (cdr (assoc target yabai/build-system-definitions)))))

(defun yabai/build-system-get-build-tree-getter-func (target)
  "Get TARGET's build tree getter function from BUILD-SYSTEMS."
  (cdr (assoc 'build-tree-getter (cdr (assoc target yabai/build-system-definitions)))))

(defun yabai/build-system-get-database-path-current-buffer ()
  "Get compilation database path used from current buffer."
  (let ((database-path-getter (yabai/build-system-get-database-path-getter-func yabai/build-system-in-local-buffer))
	(build-tree-getter (yabai/build-system-get-build-tree-getter-func yabai/build-system-in-local-buffer)))
    (funcall database-path-getter
	     yabai/path-build-config-file
	     (funcall build-tree-getter))))

(defun yabai/get-possible-config-file-name-list ()
  "Return a list of possible build config file names."
  (if (eq yabai/build-system
	  'auto)
      (mapcar (lambda (item)
		(cdr (assoc 'config-file item)))
	      yabai/build-system-definitions)
    (or (list (yabai/build-system-get-config-file-name yabai/build-system))
	(error "Unknown build system is set"))))

(defun yabai/dummy-pre-process (build-config source-tree finish-func)
  "Just call finish-func.

BUILD-SYSTEM, BUILD-CONFIG, SOURCE-TREE and FINISH-FUNC are arguments."
  (funcall finish-func nil nil))

;;;; =======================================================================================
;;;; Package integration defining API
;;;; =======================================================================================
(defmacro yabai/define-integrator (name-symbol arg-mode-symbol &rest body)
  "Add compiler options setter for other package.

NAME-SYMBOL is identifer.  Don't duplicate it.
ARG-MODE-SYMBOL is mode switcher of arguments of BODY.
BODY is your definition.
If ARG-MODE is `individuals', in BODY, you can use variables:
  `list-definitions'   - a list of definitions
  `list-include-paths' - a list of include paths
  `list-includes'      - a list of additional includes
  `list-warnings'      - a list of warning settings
  `language-standard'  - a string value of language standard
  `standard-library'   - a string value of standard language
  `flag-ms-extensions' - a t or non-nil flag whether enable ms extensions
  `flag-no-rtti'       - a t or non-nil flag whether enable no rtti
If ARG-MODE is `list', in BODY, you can use variables:
  `options-list'       - a list of parameters
If ARG-MODE is `string', in BODY, you can use variables:
  `options-string'     - a string of parameters

And user customizable variable `yabai/with-NAME-SYMBOL' is defined.
If this variable is nil, this definition will not be used."
  `(progn (add-to-list 'yabai/integration-definitions
		       (cons ,name-symbol ,(cl-case (eval arg-mode-symbol)
					      ('individuals `(lambda (options)
							      (let ((list-definitions   (yabai/options-definitions options))
								    (list-include-paths (yabai/options-include-paths options))
								    (list-includes      (yabai/options-includes options))
								    (list-warnings       (yabai/options-warnings options))
								    (language-standard  (yabai/options-language-standard options))
								    (standard-library   (yabai/options-standard-library options))
								    (flag-ms-extensions (yabai/options-ms-extension options))
								    (flag-no-rtti       (yabai/options-no-rtti options)))
								,@body)))
					      ('list `(lambda (options)
							(let ((options-list (yabai/options-to-list options)))
							  ,@body)))
					      ('string `(lambda (options)
							  (let ((options-string (yabai/options-to-string options)))
							    ,@body))))))
	  (yabai/def-option-var ,(intern (concat "yabai/with-" (symbol-name (eval name-symbol)))) t
			       (format "Set non-nil if you want to use %s as checker." ,(symbol-name (eval name-symbol))))))

;;;; Searching
(defun yabai/find-at-upper-or-current-p (pred dir-begin)
  "Find a something satisfying PRED from DIR-BEGIN to root directory.

A function PRED called with one argument directory searching.
And PRED must return file name what found, or nil."
  (cl-labels ((f (dir)
	       (let ((result (funcall pred dir)))
		 (if result
		     (expand-file-name result
				       (file-name-as-directory dir))
		   (let ((parent (file-name-directory (directory-file-name dir))))
		     (if (not (string= dir (expand-file-name "/")))
			 (f parent)
		       nil))))))
    (f (expand-file-name dir-begin))))
  
(defun yabai/file-exists-at-upper-or-current-p (fnames dir-begin)
  "Find a list FNAMES from DIR-BEGIN to root directory.

Return path where found one of the FNAME."
  (yabai/find-at-upper-or-current-p
   (lambda (dir)
     (cl-find-if (lambda (fname)
		   (let ((obj (expand-file-name
			       fname
			       (file-name-as-directory dir))))
		     (and (file-exists-p obj)
			  (not (file-directory-p obj)))))
		 fnames))
   dir-begin))

(defun yabai/directory-exists-at-upper-or-current-p (dnames dir-begin)
  "Find a directory name list DNAMES from DIR-BEGIN to root directory.

Return path where found one of the FNAME."
  (yabai/find-at-upper-or-current-p
   (lambda (dir)
     (cl-find-if (lambda (dname)
		   (let ((obj (expand-file-name
			       dname
			       (file-name-as-directory dir))))
		     (and (file-exists-p obj)
			  (file-directory-p obj))))
		 dnames))
   dir-begin))

;;;; Finding source tree
(defun yabai/find-build-config-file (fnames dir-begin)
  "Find build file list FNAMES from a directory DIR-BEGIN to root directory.

Return path as string if found.  Return nil if not."
  (yabai/file-exists-at-upper-or-current-p fnames
					   dir-begin))

;;;;
;;;; Converting string to yabai/options object
;;;;
(defun yabai/string-matches (regexp str count)
  "Return a list having all result of 'string-match with REGEXP STR.

COUNT is passed for 'match-string.
Case sensitive search is enabled."
  (let ((case-fold-search nil))
    (cl-labels ((f (pnt acc)
		   (let ((pnt-cur (string-match regexp str pnt)))
		     (if pnt-cur
			 (f (match-end count) (cons (match-string count str)
						 acc))
		       acc))))
      (f 0 nil))))

(defun yabai/string-matches-definition-option (opt str)
  "Return a list of compiler option OPT's definitions appeared in STR."
  (yabai/string-matches (format "\\(^\\| +\\)%s *\\(\\S-+\\)"
				    opt)
			    str 2))

(defun yabai/string-matches-file-option (opt str working-dir)
  "Return a list of compiler option OPT's values appeared in STR.

Results are expanded from WORKING-DIR."
  (let ((matches (yabai/string-matches (format "\\(^\\| +\\)%s *\\(\\(\\\\ \\|\\S-+\\)+\\)"
					      opt)
				      str 2)))
    (mapcar (lambda (item)
	      (yabai/expand-file-name item
				working-dir))
	    matches)))
  
(defun yabai/string-matches-value-option (opt str)
  "Return a compiler option OPT's values appeared in STR."
  (car (yabai/string-matches (format "\\(^\\| +\\)%s=\\(\\S-+\\)"
				    opt)
			    str 2)))

(defun yabai/string-matches-flag-option (opt str)
  "Return t if there is OPT in STR."
  (if (car (yabai/string-matches (format "\\(^\\| +\\)%s"
					opt)
				str 0))
      t
    nil))

(defun yabai/string-to-compiler-option-object (str working-dir)
  "Convert STR to compiler option object (yabai/options).

When converting, file paths will be expanded with WORKING-DIR."
  (make-yabai/options :definitions       (yabai/string-matches-definition-option "-D" str)
		     :include-paths     (yabai/string-matches-file-option "-I" str working-dir)
		     :includes          (yabai/string-matches-file-option "-include" str working-dir)
		     :language-standard (or (yabai/string-matches-value-option "-std" str)
					    (when (yabai/string-matches-flag-option "-ansi" str)
					      "c89"))
		     :standard-library  (yabai/string-matches-value-option "-stdlib" str)
		     :ms-extension      (yabai/string-matches-flag-option "-fms-extensions" str)
		     :no-rtti           (yabai/string-matches-flag-option "-fno-rtti" str)))

;;;; Parse Makefile generated by CMake
(defun yabai/msbuild-insert-rsp (str working-dir)
  "Return string is inserted rsp file to STR.

WORKING-DIR is used to solve relative path."
  (cl-labels ((f ()
		 (if (re-search-forward "@\\(.*.rsp\\)"
					nil
					t
					1)
		     (let* ((rsp-path (expand-file-name (match-string 1)
							working-dir))
			    (rsp-content (save-match-data
					   (with-temp-buffer
					     (insert-file-contents rsp-path)
					     (replace-regexp-in-string "\n" " " (buffer-string))))))
		       (replace-match rsp-content)
		       (f))
		   t)))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (f)
      (goto-char (point-min))
      (replace-regexp-in-string " +" " " (buffer-string)))))


;;;; Converting compiler option to List =====================================================================
(defun yabai/format-option (prefix opt)
  "Put PREFIX for compiler option (OPT)."
  (if opt
      (list (concat prefix opt))
    nil))

(defun yabai/format-option-list (prefix lst)
  "Put PREFIX for each compiler option in LST."
  (mapcar (lambda (opt)
	       (concat prefix opt))
	     lst))

(defun yabai/format-option-flag (opt-name flag)
  "Return OPT-NAME if FLAG is not nil."
  (if flag
      (list opt-name)
    nil))

(defun yabai/options-to-list (options)
  "Convert compiler OPTIONS to list."
  (cl-concatenate 'list
		  (yabai/format-option "-std="
				      (yabai/options-language-standard
				       options))
		  (yabai/format-option "-stdlib="
				      (yabai/options-standard-library
				       options))
		  (yabai/format-option-flag "-fms-extensions"
					   (yabai/options-ms-extension
					    options))
		  (yabai/format-option-flag "-fno-rtti"
					   (yabai/options-no-rtti
					    options))
		  (yabai/format-option-list "-include"
					   (yabai/options-includes
					    options))
		  (yabai/format-option-list "-W"
					   (yabai/options-warnings
					    options))
		  (yabai/format-option-list "-D"
					   (yabai/options-definitions
					    options))
		  (yabai/format-option-list "-I"
					   (yabai/options-include-paths
					    options))))

;;;; Converting compiler option to string ===========================================
(defun yabai/options-to-string (options)
  "Convert compiler OPTIONS to string."
  (mapconcat 'identity
	     (yabai/options-to-list options)
	     " "))

;;;; Setting buffer local variables about config file, source tree, build tree
(defun yabai/get-build-system-symbol (build-config-file)
  "Return build system symbol is user defined or guess from BUILD-CONFIG-FILE."
  (if (eq 'auto
	  yabai/build-system)
      (let ((fname (file-name-nondirectory build-config-file)))
	(or (cl-find-if (lambda (item)
			  (if (string= fname
				       (cdr (assoc 'config-file item)))
			      t
			    nil))
			yabai/build-system-definitions)
	    (error "Can't detect build system from `%s'" build-config-file)))
    yabai/build-system))

(defun yabai/is-string-event-finished (event)
  "If string EVENT means finished, return non-nil."
  (string= "finished\n"
	   event))

(defun yabai/compile-impl (command finish-func)
  "Compile something with COMMAND.

Finally, FINISH-FUNC will be called."
  (let ((proc (start-process-shell-command "emacs-yabai-compile"
					   yabai/build-buffer-name
					   command)))
    (yabai/puthash-process->buffer proc (current-buffer))
    (pop-to-buffer yabai/build-buffer-name)
    (setq buffer-read-only t)
    (local-set-key "q" #'quit-window)
    (when finish-func
      (set-process-sentinel proc finish-func))))

(provide 'yabai-utilities)

;;; yabai-utilities.el ends here

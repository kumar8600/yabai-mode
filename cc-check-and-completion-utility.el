;;; cc-check-and-completion-utility.el --- set options automatically for modern check and completion packages (i.e. flycheck, company-mode).

;; Author: kumar8600 <kumar8600@gmail.com>
;; URL: https://github.com/kumar8600/cc-check-and-completion-utility
;; Version: 0.01
;; Package-Requires: ((cl-lib) (json))

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

;; Minor mode for auto configuration of C/C++ syntax checking and completion plugins.

;;
;; Basic setup:
;;
;;   (add-to-list 'load-path
;;                "~/path-to-cc-check-and-completion-utility")
;;   (require 'cc-check-and-completion-utility)
;;
;;   (add-hook 'c-mode-hook 'cccc-mode)
;;   (add-hook 'c++-mode-hook 'cccc-mode)
;;
;; Notice:
;;
;;   Now, it doesn't have any functions to set compile options manually.

;;; Code:

(require 'cl-lib)
(require 'json)

(defmacro cccc/def-option-var (symbol value doc-string &rest rest)
  "Define custom variable grouped in cccc-options.

Variable SYMBOL with VALUE documented with DOC-STRING and REST for defcustom."
  `(defcustom ,symbol ,value
     ,doc-string
     :group 'cccc-options
     ,@rest))

;;;; Flags to enable cooperating with checker and completer ========================

;;;; Checker
(cccc/def-option-var cccc/with-checker-flycheck t
		     "Set non-nil if you want to use flycheck as checker.

See URL `https://github.com/flycheck/flycheck'")

;;;; Completer
(cccc/def-option-var cccc/with-completer-company t
		     "Set non-nil if you want to use company as checker.

See URL `http://company-mode.github.io/'")

(cccc/def-option-var cccc/with-completer-ac-clang-async t
		     "Set non-nil if you want to use auto-complete-clang-async as checker.

See URL `https://github.com/Golevka/emacs-clang-complete-async'")

;;;; Build tree directory name is used when find automatically ====================

(cccc/def-option-var cccc/cmake-build-tree-name "build"
		     "CMake's build tree name used when build-tree searching and creating.")

;;;; Compile options =================================================================
(cl-defstruct cccc/options
  "A struct describing compile options"
  (definitions nil           "Additional preprocessor definitions for the compiler.")
  (include-paths nil         "A list of include search path for the compiler.")
  (includes nil              "A list of additional include files for the compiler.")
  (warnings '("all" "extra") "A list of additional warnings to enable in the compiler.")
  (language-standard nil     "The language standard to use in the compiler.")
  (standard-library nil	     "Whether to disable RTTI in the compiler.")
  (ms-extension nil          "Whether to enable Microsoft extensions in the compiler.")
  (no-rtti nil               "Whether to disable RTTI in the compiler."))

;; Example
;;   (make-cccc/options :definitions nil
;; 			:include-paths '("../")
;; 			:includes nil
;; 			:warnings '("all" "extra")
;; 			:language-standard "c++11"
;; 			:standard-library nil
;; 			:ms-extension nil
;; 			:no-rtti nil))

;;;; Variables to additional setting compile options manually
;; (cccc/def-option-var cccc/definitions nil
;; 		     "Additional preprocessor definitions for the compiler.")

;; (cccc/def-option-var cccc/include-path nil
;; 		     "A list of include search path for the compiler.")

;; (cccc/def-option-var cccc/includes nil
;; 		     "A list of additional include files for the compiler.")

;; (cccc/def-option-var cccc/language-standard nil
;; 		     "The language standard to use in the compiler.")

;; (cccc/def-option-var cccc/ms-extensions nil
;; 		     "Whether to enable Microsoft extensions in the compiler.")

;; (cccc/def-option-var cccc/no-rtti nil
;; 		     "Whether to disable RTTI in the compiler.")

;; (cccc/def-option-var cccc/standard-library nil
;; 		     "The standard library to use for the compiler.")

;; (cccc/def-option-var cccc/warnings '("all" "extra")
;; 		     "A list of additional warnings to enable in the compiler.")

;;;; Const Variables ===============================================================
(defconst cccc/cmake-generation-buffer-name "*cccc-cmake*")
(defconst cccc/build-buffer-name "*cccc-build*")

;;;; Variables for implementation ==================================================
(defvar-local cccc/stored-compile-options nil)
(defvar-local cccc/path-source-tree nil)
(defvar-local cccc/path-build-tree nil)
(defvar-local cccc/path-build-config-file nil)
(defvar-local cccc/time-last-mod-build-config-file nil)

;; process=>buffer hash table made to use buffer local variables in async sentinel.
(defvar cccc/process->buffer-table (make-hash-table :test 'equal))
(defun cccc/puthash-process->buffer (key val)
  "Associate KEY with VAL for global process->buffer-table."
  (puthash key val cccc/process->buffer-table))
(defun cccc/gethash-process->buffer (key)
  "Get KEY's value from global process->buffer-table."
  (gethash key cccc/process->buffer-table))

;;;; Utilities =====================================================================
(defconst cccc/expand-file-name/regexp "^/\\([c-z]\\)\\(/\\|$\\)")
(defun cccc/expand-file-name (filename &optional directory)
  "Expand file name with 'expand-file-name.  With considering windows makefile.

FILENAME DIRECTORY are passed for 'expand-file-name."
  (when (eq system-type 'windows-nt)
    (setq filename (replace-regexp-in-string cccc/expand-file-name/regexp
					     "\\1:/"
					     filename))
    (when directory
      (setq directory (replace-regexp-in-string cccc/expand-file-name/regexp
						"\\1:/"
						directory))))
  (expand-file-name filename directory))

(defun cccc/file-name-equal (a b)
  "Compare A with B file-name."
  (cl-flet ((format (fname)
		    (directory-file-name
		     (expand-file-name fname))))
    (string= (format a)
	     (format b))))

(defun cccc/get-time-last-modified (filepath)
  "Get the time last modified of file at FILEPATH."
  (nth 5 (file-attributes filepath)))

;;;; Searching ====================================================================

;;;; Utilities
(defun cccc/find-at-upper-or-current-p (pred dir-begin)
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
  
(defun cccc/file-exists-at-upper-or-current-p (fnames dir-begin)
  "Find a list FNAMES from DIR-BEGIN to root directory.

Return path where found one of the FNAME."
  (cccc/find-at-upper-or-current-p
   (lambda (dir)
     (cl-find-if (lambda (fname)
		   (let ((obj (expand-file-name
			       fname
			       (file-name-as-directory dir))))
		     (and (file-exists-p obj)
			  (not (file-directory-p obj)))))
		 fnames))
   dir-begin))

(defun cccc/directory-exists-at-upper-or-current-p (dnames dir-begin)
  "Find a directory name list DNAMES from DIR-BEGIN to root directory.

Return path where found one of the FNAME."
  (cccc/find-at-upper-or-current-p
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
(defun cccc/find-build-config-file (dir-begin)
  "Find build configuration file from a directory DIR-BEGIN to root directory.

Return path as string if found.  Return nil if not."
  (cccc/file-exists-at-upper-or-current-p '(;; Now available only CMake
					    "CMakeLists.txt"
					    "cmakelists.txt")
					  dir-begin))

;;;; Checking build tree
(defun cccc/cmake-check-build-tree (build-tree src-tree)
  "Check BUILD-TREE made by SRC-TREE is valid."
  (let ((cache-path (expand-file-name "CMakeCache.txt" build-tree))
	(makefile-path (expand-file-name "Makefile" build-tree)))
    ;; Check this build tree is having CMakeCache.txt.
    (unless (file-exists-p cache-path)
      (error "In '%s', there aren't CMakeCache.txt" build-tree))
    ;; Check this build tree is taking source tree starts with DIR-BEGIN.
    (unless (cccc/file-name-equal
	     src-tree
	     (cccc/cmake-cache-get-value "CMAKE_HOME_DIRECTORY:INTERNAL"
					 cache-path))
      (error "'%s' is not build-tree made for source-tree '%s'"
	     build-tree
	     src-tree))
    ;; Check this build tree was made for Make.
    (unless (file-exists-p makefile-path)
      (error "'%s' is not build-tree made for Make" build-tree))
    build-tree))

;;;; Finding build tree
(defun cccc/cmake-find-build-tree (dir-begin)
  "Find build tree made by cmake from DIR-BEGIN to root."
  (let ((build-tree (cccc/directory-exists-at-upper-or-current-p (list cccc/cmake-build-tree-name) dir-begin)))
    (unless build-tree
      (error "From '%s', can't find build-tree" dir-begin))
    build-tree))

;;;; Finding build target
(defun cccc/cmake-find-first-target-config (build-tree)
  "Find first executable target from BUILD-TREE."
  (let* ((cmakefiles (expand-file-name "CMakeFiles" build-tree))
	 (lst (directory-files cmakefiles)))
    (expand-file-name (cl-find-if (lambda (item)
				    (string-match "^.*\\.dir$"
						  item))
				  lst)
		      cmakefiles)))

;;;; Guessing compile options ======================================================
;;;; CMakeLists.txt Parsing
;; Note:
;; No need to parse CMakeLists.txt.
;; Because in cmake, even if you defined multiple executable target,
;; a same compile option will be used for all target.

;;;; CMakeCache.txt Parsing
(defun cccc/cmake-cache-get-value (variable-name cache-path)
  "Get the value of VARIABLE-NAME from CMakeCache.txt at CACHE-PATH."
  (condition-case err-var
      (with-temp-buffer
	(insert-file-contents cache-path)
	(re-search-forward (format "\\(%s\\)[ \t]*=[ \t]*\\(.+\\)"
				   variable-name)
			   nil t)
	;; return value
	(match-string 2))
    (file-error nil)))

;;;; Build files Generation
(defun cccc/cmake-generate-build-files-impl (src-tree build-tree &optional finish-func)
  "Execute cmake takes sources at SRC-TREE at BUILD-TREE.

When works over, FINISH-FUNC will be called."
  (let ((generator (if (eq system-type 'windows-nt)
		       (if (executable-find "sh")
			   "MSYS Makefiles"
			 "MinGW Makefiles")
		     "Unix Makefiles")))
    (let ((prc (start-process-shell-command "emacs-cccc-cmake"
					    cccc/cmake-generation-buffer-name
					    (format "cd %s && cmake %s -G \"%s\""
						    (cccc/expand-file-name build-tree)
						    (cccc/expand-file-name src-tree)
						    generator))))
      ;;(pop-to-buffer cccc/cmake-generation-buffer-name)
      (when finish-func
	(cccc/puthash-process->buffer prc (current-buffer))
	(set-process-sentinel prc finish-func)))))

(defun cccc/cmake-choose-build-tree (&optional dir)
  "Return build-tree path DIR is user choised.

If DIR isn't exists, make it."
  (unless dir
    (setq dir (read-file-name (format "Please select build-tree (named `%s'):" cccc/cmake-build-tree-name)
			      (file-name-as-directory cccc/path-source-tree))))
  (unless (string= cccc/cmake-build-tree-name
		   (file-name-nondirectory (directory-file-name dir)))
    (error "Build tree is must named `%s'" cccc/cmake-build-tree-name))
  (if (file-exists-p dir)
      (when (not (file-directory-p dir))
	(error "Can't create build tree in `%s'" dir))
    ;; mkdir with confirmation
    (call-interactively (lambda (&optional reply)
			  (interactive "cDirectory is not exists. Do you want to create?(y/n):")
			  (if (equal reply ?y)
			      (make-directory dir)
			    (error "Build tree is not created")))))
  dir)

(defun cccc/cmake-find-or-choose-build-tree (src-tree)
  "Return build-tree path detected (from SRC-TREE) or user chosen."
  (or (ignore-errors (cccc/cmake-check-build-tree
		      (cccc/cmake-find-build-tree src-tree)
		      src-tree))
      (cccc/cmake-choose-build-tree)))

(defun cccc/cmake-generate-build-files (&optional src-tree build-tree finish-func)
  "Execute cmake at guessed build tree for guessed source tree.

If SRC-TREE or BUILD-TREE defined, will use them.
If not, will guess them.
When works over, FINISH-FUNC will be called."
  (interactive)
  (unless src-tree
    (setq src-tree (file-name-directory (cccc/find-build-config-file (file-name-directory (buffer-file-name))))))
  (unless build-tree
    (setq build-tree (cccc/cmake-find-or-choose-build-tree src-tree)))
  (cccc/cmake-generate-build-files-impl src-tree build-tree finish-func))

;;;; Converting string to cccc/options object
(defun cccc/string-matches (regexp str count)
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

(defun cccc/string-matches-definition-option (opt str)
  "Return a list of compile option OPT's definitions appeared in STR."
  (cccc/string-matches (format "\\(^\\| +\\)%s *\\(\\S-+\\)"
				    opt)
			    str 2))

(defun cccc/string-matches-file-option (opt str working-dir)
  "Return a list of compile option OPT's values appeared in STR.

Results are expanded from WORKING-DIR."
  (let ((matches (cccc/string-matches (format "\\(^\\| +\\)%s *\\(\\(\\\\ \\|\\S-+\\)+\\)"
					      opt)
				      str 2)))
    (mapcar (lambda (item)
	      (cccc/expand-file-name item
				working-dir))
	    matches)))
  
(defun cccc/string-matches-value-option (opt str)
  "Return a compile option OPT's values appeared in STR."
  (car (cccc/string-matches (format "\\(^\\| +\\)%s=\\(\\S-+\\)"
				    opt)
			    str 2)))

(defun cccc/string-matches-flag-option (opt str)
  "Return t if there is OPT in STR."
  (if (car (cccc/string-matches (format "\\(^\\| +\\)%s"
					opt)
				str 0))
      t
    nil))

(defun cccc/string-to-compile-option-object (str working-dir)
  "Convert STR to compile option object (cccc/options).

When converting, file paths will be expanded with WORKING-DIR."
  (make-cccc/options :definitions       (cccc/string-matches-definition-option "-D" str)
		     :include-paths     (cccc/string-matches-file-option "-I" str working-dir)
		     :includes          (cccc/string-matches-file-option "-include" str working-dir)
		     :language-standard (or (cccc/string-matches-value-option "-std" str)
					    (when (cccc/string-matches-flag-option "-ansi" str)
					      "c89"))
		     :standard-library  (cccc/string-matches-value-option "-stdlib" str)
		     :ms-extension      (cccc/string-matches-flag-option "-fms-extensions" str)
		     :no-rtti           (cccc/string-matches-flag-option "-fno-rtti" str)))

;;;; Parse Makefile generated by CMake
(defun cccc/makefile-print-vars-as-json (target vars)
  "Return string to print TARGET's values named a list VARS by Make as JSON."
  (format "include %s
cccc-print-vars:
\t@echo {%s}"
	  target
	  (mapconcat (lambda (var)
		       (format "\\\"%s\\\": \\\"${%s}\\\""
			       var var))
		     vars
		     ", ")))

(defun cccc/msbuild-insert-rsp (str working-dir)
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

(defun cccc/makefile-get-values (vars makefile-path build-tree-path)
  "Return VARS' values in makefile at MAKEFILE-PATH.

Make is called from BUILD-TREE-PATH
MSBuild .rsp file will be inserted if there is '@file/path.rsp'."
  (let ((temp-file-path (make-temp-name (expand-file-name "emacs-cccc"
							  temporary-file-directory))))
    (with-temp-file temp-file-path
      (insert (cccc/makefile-print-vars-as-json makefile-path
					       vars)))
    (let ((dump-str (cccc/msbuild-insert-rsp
		     (shell-command-to-string (format "cd %s && make -f %s cccc-print-vars"
						      (expand-file-name build-tree-path)
						      temp-file-path))
		     build-tree-path)))
      (json-read-from-string dump-str))))

(defun cccc/cmake-parse-options-from-executable-target-config (target-config-path build-tree-path)
  "Guess compile options from TARGET-CONFIG-PATH and BUILD-TREE-PATH.

Return the object cccc/options.

Note:
Guessing way is parsing `TARGET-CONFIG-PATH/flags.make`.
If build tree was made for Makefiles, it will be success.
I don't know more clean way.  If you know, tell me please."
  (let ((flags-make-path (expand-file-name "flags.make" target-config-path)))
    ;; parse flags.make
    (cccc/string-to-compile-option-object
     (mapconcat #'cdr
		(cccc/makefile-get-values '("CXX_FLAGS" "CXX_DEFINES" "C_FLAGS" "C_DEFINES")
					  flags-make-path
					  build-tree-path)
		" ")
     build-tree-path)))

;;;; Interface
(defun cccc/cmake-guess-options (build-tree-path)
  "From build-tree at BUILD-TREE-PATH, guess compile options.

SRC-FILE-PATH is path to the file you are opening."
  ;; find executable target's config path.
  (let* ((target-config-path (cccc/cmake-find-first-target-config build-tree-path)))
    ;; Finally, parse a `flags.make` in build tree.
    (cccc/cmake-parse-options-from-executable-target-config
     target-config-path
     build-tree-path)))

;;;; Converting compile option to List =============================================
(defun cccc/format-option (prefix opt)
  "Put PREFIX for compile option (OPT)."
  (if opt
      (list (concat prefix opt))
    nil))

(defun cccc/format-option-list (prefix lst)
  "Put PREFIX for each compile option in LST."
  (mapcar (lambda (opt)
	       (concat prefix opt))
	     lst))

(defun cccc/format-option-flag (opt-name flag)
  "Return OPT-NAME if FLAG is not nil."
  (if flag
      (list opt-name)
    nil))

(defun cccc/options-to-list (options)
  "Convert compile OPTIONS to list."
  (cl-concatenate 'list
		  (cccc/format-option "-std="
				      (cccc/options-language-standard
				       options))
		  (cccc/format-option "-stdlib="
				      (cccc/options-standard-library
				       options))
		  (cccc/format-option-flag "-fms-extensions"
					   (cccc/options-ms-extension
					    options))
		  (cccc/format-option-flag "-fno-rtti"
					   (cccc/options-no-rtti
					    options))
		  (cccc/format-option-list "-include"
					   (cccc/options-includes
					    options))
		  (cccc/format-option-list "-W"
					   (cccc/options-warnings
					    options))
		  (cccc/format-option-list "-D"
					   (cccc/options-definitions
					    options))
		  (cccc/format-option-list "-I"
					   (cccc/options-include-paths
					    options))))

;;;; Converting compile option to string ===========================================
(defun cccc/options-to-string (options)
  "Convert compile OPTIONS to string."
  (mapconcat 'identity
	     (cccc/options-to-list options)
	     " "))

;;;; Setting options ============================================================
;; Note:
;;   I want to use `custom-set-variables' but it can't set buffer local value.
;;   Do you have any idea better? Please tell me.

;;;; Setting options for syntax checkers
(defun cccc/set-options-to-flycheck (options)
  "Set compile OPTIONS to flycheck."
  (when (require 'flycheck nil t)
    (setq-local flycheck-clang-definitions       (cccc/options-definitions options))
    (setq-local flycheck-clang-include-path      (cccc/options-include-paths options))
    (setq-local flycheck-clang-includes          (cccc/options-includes options))
    (setq-local flycheck-clang-language-standard (cccc/options-language-standard options))
    (setq-local flycheck-clang-ms-extensions     (cccc/options-ms-extension options))
    (setq-local flycheck-clang-no-rtti           (cccc/options-no-rtti options))
    (setq-local flycheck-clang-standard-library  (cccc/options-standard-library options))
    (setq-local flycheck-clang-warnings          (cccc/options-warnings options))
    ;; reflesh checking
    (flycheck-buffer)
    options))

;;;; Setting options for completers
(defun cccc/set-options-to-company (options)
  "Set compile OPTIONS to company."
  (when (require 'company nil t)
    (setq-local company-clang-arguments (cccc/options-to-list options))
    options))

(defun cccc/set-options-to-ac-clang-async (options)
  "Set copile OPTIONS to auto-complete-clang-async."
  (when (require 'auto-complete-clang-async nil t)
    (setq-local ac-clang-cflags (cccc/options-to-list options))
    options))

;;;; Setting options to *
(defun cccc/set-options-to-* (options)
  "Set compile OPTIONS to syntax checker and completers."
  ;; Checkers
  (when cccc/with-checker-flycheck
    (cccc/set-options-to-flycheck options))

  ;; Completers
  (when cccc/with-completer-company
    (cccc/set-options-to-company options))
  (when cccc/with-completer-ac-clang-async
    (cccc/set-options-to-ac-clang-async options)))

;;;; Setting buffer local variables about config file, source tree, build tree
(defun cccc/search-or-ask-paths-buffer-local ()
  "Set buffer local variables about paths.

`cccc/path-build-config-file'
`cccc/path-source-tree'
`cccc/path-build-tree'
will be written."
  (let ((dir-begin (file-name-directory (buffer-file-name))))
    ;;
    ;; set special buffer local variables
    ;;
    ;; search build config file
    (setq-local cccc/path-build-config-file (cccc/find-build-config-file dir-begin))
    (if cccc/path-build-config-file
	(progn
	  ;; search or create build-tree
	  (setq-local cccc/path-source-tree (file-name-directory
					     cccc/path-build-config-file))
	  (setq-local cccc/path-build-tree (cccc/cmake-find-or-choose-build-tree
					    cccc/path-source-tree)))
      ;; if there are no build config file, do nothing.
      nil)))

(defun cccc/is-set-paths-buffer-local ()
  "If buffer local variables about paths are set, return t.

`cccc/path-build-config-file'
`cccc/path-source-tree'
`cccc/path-build-tree'
will be tested."
  (if (and cccc/path-build-config-file
	   cccc/path-source-tree
	   cccc/path-build-tree
	   cccc/time-last-mod-build-config-file)
      t
    nil))


;;;; Gluing guessing and setting ================================================
(defun cccc/guess-file-type (file-path)
  "Return file-type symbol is guessed from FILE-PATH."
  'cmakefile)

(defun cccc/guess-options-from-* (file-path build-tree-path)
  "Return options are guessed from build configuration file (FILE-PATH).

BUILD-TREE-PATH means build tree path.
If failure to guess what build system used, return nil."
  (let ((file-type (cccc/guess-file-type file-path)))
    (cl-case file-type
      ('cmakefile (cccc/cmake-guess-options build-tree-path))
      (otherwise nil))))

(defun cccc/is-string-event-finished (event)
  "If string EVENT means finished, return non-nil."
  (string= "finished\n"
	   event))

(defun cccc/guess-and-set-options ()
  "Generate build tree and guess compile options and set it."
  ;; generate build-tree
  (cccc/cmake-generate-build-files
   cccc/path-source-tree
   cccc/path-build-tree
   (lambda (process event)
     (with-current-buffer (cccc/gethash-process->buffer process)
       (unless (cccc/is-string-event-finished event)
	 (error "CMake failed"))
       ;; guess options
       (let ((options (or (cccc/guess-options-from-* cccc/path-build-config-file
						     cccc/path-build-tree)
			  (error "There are no options guessed from %s" cccc/path-source-tree))))
	 ;; store guessed options
	 (setq-local cccc/stored-compile-options options)
	 ;; store build config file's time last modified
	 (setq-local cccc/time-last-mod-build-config-file (cccc/get-time-last-modified
							   cccc/path-build-config-file))
	 ;; set guessed options
	 (cccc/set-options-to-* options))))))

;;;; User interfaces ============================================================

;;;; Setting compile options
(defun cccc/load-options ()
  "Set options are guessed from your build configuration file."
  (interactive)
  ;;
  ;; set special buffer local variables
  ;;
  (if (cccc/search-or-ask-paths-buffer-local)
      (cccc/guess-and-set-options)
    ;; If paths don't set, do nothing.
    nil))

(defun cccc/reload-options ()
  "Reload compile options from paths had already defined."
  (interactive)
  (unless (cccc/is-set-paths-buffer-local)
    (error "Please call `cccc/load-options' first"))
  (cccc/guess-and-set-options))

(defun cccc/reload-options-if-build-config-is-updated ()
  "If build config file is not one last loaded, reload."
  (if cccc/time-last-mod-build-config-file
      (unless (equal cccc/time-last-mod-build-config-file
		     (cccc/get-time-last-modified cccc/path-build-config-file))
	(cccc/reload-options))
    ;; if `cccc/time-last-mod-build-config-file' is nil, do nothing.
    nil))

;;;; define as minor mode
(defconst cccc/hooks-alist
  '((c-mode-hook                      . cccc/load-options)
    (c++-mode-hook                    . cccc/load-options)
    (window-configuration-change-hook . cccc/reload-options-if-build-config-is-updated))
  "Associations list cccc need to hook.")

(defun cccc/mode-start ()
  "Start cccc.  Add local hooks."
  (mapc (lambda (item)
	  (add-hook (car item) (cdr item) nil t))
	cccc/hooks-alist))

(defun cccc/mode-stop ()
  "Stop cccc.  Remove local hooks."
  (mapc (lambda (item)
	  (remove-hook (car item) (cdr item) t))
	cccc/hooks-alist))

(defun cccc/debug-init ()
  "Just init cccc normally."
  (interactive)
  (add-hook 'c-mode-hook #'cccc-mode)
  (add-hook 'c++-mode-hook #'cccc-mode))

(defconst cccc/mode-line-lighter " cccc"
  "The default lighter for cccc-mode.")

(define-minor-mode cccc-mode
  "Minor mode for auto configuration of C/C++ syntax checking and completion plugins.

This mode get the compile options from build systems (like CMake) and
configure syntax checking and completion plugins. Not only that, you can
generate build tree and compile and run it. So, you can use emacs like IDE
only put source codes and build configuration file (like CMakeFile.txt).

This mode can works with below.

Build systems:

  - CMake (with (UNIX/MinGW) Makefile generator)

Syntax check plugins:

  - Flycheck

Completion (like Intellisense) plugins:

  - Company
  - emacs-clang-complete-async

Compile option analysis can be controlled with `cccc/load-options' and
`cccc/reload-options'. But you probably don't need call them directly. While cccc-mode
is enabled, those commands automatically called when you open C/C++ sources and
someone modified build configuration file.

You can generate build tree with `cccc/generate-build-files'.
You can compile build tree with `cccc/compile'.
You can run executable file with `cccc/run'.

If you want to compile and run, I recommend `cccc/compile-and-run'."
  :init-value nil
  :lighter cccc/mode-line-lighter
  :group 'cccc
  ;; :after-hook (cccc/load-options)
  (if cccc-mode
      (cccc/mode-start)
    (cccc/mode-stop)))
  
;;;; Compiling
(defun cccc/cmake-compile-impl (build-tree &optional finish-func)
  "Build the cmake project BUILD-TREE.

Finally, FINISH-FUNC will be called."
  (save-some-buffers)
  (let ((proc (start-process-shell-command "cccc-cmake-build"
					   cccc/build-buffer-name
					   (format "cmake --build %s"
							    build-tree))))
    (cccc/puthash-process->buffer proc (current-buffer))
    (pop-to-buffer cccc/build-buffer-name)
    (setq buffer-read-only t)
    (local-set-key "q" #'quit-window)
    (when finish-func
      (set-process-sentinel proc finish-func))))

(defun cccc/cmake-compile (&optional finish-func)
  "Build the build tree detected or user chosen with cmake.

Return the path to built object.
Finally, FINISH-FUNC will be called."
  (unless (cccc/is-set-paths-buffer-local)
    (error "Please call ``cccc/load-options' first"))
  (cccc/cmake-compile-impl cccc/path-build-tree finish-func))

(defun cccc/compile (&optional finish-func)
  "Build the build tree detected or user chosen with cmake.

Finally, FINISH-FUNC will be called."
  (interactive)
  (cccc/cmake-compile finish-func))

;; Running
(defun cccc/run-impl (executable)
  "Run EXECUTABLE."
  (executable-interpret executable))

(cl-defun cccc/run (&optional executable)
  "Run EXECUTABLE."
  (interactive)
  (unless executable
    (setq executable (read-file-name "Please select executable file:"
				     (file-name-as-directory cccc/path-build-tree)
				     nil
				     t)))
  (cccc/run-impl executable))

;; Utility
(defun cccc/compile-and-run (&optional executable)
  "Compile and Run EXECUTABLE."
  (interactive)
  (cccc/compile (lambda (process event)
		  (unless (cccc/is-string-event-finished event)
		    (error "Failed to compile"))
		  (switch-to-buffer cccc/build-buffer-name)
		  (quit-window)
		  (with-current-buffer (cccc/gethash-process->buffer process)
		    (call-interactively #'cccc/run)))))

;; Open build configuration file
(defun cccc/open-build-config-file ()
  "Open build config file now using."
  (interactive)
  (unless (cccc/is-set-paths-buffer-local)
    (error "Please call ``cccc/load-options' first"))
  (find-file cccc/path-build-config-file))

(provide 'cc-check-and-completion-utility)

;;; cc-check-and-completion-utility.el ends here

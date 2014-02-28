;;; yabai-build-cmake.el --- yabai-mode back-end for CMake

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

(defconst yabai/cmake-generation-buffer-name "*yabai-cmake*")

;;;; CMake Generator ======================================================================

(yabai/def-option-var yabai/cmake-generator 'make
		     "CMake's build system generator.

When you choose `make', it means \"UNIX Makefiles\" if you use UNIX.
If you use Windows it means \"MinGW Makefiles\". If you use MSYS it means
\"MSYS Makefiles\".

When you choose `ninja', it means \"Ninja\" in all systems."
		     :type 'symbol
		     :options '(make ninja))

(defvar-local yabai/path-cmake-build-tree nil)

;;;; Build tree directory name is used when find automatically ====================

(yabai/def-option-var yabai/cmake-build-tree-name "emacs-build"
		     "CMake's build tree name used when build-tree searching and creating.")

;;;; =============================================================================================
;;;; Implementations about CMake build tree
;;;; =============================================================================================
;;;; Build files Generation
(defun yabai/cmake-generator-symbol-to-string (symbol)
  "Convert SYMBOL means cmake's generator to string."
  (cl-case symbol
    ('make (if (eq system-type 'windows-nt)
		   (if (executable-find "sh")
		       "MSYS Makefiles"
		     "MinGW Makefiles")
		 "Unix Makefiles"))
    ('ninja "Ninja")
    (otherwise (error "Invalid cmake generator passed"))))

;;;; Checking build tree
(defun yabai/cmake-check-build-tree (build-tree src-tree)
  "Check BUILD-TREE made by SRC-TREE is valid."
  (let ((cache-path (expand-file-name "CMakeCache.txt" build-tree)))
    ;; Check this build tree is having CMakeCache.txt.
    (unless (file-exists-p cache-path)
      (error "In '%s', there aren't CMakeCache.txt" build-tree))
    ;; Check this build tree is taking source tree starts with DIR-BEGIN.
    (unless (yabai/file-name-equal
	     src-tree
	     (yabai/cmake-cache-get-value "CMAKE_HOME_DIRECTORY:INTERNAL"
					 cache-path))
      (error "'%s' is not build-tree made for source-tree '%s'"
	     build-tree
	     src-tree))
    ;; Check this build tree was made for build system Make or Ninja.
    (let ((tree-generator (yabai/cmake-cache-get-value "CMAKE_GENERATOR:INTERNAL"
						       cache-path))
	  (current-generator (yabai/cmake-generator-symbol-to-string yabai/cmake-generator)))
      (unless (string= tree-generator
		       current-generator)
	(error "'%s' is not build-tree made for %s.  Hint: Remove build-tree or Change generator"
	       build-tree
	       current-generator))))
  build-tree)

;;;; Finding build tree
(defun yabai/cmake-find-build-tree (dir-begin)
  "Find build tree made by cmake from DIR-BEGIN to root."
  (let ((build-tree (yabai/directory-exists-at-upper-or-current-p (list yabai/cmake-build-tree-name) dir-begin)))
    (unless build-tree
      (error "From '%s', can't find build-tree" dir-begin))
    build-tree))

;;;; CMakeCache.txt Parsing
(defun yabai/cmake-cache-get-value (variable-name cache-path)
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

(defun yabai/cmake-generate-build-files-impl (src-tree build-tree &optional finish-func)
  "Execute cmake takes sources at SRC-TREE at BUILD-TREE.

When works over, FINISH-FUNC will be called."
  (let ((generator (yabai/cmake-generator-symbol-to-string yabai/cmake-generator)))
    (let ((prc (start-process-shell-command "emacs-yabai-cmake"
					    yabai/cmake-generation-buffer-name
					    ;; -DCMAKE_EXPORT_COMPILE_COMMANDS=ON means to ask cmake to
					    ;; output clang's JSON Compilation Database Format Specification.
					    (format "cd %s && cmake %s -G \"%s\" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
						    (yabai/expand-file-name build-tree)
						    (yabai/expand-file-name src-tree)
						    generator))))
      ;;(pop-to-buffer yabai/cmake-generation-buffer-name)
      (when finish-func
	(yabai/puthash-process->buffer prc (current-buffer))
	(set-process-sentinel prc finish-func)))))

(defun yabai/cmake-choose-build-tree (src-tree)
  "Return build-tree path user choised.

Build selection start at SRC-TREE."
  (let ((dir (read-directory-name (format "Please select build-tree named `%s':" yabai/cmake-build-tree-name)
				  (expand-file-name yabai/cmake-build-tree-name
						    (file-name-as-directory src-tree)))))
    (unless (string= yabai/cmake-build-tree-name
		     (file-name-nondirectory (directory-file-name dir)))
      (error "Build tree is must named `%s'" yabai/cmake-build-tree-name))
    (if (file-exists-p dir)
	(when (not (file-directory-p dir))
	  (error "Can't create build tree in `%s'" dir))
      ;; mkdir with confirmation
      (call-interactively (lambda (&optional reply)
			    (interactive "cDirectory is not exists. Do you want to create?(y/n):")
			    (if (equal reply ?y)
				(make-directory dir)
			      (error "Build tree is not created")))))
    dir))

(defun yabai/cmake-find-or-choose-build-tree (src-tree)
  "Return build-tree path detected (from SRC-TREE) or user chosen."
  (or (ignore-errors (yabai/cmake-check-build-tree
		      (yabai/cmake-find-build-tree src-tree)
		      src-tree))
      (yabai/cmake-choose-build-tree src-tree)))

(defun yabai/cmake-generate-build-files (src-tree build-tree finish-func)
  "Execute cmake at guessed build tree for guessed source tree.

If SRC-TREE or BUILD-TREE defined, will use them.
If not, cause error.
When works over, FINISH-FUNC will be called."
  (yabai/cmake-generate-build-files-impl src-tree build-tree finish-func))

;;;; =============================================================================================
;;;; Pre process
;;;; =============================================================================================
(defun yabai/cmake-pre-process (build-config source-tree finish-func)
  "Generate cmake build tree to generate compilation database.

BUILD-CONFIG, SOURCE-TREE and FINISH-FUNC are arguments.
If cmake build tree path is not defined, detect or choose."
  (unless yabai/path-cmake-build-tree
    (setq-local yabai/path-cmake-build-tree (yabai/cmake-find-or-choose-build-tree
					    source-tree)))
  (yabai/cmake-generate-build-files source-tree yabai/path-cmake-build-tree finish-func))

;;;; =============================================================================================
;;;; JSON compilation database path provider
;;;; =============================================================================================
(defun yabai/cmake-get-db-path (build-config build-tree)
  "From BUILD-CONFIG and BUILD-TREE, return path to JSON compilation db."
  (expand-file-name "compile_commands.json"
		    build-tree))

;;;; =============================================================================================
;;;; Compile
;;;; =============================================================================================
(defun yabai/cmake-compile (build-config finish-func)
  "Build the build tree detected or user chosen with cmake.

Return the path to built object.
BUILD-CONFIG argument is reserved.
Finally, FINISH-FUNC will be called."
  (yabai/compile-impl (format "cmake --build %s"
			      yabai/path-cmake-build-tree)
		      finish-func))

;;;; =============================================================================================
;;;; Integrate
;;;; =============================================================================================
(yabai/define-build-system 'cmake
			   "CMakeLists.txt"
			   #'yabai/cmake-pre-process
			   #'yabai/cmake-get-db-path
			   #'yabai/cmake-compile
			   (lambda ()
			     (eval 'yabai/path-cmake-build-tree)))
			   

(provide 'yabai-build-cmake)

;;; yabai-build-cmake.el ends here

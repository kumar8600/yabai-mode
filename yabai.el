;;; yabai.el --- Minor mode for C/C++ syntax checking and completion with any build systems without any configuration in many cases.

;; Author: kumar8600 <kumar8600@gmail.com>
;; URL: https://github.com/kumar8600/yabai
;; Version: 0.30
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

;; Minor mode for auto configuration of C/C++ syntax checking and completion packages.

;;
;; Basic setup:
;;
;;   (add-to-list 'load-path
;;                "~/path-to-yabai")
;;   (require 'yabai)
;;
;;   (add-hook 'c-mode-hook 'yabai-mode)
;;   (add-hook 'c++-mode-hook 'yabai-mode)
;;
;; Notice:
;;
;;   Now, it doesn't have any functions to set compiler options manually.

;;; Code:

(require 'cl-lib)
(require 'json)

(require 'yabai-definitions)
(require 'yabai-utilities)
(require 'yabai-build-cmake)
(require 'yabai-build-bear)
(require 'yabai-build-json-compilation)
(require 'yabai-integrators)
(require 'yabai-client)

;;;; Setting options =======================================================================
(defun yabai/set-compiler-options (options)
  "Set compiler OPTIONS to syntax checker and completers."
  (mapc (lambda (item)
	  (when (eval (intern (concat "yabai/with-" (symbol-name (car item)))))
	    (funcall (cdr item)
		     options)))
	yabai/integration-definitions))

;;;; Paths detection and confirmation ======================================================
(defun yabai/find-paths-and-set-local ()
  "Set buffer local variables about paths.

`yabai/path-build-config-file'
`yabai/path-source-tree'
`yabai/path-build-tree'
`yabai/build-system-in-local-buffer'
will be written."
  (let ((dir-begin (file-name-directory (buffer-file-name))))
    ;;
    ;; set special buffer local variables
    ;;
    ;; search build config file
    (setq-local yabai/path-build-config-file (or (yabai/find-build-config-file
						  (yabai/get-possible-config-file-name-list)
						  dir-begin)
						 (error "Build config file not found")))
    (setq-local yabai/build-system-in-local-buffer (yabai/get-build-system-symbol yabai/path-build-config-file))
    ;; search or create build-tree
    (setq-local yabai/path-source-tree (file-name-directory
					yabai/path-build-config-file))))

(defun yabai/are-set-paths-buffer-local ()
  "If buffer local variables about paths are set, return t.

`yabai/path-build-config-file'
`yabai/path-source-tree'
`yabai/build-system-in-local-buffer'
`yabai/time-last-mod-build-config-file'
and build system now selecting's build-tree
will be tested."
  (if (and yabai/path-build-config-file
	   yabai/path-source-tree
	   yabai/build-system-in-local-buffer
	   yabai/time-last-mod-build-config-file)
      t
    nil))

;;;; Control compilation database server ==================================================================
(defun yabai/get-compiler-option-current-buffer-from-server ()
  "Get compiler option for current buffer from compilaition database server."
  (yabai/client-request-get-options (buffer-file-name)
				    (yabai/build-system-get-database-path-current-buffer)
				    (lambda (result)
				      (when result
					(yabai/set-compiler-options result)))))

(defun yabai/add-database-current-buffer-to-server (&optional sentinel)
  "Add compilation database used by current buffer to server.

When works over, SENTINEL is called."
  (yabai/client-request-add-database (yabai/build-system-get-database-path-current-buffer)
				     sentinel))

(defun yabai/add-database-and-get-compiler-options-current-buffer ()
  "Add compilation database and get compiler options.

All values get from current buffer."
  (yabai/add-database-current-buffer-to-server
   (lambda (result)
     (when (string= result
		    "add-database-ok")
       (yabai/get-compiler-option-current-buffer-from-server)))))

(defun yabai/generate-database-add-to-server-and-get-compiler-options ()
  "Generate compiler option database, send to database server."
  (let ((pre-process (yabai/build-system-get-pre-process-func yabai/build-system-in-local-buffer)))
    ;; generate build-tree
    (funcall pre-process
	     yabai/path-build-config-file
	     yabai/path-source-tree
	     (lambda (process event)
	       (if process
		   (with-current-buffer (yabai/gethash-process->buffer process)
		     (unless (yabai/is-string-event-finished event)
		       (error "YABAI compilation database generation failed"))
		     (yabai/add-database-and-get-compiler-options-current-buffer))
		 (yabai/add-database-and-get-compiler-options-current-buffer))))))

;;;; User interfaces ============================================================

;;;; Setting compiler options
(defun yabai/load-options ()
  "Set options are guessed from your build configuration file."
  (interactive)
  (when (ignore-errors ;; if paths not found, do nothing.
	  ;;
	  ;; set special buffer local variables
	  ;;    
	  (yabai/find-paths-and-set-local))
    ;; LET'S BEGIN
    (yabai/generate-database-add-to-server-and-get-compiler-options)))

(defun yabai/reload-options ()
  "Reload compiler options from paths had already defined."
  (interactive)
  (unless (yabai/are-set-paths-buffer-local)
    (error "Please call `yabai/load-options' first"))
  (yabai/generate-database-add-to-server-and-get-compiler-options))

(defun yabai/reload-options-if-build-config-is-updated ()
  "If build config file is not one last loaded, reload."
  (if yabai/time-last-mod-build-config-file
      (unless (equal yabai/time-last-mod-build-config-file
		     (yabai/get-time-last-modified yabai/path-build-config-file))
	(yabai/reload-options))
    ;; if `yabai/time-last-mod-build-config-file' is nil, do nothing.
    nil))

;;;; define as minor mode
(defconst yabai/hooks-alist
  '((c-mode-hook                      . yabai/load-options)
    (c++-mode-hook                    . yabai/load-options)
    (window-configuration-change-hook . yabai/reload-options-if-build-config-is-updated))
  "Associations list yabai need to hook.")

(defun yabai/mode-start ()
  "Start yabai.  Add local hooks and launch server."
  (mapc (lambda (item)
	  (add-hook (car item) (cdr item) nil t))
	yabai/hooks-alist)
  (unless (yabai/client-is-server-working)
    (yabai/start-server)))

(defun yabai/mode-stop ()
  "Stop yabai.  Remove local hooks and finish server."
  (mapc (lambda (item)
	  (remove-hook (car item) (cdr item) t))
	yabai/hooks-alist)
  (when (yabai/client-is-server-working)
    (yabai/stop-server)))

(defun yabai/debug-init ()
  "Just init yabai normally."
  (interactive)
  (add-hook 'c-mode-hook #'yabai-mode)
  (add-hook 'c++-mode-hook #'yabai-mode))

(define-minor-mode yabai-mode
  "Minor mode for auto configuration of C/C++ syntax checking and completion packages.

This mode get the compiler options from build systems (like CMake) and
configure syntax checking and completion packages. Not only that, you can
generate build tree and compile and run it. So, you can use emacs like IDE
only put source codes and build configuration file (like CMakeFile.txt).

This mode can works with below.

Build systems:

  - CMake with generators
    - Make
    - Ninja
  - Make without CMake (Bear is required)
  - Ninja without CMake (Bear is required)
  - JSON compilation database

Syntax check packages:

  - Flycheck

Completion (like Intellisense) packages:

  - Company
  - emacs-clang-complete-async

compiler option analysis can be controlled with `yabai/load-options' and
`yabai/reload-options'. But you probably don't need call them directly. While yabai-mode
is enabled, those commands automatically called when you open C/C++ sources and
someone modified build configuration file.

You can compile build tree with `yabai/compile'.
You can run executable file with `yabai/run'.

If you want to compile and run, I recommend `yabai/compile-and-run'."
  :init-value nil
  :lighter yabai/mode-line-lighter
  :group 'yabai
  ;; :after-hook (yabai/load-options)
  (if yabai-mode
      (yabai/mode-start)
    (yabai/mode-stop)))
  
;;;; Compiling ===========================================================================================
(defun yabai/compile (&optional finish-func)
  "Build the build tree detected or user chosen with cmake.

Finally, FINISH-FUNC will be called."
  (interactive)
  (unless (yabai/are-set-paths-buffer-local)
    (error "Please call ``yabai/load-options' first"))
  (save-some-buffers)
  (funcall (yabai/build-system-get-compilation-func yabai/build-system-in-local-buffer)
	   yabai/path-build-config-file
	   finish-func))

;; Running
(defun yabai/run-impl (executable)
  "Run EXECUTABLE."
  (executable-interpret executable))

(defun yabai/run (&optional executable)
  "Run EXECUTABLE."
  (interactive)
  (unless executable
    (setq executable (read-file-name "Please select executable file:"
				     (file-name-as-directory (funcall (yabai/build-system-get-build-tree-getter-func
								       yabai/build-system-in-local-buffer)))
				     nil
				     t)))
  (yabai/run-impl (format "cd %s && %s"
			  yabai/path-source-tree
			  executable)))

;; Utility
(defun yabai/compile-and-run (&optional executable)
  "Compile and Run EXECUTABLE."
  (interactive)
  (yabai/compile (lambda (process event)
		  (unless (yabai/is-string-event-finished event)
		    (error "Failed to compile"))
		  (switch-to-buffer yabai/build-buffer-name)
		  (quit-window)
		  (with-current-buffer (yabai/gethash-process->buffer process)
		    (call-interactively #'yabai/run)))))

;; Open build configuration file
(defun yabai/open-build-config-file ()
  "Open build config file now using."
  (interactive)
  (unless (yabai/are-set-paths-buffer-local)
    (error "Please call ``yabai/load-options' first"))
  (find-file yabai/path-build-config-file))

(provide 'yabai)

;;; yabai.el ends here

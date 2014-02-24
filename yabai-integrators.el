;;; yabai-integrators.el --- yabai-mode integration back-ends

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

;;;; =======================================================================================
;;;; Packages integration defining
;;;; =======================================================================================
;; Note:
;;   I want to use `custom-set-variables' but it can't set buffer local value.
;;   Do you have any idea better? Please tell me.

(yabai/define-integrator 'checker-flycheck
				   'individuals
				   (when (require 'flycheck nil t)
				     (setq-local flycheck-clang-definitions       list-definitions)
				     (setq-local flycheck-clang-include-path      list-include-paths)
				     (setq-local flycheck-clang-includes          list-includes)
				     (setq-local flycheck-clang-language-standard language-standard)
				     (setq-local flycheck-clang-ms-extensions     flag-ms-extensions)
				     (setq-local flycheck-clang-no-rtti           flag-no-rtti)
				     (setq-local flycheck-clang-standard-library  standard-library)
				     (setq-local flycheck-clang-warnings          list-warnings)
				     (flycheck-buffer)))

(yabai/define-integrator 'completer-company
				   'list
				   (when (require 'company nil t)
				     (setq-local company-clang-arguments options-list)))

(yabai/define-integrator 'completer-ac-clang-async
				   'list
				   (when (require 'auto-complete-clang-async nil t)
				     (setq-local ac-clang-cflags options-list)))

(provide 'yabai-integrators)

;;; yabai-integrators.el ends here

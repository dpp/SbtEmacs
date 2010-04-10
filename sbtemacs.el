;;; -*-Emacs-Lisp-*-
;;; scala-mode-inf.el - Interaction with a Scala interpreter.

;;; License

;; SCALA LICENSE
;;  
;; Copyright (c) 2002-2010 EPFL, Lausanne, unless otherwise specified.
;; Portions (c) 2010 David Pollak
;; All rights reserved.
;;  
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;  
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;  
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;  
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;  
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;  
;;  
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'sbtemacs)


(require 'comint)

(defgroup sbt-mode-inf
  nil
  "Mode to interact with sbt interpreter."
  :group 'sbt
  :tag "Inferior sbt")

(defcustom sbt-interpreter "sbt"
  "The interpreter that `run-sbt' should run. This should
 be a program in your PATH or the full pathname of sbt."
  :type 'string
  :group 'sbt-mode-inf)

(defconst sbt-inf-buffer-name "*inferior-sbt*")

(define-derived-mode sbt-mode-inf comint-mode "Inferior sbt"
  "Major mode for interacting with sbt.

\\{inferior-sbt-mode-map\\}"
  (define-key sbt-mode-inf-map [(meta return)] 'comint-accumulate)

  ;; Comint configuration
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'sbt-input-sender))

(defun sbt-input-sender (proc string)
  (comint-send-string proc string)
  (comint-send-string proc "\n"))

;;;###autoload
(defun sbt-interpreter-running-p-1 ()
  ;; True iff a sbt is currently running in a buffer.
  (comint-check-proc sbt-inf-buffer-name))

(defun sbt-check-interpreter-running ()
  (unless (sbt-interpreter-running-p-1)
    (error "sbt not running")))

;; Locate the project root directory from the source buffer location.

(defun sbt-project-dir-p (path)
  "Does a project/build.properties exists in the given path."
  (file-exists-p (concat path "/project/build.properties")))

(defun sbt-at-root (path)
  "Determine if the given path is root."
  (equal path (sbt-parent-path path)))

(defun sbt-parent-path (path)
  "The parent path for the given path."
  (file-truename (concat path "/..")))

;; Search up the directory tree until directory with a "project" subdir 
;; is found with build.properties
(defun sbt-find-path-to-project ()
  "Move up the directory tree for the current buffer until root or a directory with a project/build.properities is found."
  (interactive)
  (let ((fn (buffer-file-name)))
    (let ((path (file-name-directory fn)))
      (while (and (not (sbt-project-dir-p path))
		  (not (sbt-at-root path)))
	(setf path (file-truename (sbt-parent-path path))))
      path)))

;;(defun refoo (dog)
;;  "example"
;;  (message "dog %s" dog)
;;  (if (string-match-p "[a-z]\\([1-9]*\\)" dog)
;;  (message "foo %d %d" (match-beginning 0) (match-end 0))))

;; (refoo "a992")

;;;###autoload
(defun sbt-run-sbt ()
  "Run sbt in an Emacs buffer"
  (interactive)
  (unless (sbt-interpreter-running-p-1)
    (setq sbt-interpreter (concat "cd " (sbt-find-path-to-project) "; sbt"))
    (let ((cmd/args (split-string "sbt-no-color"))) ;;; (concat "cd " (sbt-find-path-to-project) "; sbt"))))
      (set-buffer
       (apply 'make-comint "inferior-sbt" (car cmd/args) nil (cdr cmd/args))))
    (sbt-mode-inf)
    (add-hook 'comint-preoutput-filter-functions 'my-output-filter)
    (pop-to-buffer sbt-inf-buffer-name)))

(defun my-output-filter (yout)
  (interactive)
;;  (if (string-match "^\\[error\\] \\([.a-zA-Z0-9/-]+[.]scala\\):\\([0-9]+\\):" yout)
  (if (posix-string-match "^\\[error\\] \\([.a-zA-Z0-9/-]+[.]scala\\):\\([0-9]+\\):" yout)
      (message (concat "yak -> " yout)))
  yout
  )


(defun sbt-send-string (str &rest args)
  ;; Send string to interpreter
  (comint-send-string sbt-inf-buffer-name (apply 'format str args))
  (comint-send-string sbt-inf-buffer-name "\n"))

;;;###autoload
(defun sbt-switch-to-interpreter ()
  "Switch to buffer containing the interpreter"
  (interactive)
  (sbt-check-interpreter-running)
  (switch-to-buffer sbt-inf-buffer-name))

(defvar sbt-tmp-file nil)

;;;###autoload
(defun sbt-eval-region (start end)
  "Send current region to sbt."
  (interactive "r")
  (sbt-check-interpreter-running)
  (comint-send-region sbt-inf-buffer-name start end)
  (comint-send-string sbt-inf-buffer-name "\n"))


;;;###autoload
(defun sbt-quit-interpreter ()
  "Quit sbt."
  (interactive)
  (sbt-check-interpreter-running)
  (sbt-send-string "\nquit\n"))

;;;###autoload
(defun sbt-do-compile ()
  "Compile the code."
  (interactive)
  (message "starting compile")
  (if (sbt-interpreter-running-p-1) 
      (sbt-send-string "compile\n")))


(add-to-list 'after-save-hook 'sbt-do-compile)



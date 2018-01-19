;;; cask-bootstrap.el --- Cask: Bootstrap internal dependencies  -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; URL: http://github.com/cpitclaudel/cask

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bootstrap Cask's internal dependencies.

;;; Code:

(eval-when-compile
  (defvar cask-directory))

(defconst cask-bootstrap-emacs-version
  (format "%s.%s"
          emacs-major-version
          emacs-minor-version))

(defconst cask-bootstrap-dir
  (expand-file-name
   (locate-user-emacs-file (format ".cask/%s/bootstrap" cask-bootstrap-emacs-version)))
  "Path to Cask bootstrap directory.")

(defconst cask-bootstrap-packages
  '(s dash f commander git epl shut-up cl-lib package-build)
  "List of bootstrap packages required by this file.")

(unless (require 'package nil :noerror)
  (require 'package (expand-file-name "package-legacy" cask-directory)))


(require 'package)
(require 'tls)

(defun my/package--download-and-read-archives (&optional async)
  "Download descriptions of all `package-archives' and read them.
This populates `package-archive-contents'.  If ASYNC is non-nil,
perform the downloads asynchronously."
  ;; The downloaded archive contents will be read as part of
  ;; `package--update-downloads-in-progress'.
  (dolist (archive package-archives)
    (cl-pushnew archive package--downloads-in-progress
                :test #'equal))
  (dolist (archive package-archives)
    (condition-case-unless-debug err
        (package--download-one-archive archive "archive-contents" async)
      (error (message "Failed to download `%s' archive: %S"
                      (car archive) err)))))

(advice-add 'package--download-and-read-archives
            :override
            #'my/package--download-and-read-archives)

(defun my/open-tls-stream (name buffer host port)
  "Open a TLS connection for a port to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST PORT.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to, or its IP address.
Fourth arg PORT is an integer specifying a port to connect to."
  (let ((cmds tls-program)
	(use-temp-buffer (null buffer))
	process	cmd done)
    (if use-temp-buffer
	(setq buffer (generate-new-buffer " TLS"))
      ;; BUFFER is a string but does not exist as a buffer object.
      (unless (and (get-buffer buffer)
		   (buffer-name (get-buffer buffer)))
	(generate-new-buffer buffer)))
    (with-current-buffer buffer
      (message "Opening TLS connection to `%s'..." host)
      (while (and (not done) (setq cmd (pop cmds)))
	(let ((process-connection-type tls-process-connection-type)
	      (formatted-cmd
	       (format-spec
		cmd
		(format-spec-make
                 ?t (car (gnutls-trustfiles))
		 ?h host
		 ?p (if (integerp port)
			(int-to-string port)
		      port)))))
	  (message "Opening TLS connection with `%s'..." formatted-cmd)
	  (setq process (start-process
			 name buffer shell-file-name shell-command-switch
			 formatted-cmd))
	  (while (and process
		      (memq (process-status process) '(open run))
		      (progn
			(goto-char (point-min))
			(not (setq done (re-search-forward
					 tls-success nil t)))))
	    (unless (accept-process-output process 1)
	      (sit-for 1)))
	  (message "Opening TLS connection with `%s'...%s" formatted-cmd
		   (if done "done" (format "failed: %S" (buffer-string))))
	  (if (not done)
	      (delete-process process)
	    ;; advance point to after all informational messages that
	    ;; `openssl s_client' and `gnutls' print
	    (let ((start-of-data nil))
	      (while
		  (not (setq start-of-data
			     ;; the string matching `tls-end-of-info'
			     ;; might come in separate chunks from
			     ;; `accept-process-output', so start the
			     ;; search where `tls-success' ended
			     (save-excursion
			       (if (re-search-forward tls-end-of-info nil t)
				   (match-end 0)))))
		(accept-process-output process 1))
	      (if start-of-data
		  ;; move point to start of client data
		  (goto-char start-of-data)))
	    (setq done process))))
      (when (and done
		 (or
		  (and tls-checktrust
		       (save-excursion
			 (goto-char (point-min))
			 (re-search-forward tls-untrusted nil t))
		       (or
			(and (not (eq tls-checktrust 'ask))
			     (message "The certificate presented by `%s' is \
NOT trusted." host))
			(not (yes-or-no-p
			      (tls-format-message "\
The certificate presented by `%s' is NOT trusted. Accept anyway? " host)))))
		  (and tls-hostmismatch
		       (save-excursion
			 (goto-char (point-min))
			 (re-search-forward tls-hostmismatch nil t))
		       (not (yes-or-no-p
			     (format "Host name in certificate doesn't \
match `%s'. Connect anyway? " host))))))
	(setq done nil)
	(delete-process process))
      ;; Delete all the informational messages that could confuse
      ;; future uses of `buffer'.
      (delete-region (point-min) (point)))
    (message "Opening TLS connection to `%s'...%s"
	     host (if done "done" "failed"))
    (when use-temp-buffer
      (if done (set-process-buffer process nil))
      (kill-buffer buffer))
    done))

(advice-add 'open-tls-stream
            :override
            #'my/open-tls-stream)

(message "Using overridden package code.")
(prin1 "Using overridden package code.")

(let ((orig-load-path load-path))
  (unwind-protect
      (let (package-archives
            package-alist
            package-archive-contents
            (package-user-dir cask-bootstrap-dir))
        (package-initialize)
        (condition-case nil
            (mapc 'require cask-bootstrap-packages)
          (error
           (add-to-list 'package-archives (cons "gnu" "https://elpa.gnu.org/packages/"))
           (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
           (package-refresh-contents)
           (mapc
            (lambda (package)
              (unless (package-installed-p package)
                (package-install package)))
            cask-bootstrap-packages)
           (mapc 'require cask-bootstrap-packages))))
    (setq load-path orig-load-path)))

(provide 'cask-bootstrap)

;;; cask-bootstrap.el ends here

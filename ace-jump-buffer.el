;;; ace-jump-buffer.el --- fast buffer switching extension to `ace-jump-mode'
;;
;; Copyright 2013 Justin Talbott
;;
;; Author: Justin Talbott <justin@waymondo.com>
;; URL: https://github.com/waymondo/ace-jump-buffer
;; Version: 0.2.2
;; Package-Requires: ((ace-jump-mode "1.0") (dash "2.4.0"))
;;
;;
;; Installation:
;;
;;   (require 'ace-jump-buffer)
;;   then bind `ace-jump-buffer' to something useful
;;

(require 'bs)
(require 'ace-jump-mode)
(require 'recentf)
(require 'dash)

(defmacro make-ace-jump-buffer-function (name &rest buffer-list-reject-filter)
  "Create a `bs-configuration' and interactive defun using NAME that displays buffers
that don't get rejected by the body of BUFFER-LIST-REJECT-FILTER."
  (declare (indent 1))
  (let ((filter-defun-name (intern (format "ajb/filter-%s-buffers" name)))
        (defun-name (intern (format "ace-jump-%s-buffers" name))))
    `(progn
       (defun ,filter-defun-name (buffer)
         ,@buffer-list-reject-filter)
       (defun ,defun-name ()
         (interactive)
         (let ((ajb-bs-configuration ,name))
           (call-interactively 'ace-jump-buffer)))
       (add-to-list 'bs-configurations
                 '(,name nil nil nil ,filter-defun-name nil)))))

(when (require 'perspective nil 'noerror)
  (make-ace-jump-buffer-function "persp"
    (with-current-buffer buffer
      (not (member buffer (persp-buffers persp-curr))))))

(when (require 'projectile nil 'noerror)
  (make-ace-jump-buffer-function "projectile"
    (let ((project-root (projectile-project-root)))
      (with-current-buffer buffer
        (not (projectile-project-buffer-p buffer project-root))))))

(defgroup ace-jump-buffer nil
  "fast buffer switching extension to `ace-jump-mode'"
  :version "0.2.0"
  :link '(url-link "https://github.com/waymondo/ace-jump-buffer")
  :group 'convenience)

(defcustom ajb-max-window-height 27
  "Maximal window height of Ace Jump Buffer Selection Menu."
  :group 'ace-jump-buffer
  :type 'integer)

(defcustom ajb-sort-function 'bs-sort-buffers-by-recentf
  "The `bs-sort-function' function used when displaying `ace-jump-buffer'"
  :group 'ace-jump-buffer)

(defcustom ajb-bs-configuration "all"
  "The `bs-configuration' used when displaying `ace-jump-buffer'"
  :group 'ace-jump-buffer)

;; interval settings
(defvar ajb/showing nil)
(defvar ajb/other-window nil)
(defvar ajb/in-one-window nil)

;; settings for a barebones `bs' switcher
(defvar ajb/bs-attributes-list '(("" 2 2 left " ")
                                 ("" 1 1 left bs--get-marked-string)
                                 ("" 1 1 left " ")
                                 ("Buffer" bs--get-name-length 10 left bs--get-name)))


(defadvice bs--show-header (around maybe-disable-bs-header activate)
  "Don't show the `bs' header when doing `ace-jump-buffer'"
  (unless ajb/showing ad-do-it))

(defun ace-jump-buffer-hook ()
  "On the end of ace jump, select the buffer at the current line."
  (when (string-match (buffer-name) "*buffer-selection*")
    (if ajb/other-window (bs-select-other-window)
      (if ajb/in-one-window (bs-select-in-one-window)
        (bs-select)))
    (ace-jump-buffer-reset)))

(add-hook 'ace-jump-mode-end-hook 'ace-jump-buffer-hook)

(defun ace-jump-buffer-reset ()
  (setq ajb/other-window nil)
  (setq ajb/in-one-window nil)
  (kill-buffer "*buffer-selection*"))

;;;###autoload
(defun ace-jump-buffer ()
  "Quickly hop to buffer with `ace-jump-mode'."
  (interactive)
  (let ((ace-jump-mode-gray-background nil)
        (ace-jump-mode-scope 'window)
        (bs-buffer-sort-function ajb-sort-function)
        (bs-attributes-list ajb/bs-attributes-list)
        (ajb/showing t))
    (bs--show-with-configuration ajb-bs-configuration)
    (set (make-local-variable 'bs-header-lines-length) 0)
    (set (make-local-variable 'bs-max-window-height) ajb-max-window-height)
    (push-mark)
    (goto-char (point-min))
    (bs--set-window-height)
    (call-interactively 'ace-jump-line-mode)
    (define-key overriding-local-map [t] 'ace-jump-buffer-exit)))

;;;###autoload
(defun ace-jump-buffer-other-window ()
  "Quickly hop to buffer with `ace-jump-mode' in other window."
  (interactive)
  (setq ajb/other-window t)
  (ace-jump-buffer))

;;;###autoload
(defun ace-jump-buffer-in-one-window ()
  "Quickly hop to buffer with `ace-jump-mode' in one window."
  (interactive)
  (setq ajb/in-one-window t)
  (ace-jump-buffer))

(defun bs-sort-buffers-by-recentf (b1 b2)
  "Function for sorting buffers by recentf order."
  (let ((b1-index (-elem-index (buffer-file-name b1) recentf-list))
        (b2-index (-elem-index (buffer-file-name b2) recentf-list)))
    (when (< b1-index b2-index) t)))

(defun ace-jump-buffer-exit ()
  (interactive)
  (if (string-match (buffer-name) "*buffer-selection*")
      (progn
        (when ace-jump-current-mode (ace-jump-done))
        (bs-kill)
        (ace-jump-buffer-reset))
    (ace-jump-done)))

(provide 'ace-jump-buffer)
;;; ace-jump-buffer.el ends here

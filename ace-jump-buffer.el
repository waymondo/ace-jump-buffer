;;; ace-jump-buffer.el --- fast buffer switching extension to `ace-jump-mode'
;;
;; Copyright 2013 Justin Talbott
;;
;; Author: Justin Talbott <justin@waymondo.com>
;; URL: https://github.com/waymondo/ace-jump-buffer
;; Version: 0.1.2
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

;; cache any initial `bs' settings
(defvar ajb-initial-bs-header-lines-length bs-header-lines-length)
(defvar ajb-initial-bs-max-window-height bs-max-window-height)
(defvar ajb-initial-bs-attributes-list bs-attributes-list)
(defvar ajb-initial-bs-configuration bs-current-configuration)
(defvar ajb-initial-bs-buffer-sort-function bs-buffer-sort-function)

;; cache current ace jump mode scope
(defvar ajb-initial-ace-jump-mode-scope ace-jump-mode-scope)
(defvar ajb-initial-ace-jump-mode-gray-background ace-jump-mode-gray-background)

;; when `perspective' mode is found and loaded, add a `bs-configuration' for it
(when (require 'perspective nil 'noerror)

  (defun ajb-buffer-in-persp-curr (buffer)
    (with-current-buffer buffer
      (not (member buffer (persp-buffers persp-curr)))))

  (add-to-list 'bs-configurations
               '("persp" nil nil nil ajb-buffer-in-persp-curr nil)))

(defun bs-sort-buffers-by-recentf (b1 b2)
  "Function for sorting buffers by recentf order."
  (let ((b1-index (-elem-index (buffer-file-name b1) recentf-list))
        (b2-index (-elem-index (buffer-file-name b2) recentf-list)))
    (when (< b1-index b2-index) t)))

;; settings for a barebones `bs' switcher
(defvar ajb-bs-configuration "all")
(defvar ajb-bs-header-lines-length 0)
(defvar ajb-bs-max-window-height 27)
(defvar ajb-bs-attributes-list (quote (("" 2 2 left " ")
                                       ("" 1 1 left bs--get-marked-string)
                                       ("" 1 1 left " ")
                                       ("Buffer" bs--get-name-length 10 left bs--get-name))))

(defadvice bs--show-header (around maybe-disable-bs-header)
  "Don't show the `bs' header when doing `ace-jump-buffer'"
  (if nil ad-do-it))

(defun ace-jump-buffer-turn-on ()
  (add-hook 'ace-jump-mode-end-hook 'ace-jump-buffer-hook)
  (ad-activate 'bs--show-header)
  (setq ace-jump-mode-scope 'window)
  (setq ace-jump-mode-gray-background nil)
  (setq bs-buffer-sort-function 'bs-sort-buffers-by-recentf)
  (setq bs-header-lines-length ajb-bs-header-lines-length)
  (setq bs-max-window-height ajb-bs-max-window-height)
  (setq bs-attributes-list ajb-bs-attributes-list))

(defun ace-jump-buffer-turn-off ()
  (remove-hook 'ace-jump-mode-end-hook 'ace-jump-buffer-hook)
  (ad-deactivate 'bs--show-header)
  (setq ace-jump-mode-scope ajb-initial-ace-jump-mode-scope)
  (setq ace-jump-mode-gray-background ajb-initial-ace-jump-mode-gray-background)
  (setq bs-buffer-sort-function ajb-initial-bs-buffer-sort-function)
  (setq bs-header-lines-length ajb-initial-bs-header-lines-length)
  (setq bs-max-window-height ajb-initial-bs-max-window-height)
  (setq bs-attributes-list ajb-initial-bs-attributes-list))

(defun ace-jump-buffer-hook ()
  "On the end of ace jump, select the buffer at the current line."
  (when (string-match (buffer-name) "*buffer-selection*")
    (bs-select)
    (ace-jump-buffer-turn-off)))

;;;###autoload
(defun ace-jump-buffer ()
  "Quickly hop between buffers with `ace-jump-mode'"
  (interactive)
  (ace-jump-buffer-turn-on)
  (bs--show-with-configuration ajb-bs-configuration)
  (push-mark)
  (goto-char (point-min))
  (set (make-local-variable 'ace-jump-mode-scope) 'window)
  (call-interactively 'ace-jump-line-mode)
  (define-key overriding-local-map (kbd "C-g") 'ace-jump-buffer-exit)
  (define-key overriding-local-map [t] 'ace-jump-buffer-exit))

(defun ace-jump-buffer-exit ()
  (interactive)
  (if (string-match (buffer-name) "*buffer-selection*")
      (progn
        (when ace-jump-current-mode (ace-jump-done))
        (ace-jump-buffer-turn-off)
        (bs-kill)
        (kill-buffer "*buffer-selection*"))
    (let* ((ace-jump-mode nil)
           (original-func (key-binding (kbd "C-g"))))
      (call-interactively original-func))))

(provide 'ace-jump-buffer)
;;; ace-jump-buffer.el ends here

;;; ace-jump-buffer.el --- fast buffer switching extension to `ace-jump-mode'
;;
;; Copyright 2013 Justin Talbott
;;
;; Author: Justin Talbott <justin@waymondo.com>
;; URL: https://github.com/waymondo/ace-jump-buffer
;; Version: 0.1
;; Package-Requires: ((ace-jump-mode "1.0"))
;;
;;
;; Installation:
;;
;;   (require 'ace-jump-buffer)
;;   (ace-jump-buffer-mode t)
;;
;;   then bind `ace-jump-buffer' to something useful
;;

(require 'bs)
(require 'ace-jump-mode)

;; cache any initial `bs' settings
(defvar ajb-initial-bs-header-lines-length bs-header-lines-length)
(defvar ajb-initial-bs-max-window-height bs-max-window-height)
(defvar ajb-initial-bs-attributes-list bs-attributes-list)
(defvar ajb-initial-bs-configuration bs-current-configuration)

;; cache current ace jump mode scope
(defvar ajb-initial-ace-jump-mode-scope ace-jump-mode-scope)

;; when `perspective' mode is found and loaded, add a `bs-configuration' for it
(when (require 'perspective nil 'noerror)
  (add-to-list 'bs-configurations
               '("persp" nil nil nil
                 (lambda (buf)
                   (with-current-buffer buf
                     (not (member buf (persp-buffers persp-curr)))))) nil))

(defvar ajb-bs-configuration (if (require 'perspective nil 'noerror) "persp" "all"))

;; settings for a barebones `bs' switcher
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
  (setq bs-header-lines-length ajb-bs-header-lines-length)
  (setq bs-max-window-height ajb-bs-max-window-height)
  (setq bs-attributes-list ajb-bs-attributes-list))

(defun ace-jump-buffer-turn-off ()
  (remove-hook 'ace-jump-mode-end-hook 'ace-jump-buffer-hook)
  (ad-deactivate 'bs--show-header)
  (setq ace-jump-mode-scope ajb-initial-ace-jump-mode-scope)
  (setq bs-header-lines-length ajb-initial-bs-header-lines-length)
  (setq bs-max-window-height ajb-initial-bs-max-window-height)
  (setq bs-attributes-list ajb-initial-bs-attributes-list))

(defun ace-jump-buffer-hook ()
  "On the end of ace jump, select the buffer at the current line."
  (when (and ace-jump-buffer-mode
             (string-match (buffer-name) "*buffer-selection*"))
    (bs-select)
    (ace-jump-buffer-turn-off)))

(defun ace-jump-buffer ()
  (interactive)
  (when ace-jump-buffer-mode
    (ace-jump-buffer-turn-on)
    (bs--show-with-configuration ajb-bs-configuration)
    (beginning-of-buffer)
    (set (make-local-variable 'ace-jump-mode-scope) 'window)
    (call-interactively 'ace-jump-line-mode)
    (define-key overriding-local-map (kbd "C-g") 'ace-jump-buffer-exit)
    (define-key overriding-local-map [t] 'ace-jump-buffer-done)))

(defun ace-jump-buffer-done ()
  (interactive)
  (when ace-jump-current-mode
    (ace-jump-done))
  (ace-jump-buffer-turn-off))

(defun ace-jump-buffer-exit ()
  (interactive)
  (if (and ace-jump-buffer-mode
           (string-match (buffer-name) "*buffer-selection*"))
      (progn
        (ace-jump-buffer-done)
        (bs-kill)
        (kill-buffer "*buffer-selection*"))
    (let* ((ace-jump-mode nil)
           (original-func (key-binding (kbd "C-g"))))
      (call-interactively original-func))))

;;;###autoload
(define-minor-mode ace-jump-buffer-mode
  "Use `ace-jump-mode` to quickly toggle between buffer"
  :init-value nil
  :lighter ""
  :group 'ace-jump-buffer
  :global t)

(provide 'ace-jump-buffer)
;;; ace-jump-buffer.el ends here

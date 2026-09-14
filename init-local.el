;;; init-local.el --- Load the after purcell configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file my additional configuration.

;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))

(defun kill-default-buffer ()
  "Kill buffer without prompt"
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)

(use-package google-this
  :ensure t)
(use-package poly-R
  :ensure t)
(use-package julia-mode
  :ensure t)

(use-package swift-mode
  :ensure t)

;;using purcell emacs starter kit add delete trailing whitespace in local-init.el ?
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; https://www.polyomica.com/weekly-emacs-tip-5-make-sure-files-always-end-with-a-newline/
(setq require-final-newline t)



(provide 'init-local)

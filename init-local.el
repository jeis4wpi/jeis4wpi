;;; init-local.el --- Load the after purcell configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file my additional configuration.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

(display-time)

(defun kill-default-buffer ()
  "Kill buffer without prompt"
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)


(add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

(add-to-list 'auto-mode-alist '("\\.pg\\'" . cperl-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))

;; installed by package-install
(global-set-key (kbd "C-x g") 'google-this-mode-submap)
(google-this-mode 1)

;;using purcell emacs starter kit add delete trailing whitespace in local-init.el ?
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; https://www.polyomica.com/weekly-emacs-tip-5-make-sure-files-always-end-with-a-newline/
(setq require-final-newline t)

(provide 'init-local)

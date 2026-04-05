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
(global-set-key (kbd "C-x g") 'google-this)
(google-this-mode 1)

;;using purcell emacs starter kit add delete trailing whitespace in local-init.el ?
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; https://www.polyomica.com/weekly-emacs-tip-5-make-sure-files-always-end-with-a-newline/
(setq require-final-newline t)


;;;go install golang.org/x/tools/gopls@latest
;;;emacs config for go mode for go code

;; (use-package go-mode
;;   :mode "\\.go\\'"
;;   :config
;;   (defun my/go-mode-setup ()
;;     "Setup go-mode with LSP and formatting."
;;     (setq indent-tabs-mode nil)
;;     (setq tab-width 4)
;;     (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;     (add-hook 'before-save-hook #'lsp-organize-imports t t)
;;     (lsp-deferred))
;;   :hook
;;   ;;  (go-mode-hook #'my/go-mode-setup)
;;   (go-mode-hook #'/Users/johne/Public/gpu-operator)
;;   :custom
;;   (gofmt-command "goimports"))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook ((go-mode . lsp-deferred))
;;   :config
;;   (setq lsp-prefer-flymake nil))

;; https://go.dev/gopls/editor/emacs
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)
;; Optional: load other packages before eglot to enable eglot integrations.
;;(require 'company)
;;(require 'yasnippet)

(require 'go-mode)
(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-before-save)

(setq-default eglot-workspace-configuration
              '((:gopls .
                        ((staticcheck . t)
                         (matcher . "CaseSensitive")))))


;; To automatically organize imports before saving, add a hook
(add-hook 'before-save-hook
          (lambda ()
            (call-interactively 'eglot-code-action-organize-imports))
          nil t)

;; still looking for cmake mode
;; treesit-error "Cannot find recipe for this language" cmake
;;
;; didn't match Mickey P.
;;
;; (add-to-list 'treesit-language-source-alist
;;              '(cmake "https://github.com/Kitware/CMake"))


;; what about - still very broken
;; https://www.reddit.com/r/emacs/comments/15tz3vx/treesit_error_while_installing_grammar/
(add-to-list 'treesit-language-source-alist
             '(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))




;;;https://github.com/JuliaEditorSupport/julia-emacs
(use-package julia-mode
  :ensure t)

;;; emacs cmake mode automatically ?
(use-package cmake-mode
  :mode (("\\.cmake\\'" . cmake-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)))

;;; example emacs config straight set to t ?
;; Ensure use-package is available
;;(straight-use-package 'use-package)

;; Enable straight.el for all use-package declarationsnn
;;(setq straight-use-package-by-default t)
;;(setq use-package-always-defer t) ;; Optional: defer loading by default


;; close but startup fails.
;; (package-vc-install '(combobulate :url "https://github.com/mickeynp/combobulate"))

;; (use-package combobulate
;;   :ensure t
;;   :hook (julia-mode . combobulate-mode))

;; (use-package julia-mode
;;   :ensure t
;;   :mode "\\.jl\\'"
;;   :interpreter ("julia" . julia-mode))

;; (use-package julia-snail
;;   :ensure t
;;   :hook (julia-mode . julia-snail-mode))


;; install zig grammar for prelude emacs
(use-package zig-ts-mode
  :vc (:url "https://codeberg.org/meow_king/zig-ts-mode" :rev :newest)
  :config
  (add-to-list 'eglot-server-programs '(zig-ts-mode . ("zls")))
  ;; Ensure grammars are installed if using treesit-auto
  (require 'treesit-auto)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;; how do I install tree sitter grammar for go emacs
;; (setq treesit-language-source-alist
;;       '((go "https://github.com/tree-sitter/tree-sitter-go")
;;         (gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
;;       ((python "https://github.com/tree-sitter/tree-sitter-python")))

;; grammer install python for emacs
;; (setq treesit-language-source-alist
;;       '((python "https://github.com/tree-sitter/tree-sitter-python")))

;; M-x treesit-install-language-grammar RET python RET

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))


;;
;; (setq treesit-language-source-alist
;;       '((dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.23.6")))

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter + dockerfile + gocmd

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


;;; tree sitter grammar for lua code for emacs configs

(use-package lua-ts-mode
  :ensure t ;; Installs from MELPA or ELPA
  :mode ("\\.lua\\'" . lua-ts-mode)
  :config
  (add-to-list 'treesit-language-source-alist
               '(lua "https://github.com/Azganoth/tree-sitter-lua")))

;; Run this command manually after adding the source above:
;; M-x treesit-install-language-grammar RET lua RET


;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(provide 'init-local)

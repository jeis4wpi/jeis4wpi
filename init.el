

(load "/opt/homebrew/opt/scimax/share/emacs/site-lisp/scimax/init.el")

;; Shorten yes/no answers to y/n
(fset 'yes-or-no-p 'y-or-n-p)

(setq global-display-line-number-mode 't)

(defun kill-default-buffer ()
  "Kill buffer without prompt"
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)
;;;(set-scroll-bar-mode 'nil)
;;;(setq electric-pair-mode 'enabled)
(electric-pair-mode 1)
(blink-cursor-mode 1)
(menu-bar-mode -1)

;;; Highlight tabulations
(setq-default highlight-tabs t)

;;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

;; https://howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


(add-hook 'perl-mode-hook 'flymake-perlcritic-setup)

;; emacs + perlcritic
;; (defun turn-on-perlcritic-mode ()
;;   (perlcritic-mode +1))

;; (eval-after-load "cperl-mode"
;;   '(add-hook 'cperl-mode-hook 'turn-on-perlcritic-mode))


;; (require 'perltidy)
;; (setq perltidy-on-save t)
;; https://www.emacswiki.org/emacs/CPerlMode/
(defalias 'perl-mode 'cperl-mode)


(setq mail-user-agent 'gnus-user-agent)
(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
      org-msg-startup "hidestars indent inlineimages"
      org-msg-greeting-fmt "\nHi%s,\n\n"
      org-msg-recipient-names '(("jpei4wpi@outlook.com" . "John"))
      org-msg-greeting-name-limit 3
      org-msg-default-alternatives '((new		. (text html))
				     (reply-to-html	. (text html))
				     (reply-to-text	. (text)))
      org-msg-convert-citation t
      org-msg-signature "

 Regards,

 #+begin_signature
 -- John
 #+end_signature")
(org-msg-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   '("https://sachachua.com/blog/feed/" "https://phys.org/rss-feed/"
     "https://phys.org/feeds" "https://www.stembits.org/blog-feed.xml"
     "http://rss.slashdot.org/Slashdot/slashdotMain"
     "https://www.space.com/feeds.xml"
     "https://www.livescience.com/feeds.xml"
     "https://www.science.org/action/showFeed?type=etoc&feed=rss&jc=sciimmunol"
     "https://www.science.org/action/showFeed?type=axatoc&feed=rss&jc=science"
     "https://www.sciencedaily.com/rss/top/technology.xml"
     "https://www.sciencedaily.com/rss/all.xml"
     "http://planet.emacsen.org/atom.xml"
     "http://planetpython.org/rss20.xml")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;; https://stackoverflow.com/questions/79555604/run-ruff-in-emacs

(defun ruff-check ()
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
	(async-shell-command
         (format "ruff check --select ALL %s" (shell-quote-argument current-file))
	 )
      )
    )
  )

(defun ruff-fix ()
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
	(progn
          (shell-command
           (format "ruff check --select ALL --fix %s" (shell-quote-argument current-file))
           )
          (revert-buffer t t t)
	  )
      )
    )
  )

;; emacs perl with file extention of pg ?
(add-to-list 'auto-mode-alist '("\\.pg\\'" . cperl-mode))

;; https://arnesonium.com/2023/08/configuring-emacs-29-1-for-golang
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; https://andrewfavia.dev/posts/emacs-as-python-ide-again/#:~:text=Eglot%20is%20now%20an%20Emacs%20built%20in,built%2Din%20Emacs%20tools%20like%20xref%20and%20eldoc.


;; simple example emacs eglot with bash perl python go and rust
;; Ensure use-package is available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package
(require 'use-package)

;; Enable Eglot globally
;(use-package eglot
;  :straight t
;  :defer t
;  :hook ((bash-mode . eglot-ensure)
;         (perl-mode . eglot-ensure)
;         (python-mode . eglot-ensure)
;         (go-mode . eglot-ensure)
;         (rust-mode . eglot-ensure))
;  :config
  ;; Configure LSP servers for each language
;  (add-to-list 'eglot-server-programs
;               '(bash-mode . ("bash-language-server" "start")))
;  (add-to-list 'eglot-server-programs
;               '(perl-mode . ("perl-language-server")))
;  (add-to-list 'eglot-server-programs
;               '(python-mode . ("pyright-langserver" "--stdio")))
;  (add-to-list 'eglot-server-programs
;               '(go-mode . ("gopls" "-mode=stdio")))
;  (add-to-list 'eglot-server-programs
;               '(rust-mode . ("rust-analyzer")))
  ;; Optional: Enable company-mode for completion
;  (use-package company
;    :straight t
;    :hook (eglot-mode . company-mode)
;    :config
;    (setq company-minimum-prefix-length 1
;          company-idle-delay 0.1))
  ;; Optional: Enable eldoc for inline documentation
;  (use-package eldoc
;   :straight t
;   :hook (eglot-mode . eldoc-mode)))


;;simple example emacs eglot with bash perl python go R TeX and rust

(use-package eglot
  :ensure t
  :hook ((bash-mode . eglot-ensure)
         (perl-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (R-mode . eglot-ensure)
         (latex-mode . eglot-ensure)
         (rustic-mode . eglot-ensure))
  :config
  ;; Bash: Use bash-language-server
  (add-to-list 'eglot-server-programs '(bash-mode . ("bash-language-server" "start")))

  ;; Perl: Use perl-language-server (if available)
  (add-to-list 'eglot-server-programs '(perl-mode . ("perl-language-server")))

  ;; Python: Use pylsp (python-lsp-server)
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

  ;; Go: Use gopls
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))

  ;; R: Use languageserver (R's language server)
  (add-to-list 'eglot-server-programs '(R-mode . ("languageserver")))

  ;; TeX: Use digestif (Lua-based server)
  (add-to-list 'eglot-server-programs '(latex-mode . ("digestif")))

  ;; Rust: Use rust-analyzer
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))

  ;; Optional: Enable formatting on save
  (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'go-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'rustic-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  )


;; (use-package uv-mode
;;   :hook (python-mode . uv-mode-auto-activate-hook))

(require 'uv-mode)
(add-hook 'python-mode-hook #'uv-mode-auto-activate-hook)

;; https://www.reddit.com/r/emacs/comments/13ezfq9/python_do_you_install_the_language_server/
;; pipx install python-lsp-server
;; pipx inject python-lsp-server pylsp-mypy

;; emacs eglot yaml lsp
(add-hook 'yaml-mode-hook (lambda () (eglot-ensure)))
;; https://redpenguin101.github.io/html/posts/2025_11_23_emacs_for_code_editing.html
;; (setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(global-hl-line-mode +1)
(global-subword-mode 1)

;; emacs lsp typescript languange server
;; npm install -g typescript-language-server typescript

(add-hook 'typescript-mode-hook #'eglot-hover-mode) ; or another eglot function
;; emacs javascript lsp
(add-hook 'js-mode-hook #'eglot-ensure)

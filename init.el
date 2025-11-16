

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

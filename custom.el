(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))))
 '(blink-cursor-mode nil)
 '(case-replace nil)
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-replace nil)
 '(delete-old-versions t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(evil-ex-search-highlight-all nil)
 '(evil-ex-substitute-case (quote sensitive))
 '(evil-insert-state-modes (quote (comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode nodejs-repl-mode)))
 '(evil-search-module (quote evil-search))
 '(global-auto-revert-mode t)
 '(goto-alternate-git-file-patterns (quote (("\\.spec\\.js$" . ".js") ("\\.h$" . ".c") ("Test\\.java$" . ".java") ("\\.ko$" . ".less") ("\\.less$" . ".ko"))))
 '(hl-paren-colors (quote ("green" "yellow" "magenta" "orange")))
 '(ido-auto-merge-delay-time 100)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(kept-new-versions 3)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(scroll-margin 4)
 '(scroll-step 1)
 '(scss-compile-at-save nil)
 '(sgml-basic-offset 4)
 '(surround-pairs-alist (quote ((40 "(" . ")") (91 "[" . "]") (123 "{" . "}") (41 "(" . ")") (93 "[" . "]") (125 "{" . "}") (35 "#{" . "}") (98 "(" . ")") (66 "{" . "}") (62 "<" . ">") (116 . surround-read-tag) (60 . surround-read-tag))))
 '(tab-width 4)
 '(timeclock-file "~/Dropbox/timelog")
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(version-control t)
 '(visible-bell t)
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary t)
 '(yas-prompt-functions (quote (yas-ido-prompt yas-x-prompt yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-no-prompt)))
 '(yas/prompt-functions (quote (yas/ido-prompt yas/x-prompt yas/dropdown-prompt yas/completing-prompt yas/no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(error ((t (:underline t))))
 '(lazy-highlight ((t (:background "gray17")))))

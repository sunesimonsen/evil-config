(require 'thingatpt)

;;; Setup environment
(setq dotfiles (shell-command-to-string ". ~/.bashrc; echo -n $DOTFILES"))
(setenv "DOTFILES" dotfiles)
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;;; Customize
(setq custom-file (concat dotfiles "/evil-config/custom.el"))
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

;;; IDo mode
(ido-mode 't)
(setq ido-enable-flex-matching 't)
(define-key ido-file-dir-completion-map (kbd "C-c C-s")
  (lambda()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))

(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;;; Font
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

;;; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)

(define-key key-translation-map (kbd "½") (kbd "$"))

(require 'uniquify)
(set-variable 'uniquify-buffer-name-style 'forward)

;;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.ko$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cjson$" . json-mode))

(defun find-file-in-project-dir ()
  (interactive)
  (ido-find-file-in-dir "~/Projects"))

(setq
 el-get-sources
 '((:name el-get)
   (:name evil-surround)
   (:name evil-numbers)
   (:name flymake-cursor)
   (:name markdown-mode)
   (:name ace-jump-mode)
   (:name evil-nerd-commenter)
   (:name evil-little-word)
   (:name ido-goto-symbol)
   (:name ido-bookmark-jump)
   (:name todotxt-mode)
   (:name find-file-in-git-repo)
   (:name evil-walk-on-the-edge)
   (:name magit)
   (:name runtests)
   (:name less-css-mode)
   (:name json-mode)

   (:name diff-hl
          :after
          (progn
            (global-diff-hl-mode 't)
            (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
            (unless (display-graphic-p)
              (diff-hl-margin-mode))))

   (:name nodejs-repl
          :after
          (progn
            (defun nodejs-repl-send-region ()
              (interactive)
              (when (use-region-p)
                (let ((selection (buffer-substring-no-properties  (region-beginning) (region-end))))
                  (with-current-buffer (get-buffer "*nodejs*")
                    (message selection)
                    (end-of-buffer)
                    (insert selection)
                    (comint-send-input)
                    (end-of-buffer)))))))


   (:name color-theme
          :after
          (progn
            (color-theme-initialize)
            (defun light-theme ()
              (interactive)
              (color-theme-standard))

            (defun dark-theme ()
              (interactive)
              (color-theme-billw))

            (dark-theme)))


   (:name evil
          :after
          (progn
            (evil-mode 't)

            (loop for (mode . state) in '((git-commit-mode . insert)
                                          (git-rebase-mode . emacs)
                                          (magit-branch-manager-mode . emacs))
                  do (evil-set-initial-state mode state))

            (define-key evil-motion-state-map "æ" 'evil-ex)
            (define-key evil-normal-state-map "Æ" 'evil-execute-macro)

            (define-key evil-motion-state-map "å" 'evil-move-backward-paren)
            (define-key evil-motion-state-map "ø" 'evil-move-forward-paren)

            (define-key evil-motion-state-map "Å" 'evil-backward-paragraph)
            (define-key evil-motion-state-map "Ø" 'evil-forward-paragraph)

            (define-key evil-motion-state-map (kbd "SPC") 'evil-ex-search-forward)
            (define-key evil-motion-state-map (kbd "C-SPC") 'evil-ex-search-backward)

            (define-key evil-visual-state-map (kbd "C-æ") 'evil-exit-visual-state)
            (define-key evil-insert-state-map (kbd "C-æ") 'evil-normal-state)
            (define-key evil-replace-state-map (kbd "C-æ") 'evil-normal-state)
            (define-key evil-normal-state-map (kbd "C-æ") 'evil-force-normal-state)

            (define-key evil-outer-text-objects-map "å" 'evil-a-bracket)
            (define-key evil-outer-text-objects-map "ø" 'evil-a-bracket)
            (define-key evil-outer-text-objects-map "Å" 'evil-a-curly)
            (define-key evil-outer-text-objects-map "Ø" 'evil-a-curly)

            (define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
            (define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
            (define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
            (define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)
            (define-key evil-motion-state-map (kbd "M-n") 'evil-window-new)
            (define-key evil-motion-state-map (kbd "M-o") 'delete-other-windows)
            (define-key evil-motion-state-map (kbd "M-p") 'evil-window-mru)
            (define-key evil-motion-state-map (kbd "M-c") 'delete-window)
            (define-key evil-motion-state-map (kbd "M-v") 'split-window-horizontally)
            (define-key evil-motion-state-map (kbd "M-s") 'split-window-vertically)
            (define-key evil-motion-state-map (kbd "M--") 'evil-window-decrease-height)
            (define-key evil-motion-state-map (kbd "M-+") 'evil-window-increase-height)


            (define-key evil-inner-text-objects-map "å" 'evil-inner-bracket)
            (define-key evil-inner-text-objects-map "ø" 'evil-inner-bracket)
            (define-key evil-inner-text-objects-map "Å" 'evil-inner-curly)
            (define-key evil-inner-text-objects-map "Ø" 'evil-inner-curly)

            (define-key evil-motion-state-map "gt" 'evil-jump-to-tag)
            (define-key evil-motion-state-map "gk" 'evil-ace-jump-word-mode)
            (define-key evil-motion-state-map "gå" 'flycheck-previous-error)
            (define-key evil-motion-state-map "gø" 'flycheck-next-error)
            (define-key evil-motion-state-map (kbd "C-å") 'diff-hl-previous-hunk)
            (define-key evil-motion-state-map (kbd "C-ø") 'diff-hl-next-hunk)

            (define-key evil-visual-state-map (kbd "<tab>") ">gv")
            (define-key evil-visual-state-map (kbd "<backtab>") "<gv")

            (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
            (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

            (define-prefix-command 'evil-leader-map)
            (define-key evil-leader-map "," 'evil-repeat-find-char)
            (define-key evil-leader-map "p" 'find-file-in-git-repo)
            (define-key evil-leader-map "o" 'ido-goto-symbol)
            (define-key evil-leader-map "e" 'ido-find-file)
            (define-key evil-leader-map "b" 'ido-switch-buffer)
            (define-key evil-leader-map "w" 'evil-write)
            (define-key evil-leader-map "a" 'goto-alternate-git-file)
            (define-key evil-leader-map "g" 'find-file-in-project-dir)
            (define-key evil-leader-map "s" 'magit-status)
            (define-key evil-leader-map "vr" 'diff-hl-revert-hunk)
            (define-key evil-leader-map "vd" 'vc-diff)
            (define-key evil-leader-map "va" 'vc-annotate)
            (define-key evil-leader-map "vb" 'magit-blame-mode)
            (define-key evil-leader-map "vl" 'magit-log)
            (define-key evil-leader-map "t" 'runtests)
            (define-key evil-motion-state-map "," 'evil-leader-map)

            (define-key evil-motion-state-map ";" 'evil-repeat-find-char-reverse)

            (defun query-replace-symbol-at-point (replacement)
              "Query replace the symbol at point with the given replacement"
              (interactive "swith: ")
              (beginning-of-thing 'symbol)
              (let* ((symbol-at-point (thing-at-point 'symbol))
                     (pattern (format "\\_<%s\\_>" (regexp-quote symbol-at-point))))
                (query-replace-regexp pattern replacement)))

            (define-key evil-normal-state-map "gs" 'query-replace-symbol-at-point)

            (define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
            (define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
            (define-key minibuffer-local-completion-map [escape]
              'keyboard-escape-quit)
            (define-key minibuffer-local-must-match-map [escape]
              'keyboard-escape-quit)
            (define-key minibuffer-local-isearch-map [escape]
              'keyboard-escape-quit)

            (evil-ex-define-cmd "bo[okmarks]" 'ido-bookmark-jump)
            (evil-ex-define-cmd "p[roject]" 'ido-project-root-find-file)
            (evil-ex-define-cmd "b[uffer]" 'ido-switch-buffer)
            (evil-ex-define-cmd "e[dit]" 'ido-find-file)

            (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
              "K" 'magit-discard-item
              "L" 'magit-key-mode-popup-logging)

            (evil-add-hjkl-bindings magit-status-mode-map 'emacs
              "K" 'magit-discard-item
              "L" 'magit-key-mode-popup-logging
              "H" 'magit-toggle-diff-refine-hunk)

            (evil-add-hjkl-bindings bookmark-bmenu-mode-map 'emacs
              "K" 'bookmark-bmenu-delete
              "L" 'bookmark-bmenu-load
              )


            (add-hook 'compilation-mode-hook
                      (lambda ()
                        (define-key compilation-mode-map "f" 'next-error-follow-minor-mode)))
            (add-hook 'grep-setup-hook
                      (lambda ()
                        (define-key grep-mode-map "f" 'next-error-follow-minor-mode)))

            (evil-add-hjkl-bindings occur-mode-map 'emacs)

            (evil-add-hjkl-bindings magit-log-mode-map 'emacs
              "L" 'magit-key-mode-popup-logging)
            ))

   (:name smex
          :after
          (progn
            ;; SMEX
            (setq smex-save-file "~/.emacs.d/.smex-items")

            (defun lazy-load-smex ()
              (interactive)
              (or (boundp 'smex-cache)
                  (smex-initialize))
              (global-set-key [(meta x)] 'smex)
              (global-set-key [menu] 'smex)
              (smex))

            (global-set-key [menu] 'lazy-load-smex)
            (global-set-key [(meta x)] 'lazy-load-smex)))

   (:name yasnippet
          :after
          (progn
            (setq yas-snippet-dirs
                  (list (concat dotfiles "/evil-config/snippets")))
            (yas-global-mode 't)))

   (:name flycheck
          :after
          (progn

            (flycheck-def-config-file-var flycheck-jscs javascript-jscs ".jscsrc"
              :safe #'stringp)

            (flycheck-define-checker javascript-jscs
              "A JavaScript code style checker. See URL `https://github.com/mdevils/node-jscs'."
              :command ("jscs" "--reporter" "checkstyle"
                        (config-file "--config" flycheck-jscs)
                        source)
              :error-parser flycheck-parse-checkstyle
              :modes (js-mode js2-mode js3-mode)
              :next-checkers (javascript-jshint))

            (add-to-list 'flycheck-checkers 'javascript-jscs)
            (add-hook 'js-mode-hook (lambda () (flycheck-mode 't)))))
   ))

(setq my:el-get-packages
      (loop for src in el-get-sources collect (el-get-source-name src)))

(add-to-list 'el-get-recipe-path (concat dotfiles "/evil-config/recipes"))
(el-get 'sync my:el-get-packages)

(add-hook 'ido-setup-hook '(lambda ()
                             (define-key ido-completion-map (kbd "<C-return>") 'ido-select-text)
                             (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
                             (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
                             (define-key ido-completion-map (kbd "<C-delete>") 'ido-delete-file-at-head)
                             ))

;;; IMenu
(defun my-js-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "function\\s-+\\([^ ]+\\)(" 1)
                               (nil "\\.\\([^\\. ]+\\)\\s-*=\\s-*function\\s-*(" 1)))))

;;; TodoTxt
(setq todotxt-default-file (expand-file-name "~/Dropbox/todo/todo.txt"))
(add-to-list 'auto-mode-alist '("/todo.txt$" . todotxt-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; JavaScript mode hook
(add-hook 'js-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'my-js-imenu-make-index)))

(add-hook 'json-mode-hook
          (lambda ()
            (when (and (buffer-file-name) (string= "package.json" (file-name-nondirectory (buffer-file-name))))
              (setq-local js-indent-level 2))))

;;; Make ffap use line numbers
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (goto-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))

(server-start)

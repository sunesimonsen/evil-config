# Emacs Evil setup:

## Window management:

```
ALT-h evil-window-left
ALT-j evil-window-down
ALT-k evil-window-up
ALT-l evil-window-right
ALT-n evil-window-new
ALT-o delete-other-windows
ALT-p evil-window-mru
ALT-c delete-window
ALT-v split-window-horizontally
ALT-s split-window-vertically
ALT-- evil-window-decrease-height
ALT-+ evil-window-increase-height
```

## File management

```
,p find-file-in-git-repo
,o ido-goto-symbol
,e ido-find-file
,b ido-switch-buffer
,w evil-write
,a goto-alternate-git-file
,g find-file-in-project-dir
,s magit-status
,vr diff-hl-revert-hunk
,vd vc-diff
,va vc-annotate
,vb magit-blame-mode
,vl magit-log
,, evilnc-comment-or-uncomment-lines
,cl evilnc-comment-or-uncomment-to-the-line
,cc evilnc-copy-and-comment-lines
,cp evilnc-comment-or-uncomment-paragraphs
,t runtests
```

## Navigation

```
Space evil-ex-search-forward
C-Space evil-ex-search-backward

å" 'evil-move-backward-paren
ø" 'evil-move-forward-paren

Å" 'evil-backward-paragraph
Ø" 'evil-forward-paragraph

Control-å diff-hl-previous-hunk
Control-ø diff-hl-next-hunk

gå flycheck-previous-error
gø flycheck-next-error

gk evil-ace-jump-word-mode
```

# Convenience

```
æ" 'evil-ex
Æ" 'evil-execute-macro

Control-æ evil-normal-state
```

# Indentation in visual mode

```
tab to indent selected lines
Shift-tab
```

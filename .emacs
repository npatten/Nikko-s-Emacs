(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (wheatgrass))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; all the package managers!!
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
            '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))


;; swap around mod keys for mac
(setq mac-command-modifier 'control)
(setq mac-command-key-is-meta t)
(setq mac-control-modifier 'meta)


;;; Disable unused UI elements.
;;;  in a way that doesn't load them just for them to disabled.
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode -1)
(setq-default inhibit-startup-screen t)

;;ido all the things
(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode t)

;; share OS clipboard
(desktop-save-mode 1)
(setq x-select-enable-clipboard t)
(transient-mark-mode 1) ;; No region when it is not highlighted
(delete-selection-mode 1)

;;silence annoying beeps
(setq ring-bell-function 'ignore)

;; smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)

;;;Emacs Desktop!
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

;;; Highlight open and close parenthesis when the point is on them.
(show-paren-mode t)
(setq show-paren-delay 0)

;;Show Line & Col numbers
(line-number-mode 1)
(global-linum-mode 1)
(column-number-mode t)

;;; Indent with spaces, not tabs.
(setq-default indent-tabs-mode nil)

;;; Set tab width to 2.
(setq-default tab-width 2)

;;Font
(set-frame-font "Menlo Regular 14")

;;;; Clojure!
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;;cider
(unless (package-installed-p 'cider)
  (package-install 'cider))
(require 'cider)

;;Paredit
(require 'paredit)
(dolist (hook '(scheme-mode-hook
                emacs-lisp-mode-hook
                lisp-mode-hook
                clojure-mode-hook))
  (add-hook hook 'enable-paredit-mode))

;; Remove trailing whitespace before saves
;; (dolist (hook programmig-modes)
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun NikkoNav-indent-for-tab-command ()
  "This is to fix `indent-for-tab-command' for `NikkoNav'.
It runs [tab] or C-i with `gvol-mode' nil because `NikkoNav'
binds C-i to a different command.  Ideally this should take into
account window system so that it can DTRT in a terminal (whatever
the right thing is)."
  (interactive)
  (let* ((NikkoNav nil)
         (command (or (key-binding [tab])
                      (key-binding "\C-i"))))
    ;; This is to satisfy `python-indent-line' which checks
    ;; `this-command' to cycle
    (setq this-command 'indent-for-tab-command)
    ;; Make people think this was called with C-i.  This allows
    ;; `self-insert-command' to work
    (setq last-command-event 9)
    (call-interactively command)))

;;; let undo
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)



;;; Custom Minor Mode for nav
(define-minor-mode NikkoNav
  "Nikko's custom navigation which uses the i j k l keys in conjunction with control and
   meta modifiers to have a more gamer friendly navigation system."
  t
  ;; The indicator for the mode line.
  " NikkoNav"
  ;; The minor mode keymap
  `(
    (,(kbd "C-i") .  previous-line)
    (,(kbd "C-k") .  next-line)
    (,(kbd "C-j") .  backward-char)
    (,(kbd "C-l") .  forward-char)
    (,(kbd "M-i") .  backward-page)
    (,(kbd "M-k") .  forward-page)
    (,(kbd "M-j") .  backward-word)
    (,(kbd "M-l") .  forward-word)
    (,(kbd "<tab>") .  NikkoNav-indent-for-tab-command)
    (,(kbd "C-;") . comment-region)
    (,(kbd "C-:") . uncomment-region)
    (,(kbd "C-f") . isearch-forward)
    (,(kbd "C-s") . save-buffer)
    (,(kbd "C-p") . projectile-find-files)
    (,(kbd "C-S-k") . kill-line)
))

(NikkoNav 1)

;;; This prevents NikkoNav from breaking auto complete in the minibuffer
(defun my-minibuffer-setup-hook ()
  (NikkoNav 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; projectile mode!
(projectile-global-mode)
(setq projectile-indexing-method 'native)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;;; deft for notes
(require 'deft)
(when (require 'deft nil 'noerror)
  (setq
     deft-extension "org"
     deft-directory "~/Google Drive/emacs-notes/"
     deft-text-mode 'org-mode
     deft-auto-save-interval 30.0))

(global-set-key [f8] 'deft)

;; IRC!
(add-to-list 'load-path "~/elisp/erc")
(require 'erc)

(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs" "#clojure" "#angularjs" "#freestream"
             "#git" )
            ("crystalia.net" "#crystalia")))

(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(require 'erc-match)
(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
    (setq erc-beep-match-types '(current-nick keyword))

;;; spelling
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(erc-spelling-mode 1)

;; logging:
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))
;; end logging

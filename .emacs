

;;; Disable unused UI elements.
;;;  in a way that doesn't load them just for them to disabled.
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode 1)
(setq-default inhibit-startup-screen t)
 
(global-auto-revert-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Package Management Stuff ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; Import packages and add additional package repositories
(require 'package)
;; (add-to-list 'package-archives
;;   '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives 
;;   '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
  ;;Zomgz, it's stable
  '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)


;;all the toys
(defvar nikko/packages '(ace-jump-mode
                         auto-complete
                         cider
                         clojure-mode
                         clojure-snippets
                         ;;clojure-test-mode
                         closure-lint-mode
                         dash
                         deft
                         epl
                         expand-region
                         flx-ido
                         flex-autopair
                         flycheck
                         git-commit-mode
                         helm
                         helm-projectile
                         helm-themes
                         magit
                         multiple-cursors
                         paredit
                         pkg-info
                         popup
                         projectile
                         rainbow-delimiters
                         undo-tree
                         ))

;;load em all up
(mapc (lambda (package) (when (not (package-installed-p package)) (package-install package)))
       nikko/packages)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Make emacs fun to work with ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; raaaaainbows
(global-rainbow-delimiters-mode t)

;; like I'm going to type out 'yes', pfffft.
(fset 'yes-or-no-p 'y-or-n-p)

;;; Multiple cursors
(require 'multiple-cursors)
(multiple-cursors-mode 1)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'ace-jump-mode)
(global-set-key (kbd "C-0") 'ace-jump-mode)


;;helm-config
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key [(meta x)] 'helm-M-x)
(global-set-key [(ctrl x) (ctrl f)] 'helm-find-files)
(global-set-key [(ctrl x) (b)] 'helm-buffers-list)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;;ido all the things
(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode t)

;;; projectile mode!
(projectile-global-mode)
;; (setq projectile-indexing-method 'native)
;;(setq projectile-enable-caching t)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;;;; Set up YASnippet
(require 'yasnippet)
(yas-global-mode t)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

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

;;; magit for git awesomeness
(autoload 'magit-status "magit" nil t)
(global-set-key [f7] 'magit-status)

;;;; Enable Expand Region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;; Enable Undo-Tree for better undo handling
(require 'undo-tree)
(global-undo-tree-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Notes ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; deft for notes
(require 'deft)
(when (require 'deft nil 'noerror)
  (setq
     deft-extension "org"
     deft-directory "~/Google Drive/emacs-notes/"
     deft-text-mode 'org-mode
     deft-auto-save-interval 30.0))

(global-set-key [f8] 'deft)


;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)


;;; Visual line mode for org + text buffers
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; We want in line syntax for org-babel
(setq org-src-fontify-natively t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Language-specifics  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Paredit
(require 'paredit)

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)



;; quick key change for paredit
(eval-after-load "paredit"
  '(progn
    (define-key paredit-mode-map (kbd "M-e") 'paredit-forward)
    (define-key paredit-mode-map (kbd "M-a") 'paredit-backward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))  
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'clojure-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
 
(add-hook 'clojure-mode-hook 
	  (lambda ()
	    (local-set-key (kbd "C-c C-e") 'cider-pprint-eval-last-sexp)))

;; (defun jim/cider-test-is-test-ns (ns)
;;   (let ((suffix "-test")) 
;;     (string-match (rx-to-string `(: ,suffix eos) t) ns)))
 
;; (defun jim/cider-test-impl-ns-fn (ns)
;;   (when ns
;;     (let ((suffix "-test"))
;;       (if (string-match (rx-to-string `(: ,suffix eos) t) ns)
;;           (s-replace suffix "" ns)
;;         ns))))
 
;; (defun jim/cider-test-jump-around ()
;;   (interactive)
;;   (let ((ns (cider-current-ns)))
;;     (switch-to-buffer
;;      (cider-find-buffer
;;       (if (jim/cider-test-is-test-ns ns)
;;           (jim/cider-test-impl-ns-fn ns)
;;         (cider-test-default-test-ns-fn ns))))))
 
(defun jim/clojure-keybinds ()
  (interactive)
  (define-key clojure-mode-map (kbd "C-c C-e") 'cider-pprint-eval-last-sexp)
  (define-key cider-mode-map (kbd "C-c C-t") 'jim/cider-test-jump-around)
  (define-key cider-mode-map (kbd "C-c M-,") 'cider-test-run-tests))
 
(add-hook 'clojure-mode-hook 'jim/clojure-keybinds)

;;Stuar Sierra's refresh workflow
(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(define-key clojure-mode-map (kbd "M-r") 'cider-namespace-refresh)

;;set cider as the backend for org-babel
(setq org-babel-clojure-backend 'cider)


;;; Lisp

(add-to-list 'lisp-mode-hook 'enable-paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; IRC ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/elisp/erc")
(require 'erc)

(require 'erc-join)
(erc-autojoin-mode 0)
(setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs" "#clojure" "#angularjs" "#freestream"
             "#git" )))

;;(erc :server  "chat.freenode.net" :port 6667 :nick "npatten")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Nikko's Crazy Keys  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; swap around mod keys for mac
(setq mac-command-modifier 'control)
(setq mac-command-key-is-meta t)
(setq mac-control-modifier 'meta)


;;; C-z undo
(global-unset-key (kbd "C-z"))
;;; Make undo's better
(global-set-key (kbd "C-z") 'undo)

(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-?") 'uncomment-region)

;; going to use C-s as save, and C-f as search
(global-unset-key (kbd "C-f"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-j"))

(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

;;helm
(global-set-key (kbd "C-c h") 'helm-projectile)


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

;;; Custom Minor Mode for nav
;;; TODO: read http://nullprogram.com/blog/2013/02/06/
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
    (,(kbd "C-f") . isearch-forward)
    (,(kbd "C-s") . save-buffer)
    (,(kbd "C-p") . projectile-find-files)
    (,(kbd "C-S-k") . kill-line)
    (,(kbd "C-/") . comment-region)
))

(define-key cider-mode-map (kbd "C-c C-e") 'cider-pprint-eval-last-sexp)

(NikkoNav 1)

;;; This prevents NikkoNav from breaking auto complete in the minibuffer
(defun my-minibuffer-setup-hook ()
  (NikkoNav 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 2)
 '(ac-trigger-key "TAB")
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(global-auto-complete-mode t)
 '(global-rainbow-delimiters-mode t)
 '(helm-always-two-windows nil)
 '(helm-buffer-max-length 60)
 '(helm-split-window-in-side-p t)
 '(inhibit-startup-screen t)
 '(projectile-global-mode t)
 '(semantic-mode t)
 '(semantic-symref-auto-expand-results t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



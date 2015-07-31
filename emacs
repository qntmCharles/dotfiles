;; Package Management
(load "package")
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(package-initialize)

(setq required-packages 
	'(evil
	  evil-tabs
	  evil-leader
	  helm
	  helm-core
	  helm-projectile
	  projectile
	  key-chord
	  linum-relative
	  color-theme-solarized
	  flycheck
	  helm-flycheck
	  ycmd))

(unless package-archive-contents
(package-refresh-contents))

(dolist (package required-packages)
	(unless (package-installed-p package)
		(package-install package)))

(menu-bar-mode 0)

;;; Configuring Evil mode
(evil-mode 1)
(require 'evil)

(global-evil-tabs-mode t)
(global-evil-leader-mode)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
	"h" 'helm-flycheck)

;;; Helm config
(helm-mode 1)
(require 'helm-config)
(require 'helm-misc)
(require 'helm-locate)
(require 'helm-flycheck)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)

;(global-set-key (kbd "M-x") 'helm-M-x)

;; Actual Editing
(setq tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Key Bindings
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

(define-key evil-normal-state-map " " 'helm-find-files)
(key-chord-define evil-insert-state-map "jj" [escape])

;; Linum
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(global-linum-mode t)

;; Misc
(setq scroll-step 1)


;; Theme and Appearance
(load-theme 'solarized t)

(require 'whitespace)

(setq whitespace-style
      '(face empty newline newline-mark tabs tab-mark lines-tail trailing))
      
(setq whitespace-display-mappings
      '(
	(newline-mark 10 [8629 10])
	(tab-mark 9 [9655 9] [92 9])))

(global-whitespace-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Autocomplete and Syntax Checking

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
	  	  (list (expand-file-name "~/.local/include/")))))
(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-clang-language-standard "c++14")))

(require 'ycmd)
(ycmd-setup)
(set-variable 'ycmd-global-config "/home/wjh/dotfiles/.ycm_extra_conf.py")
(add-hook 'c++-mode-hook 'ycmd-mode)

(set-variable 'ycmd-server-command
	      '("python2" "/home/wjh/dotfiles/ycmd/ycmd"))

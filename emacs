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
	  ycmd
	  company
	  company-ycmd
	  omnisharp
      smart-tabs-mode
	  clang-format))

(unless package-archive-contents
(package-refresh-contents))

(dolist (package required-packages)
	(unless (package-installed-p package)
		(package-install package)))

(menu-bar-mode 0)
(tool-bar-mode 0)

;;; Configuring Evil mode
(evil-mode 1)
(require 'evil)

(global-evil-tabs-mode t)
(global-evil-leader-mode)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
	"h" 'helm-flycheck
	"f" 'clang-format-buffer)

;;; Helm config
(helm-mode 1)
(require 'helm-config)
(require 'helm-misc)
(require 'helm-locate)
(require 'helm-flycheck)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)


;; Key Bindings
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

(define-key evil-normal-state-map (kbd "C-p") 'helm-find-files)
(key-chord-define evil-insert-state-map "jj" [escape])

(global-set-key (kbd "M-x") 'helm-M-x)
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

;; Autocomplete and Syntax Checking


(require 'ycmd)
(require 'company-ycmd)
(ycmd-setup)
(company-ycmd-setup)
(set-variable 'ycmd-global-config "/home/wjh/dotfiles/.ycm_extra_conf.py")
(add-hook 'c++-mode-hook 'ycmd-mode)

(set-variable 'ycmd-server-command
	      '("python2" "/home/wjh/dotfiles/ycmd/ycmd"))

(set-variable 'omnisharp-server-executable-path "/home/wjh/dotfiles/ycmd/third_party/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe")

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
		                             (list (expand-file-name "~/.local/include/")))))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'csharp-mode-hook 'omnisharp-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(setq default-tab-width 4)
(smart-tabs-insinuate 'c++)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "brightcyan")))))

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a toroidal space
(setq windmove-wrap-around t )

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(electric-pair-mode 1)

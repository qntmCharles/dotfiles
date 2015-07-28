;; Package Management
(load "package")
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
 
(setq package-archive-enable-alist '(("melpa" deft magit)))

(package-initialize)

(setq required-packages 
	'(evil evil-tabs evil-leader helm helm-core helm-projectile projectile key-chord))

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

(evil-leader/set-leader "<space>")

;;; Helm config
(helm-mode 1)
(require 'helm-config)
(require 'helm-misc)
(require 'helm-locate)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)

(global-set-key (kbd "M-x") 'helm-M-x)

(defun helm-my-buffers ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer '(helm-c-source-buffers-list
			 helm-c-source-elscreen
			 helm-c-source-recentf
			 helm-c-source-locate)
		                            "*helm-my-buffers*")))

;;; Key Bindings
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

(define-key evil-normal-state-map " " 'helm-my-buffers)
(key-chord-define evil-insert-state-map "jj" [escape])

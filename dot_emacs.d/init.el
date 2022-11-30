;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; I have no idea what im doing.

;;; Code:
(setq user-full-name "Emre Cebi")

(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Clean folder
(setq make-backup-files nil)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq create-lockfiles nil)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

;; easy config open
(defun find-config ()
  "Edit init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c I") 'find-config)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit )

;;Themes and font
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font"Fira Code" :height 130)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		 eshell-mode-hook))
	 (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Elastic pairs
(electric-pair-mode 1)

;;Package repos
(when (eq system-type 'darwin)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(add-to-list 'load-path "~/.emacs.d/load-path")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Used packages
;; Basic
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Zen mode
(use-package zen-mode)
(require 'zen-mode)
(global-set-key (kbd "C-M-z") 'zen-mode)

;;ivy
(use-package ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

;;Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;all-the-icons
(use-package all-the-icons)

;;Code
;;Magit
(use-package magit
  :ensure t
  :bind ("C-c m s" . magit-status))

;;LSP mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Dap-mode
(use-package dap-mode)
(setq dap-auto-configure-features '(sessions locals controls tooltip))
;;C++
(require 'dap-cpptools)

;;Javascript
(require 'dap-firefox)

;; Auto-compleat framework
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("M-/" . company-complete)  
  :config
  (setq company-show-numbers            t
	company-minimum-prefix-length   1
	company-idle-delay              0.5
	company-backends
	'((company-files         
	   company-keywords      
	   company-capf          
	   company-yasnippet)
	  (company-abbrev company-dabbrev)))
  (global-company-mode))

;;Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list))
  
;;;ORG
(defun my-org-mode-hook ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(use-package org
  :hook
  (org-mode . visual-line-mode)
  (org-mode-hook . auto-revert-mod)
  (org-mode-hook . my-org-mode-hook)
  :config
  (setq calendar-week-start-day 1)
  (custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  '(org-document-title ((t (:inherit default :height 2.0)))))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Documents/ORG/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Documents/ORG/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))
  (setq org-image-actual-width nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-agenda-files '("~/Documents/ORG/gtd/inbox.org"
			   "~/Documents/ORG/gtd/gtd.org"
			   "~/Documents/ORG/gtd/tickler.org"))
  (setq org-refile-targets '(( "~/Documents/ORG/gtd/gtd.org" :maxlevel . 3)
                           ( "~/Documents/ORG/gtd/someday.org" :level . 1)
                           ( "~/Documents/ORG/gtd/tickler.org" :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-c l" 'org-store-link)
(global-set-key "\C-c a" 'org-agenda)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;;Hugo Blog file
(defun find-blog ()
  "Blogging"
  (interactive)
  (find-file "~/websites/imstudyinghere/content-org/org-posts.org"))

(global-set-key (kbd "C-c H") 'find-blog)

;; ORG-Roam
(use-package org-roam
  :after (org emacsql emacsql-sqlite)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/ORG/Notes")
  (org-roam-dailies-directory "Daily/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  :config
  (org-roam-db-autosync-mode)
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))

;; Org Drag and drop support
(use-package org-download)

;; org bullets
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;;ORG export
(use-package ox-html
  :ensure nil
  :defer 3
  :after org
  :custom
  (org-html-checkbox-type 'unicode))

(use-package ox-md
  :ensure nil
  :defer 3
  :after org)

(use-package ox-hugo
  :defer 3
  :after org
  :custom
  (org-hugo-use-code-for-kbd t))

;;; init.el ends here

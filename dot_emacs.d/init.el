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

(when (eq system-type 'darwin)
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (setq mac-right-option-modifier 'super)
  (setq python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/3.11/bin/python3"))

;; Clean folder
(setq make-backup-files nil)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq create-lockfiles nil)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

;; easy file open
(defun find-config ()
  "Edit init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun find-org-todo ()
  "TODO list"
  (interactive)
  (find-file "~/Documents/ORG/Agenda/todo.org"))

(defun find-org-agenda ()
  "Personal Agenda"
  (interactive)
  (find-file "~/Documents/ORG/Agenda/agenda.org"))

(defun find-blog ()
  "Blogging"
  (interactive)
  (find-file "~/websites/imstudyinghere/content-org/org-posts.org"))

(global-set-key (kbd "C-c I") 'find-config)
(global-set-key (kbd "C-c T") 'find-org-todo)
(global-set-key (kbd "C-c A") 'find-org-agenda)
(global-set-key (kbd "C-c H") 'find-blog)

;; Personal fixes
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit )

;;Themes and font
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font"FiraCode Nerd Font" :height 130)

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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

 (straight-use-package 'use-package)
 (setq straight-use-package-by-default t)

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(add-to-list 'load-path "~/.emacs.d/load-path")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Used packages
;; Basic
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package smex
  :init (smex-initialize))

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;; Company
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-backends
	'((company-files          ; files & directory
	   company-keywords       ; keywords
	   company-capf           ; what is this?
	   company-yasnippet)
	  (company-abbrev company-dabbrev)))
  (company-tng-configure-default))

;; Yassnipets
(use-package yasnippet)
(use-package yasnippet-snippets)

(yas-global-mode t) ;; activate yasnippet

;; Projectile
(use-package projectile
  :init (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Documents/Projects"))
  :bind
  ("s-p" . projectile-command-map))

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
(global-set-key (kbd "C-c t") 'counsel-org-tag)

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

;;Doom Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;;all-the-icons
(use-package all-the-icons)

;;Code
;;Magit
(use-package magit
  :bind ("C-c m s" . magit-status))

;;Flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list))

(use-package flycheck-irony
  :after flycheck
  :hook
  (flycheck-mode-hook . flycheck-irony-setup))

(use-package flycheck-cask
  :after flycheck
  :hook
  (flycheck-mode-hook . flycheck-cask-setup))

;; eglot
(defun dd/projectile-proj-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package eglot)

(defun dd/cpp-eglot-enable ()
  "enable variables and hooks for eglot cpp IDE"
  (interactive)
  (setq company-backends
        (cons 'company-capf
              (remove 'company-capf company-backends)))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions
                 'dd/projectile-proj-find-function))
  (add-to-list 'eglot-server-programs
               `((c++-mode) ,ddavis-clangd-exe))
  (add-hook 'c++-mode-hook 'eglot-ensure))

(defun my-project-try-tsconfig-json (dir)
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(add-hook 'project-find-functions
          'my-project-try-tsconfig-json nil nil)

(add-to-list 'eglot-server-programs
             '((typescript-mode) "typescript-language-server" "--stdio"))


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
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  '(org-document-title ((t (:inherit default :height 2.0)))))
  (setq org-agenda-files
	'("~/Documents/ORG/Agenda/agenda.org"
	  "~/Documents/ORG/Agenda/todo.org"
	  "~/Documents/ORG/Agenda/habits.org"))
  (setq org-capture-templates '(("p" "Planer")
				("pt" "Tasks" entry (file "~/Documents/ORG/Agenda/todo.org")
                                 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
			        ("pa" "Agenda" entry (file+olp "~/Documents/ORG/Agenda/agenda.org" "Not Started")
                                 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))
  (setq org-refile-targets
    '(("~/Documents/ORG/Agenda/Archive/archive.org" :maxlevel . 1)
      ("agenda.org" :maxlevel . 1)
      ("todo.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-image-actual-width nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-tag-alist (quote ((:startgroup)
                            ("@city" . ?e)
                            ("@campus" . ?o)
                            ("@home" . ?H)
                            (:endgroup)
                            ("uni" . ?w)
                            ("personal" . ?h)
                            ("org" . ?P)
                            ("holiday" . ?W)
                            ("programming" . ?F)
                            ("sib" . ?O)
                            ("friends" . ?N)
                            ("blog" . ?E))))
  (setq org-todo-keywords '((sequence "TODO(t)" "PLANING(p)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "SKIPED(s)")))
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((tags-todo "*"
                     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                      (org-agenda-block-separator nil)
                      (org-agenda-overriding-header "Important tasks without a date\n")))
	    (agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "+personal" ((org-agenda-overriding-header "Personal Tasks")))
	    (tags-todo "+uni" ((org-agenda-overriding-header "University Tasks")))))
	  ("P" "Personal Tasks" tags-todo "+personal")
	  ("U" "University Tasks" tags-todo "+uni")
	  ("H" "Homeworks" tags-todo "+homework")
	  ("S" "Shopping" tags-todo "+shoping"))))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-c l" 'org-store-link)
(global-set-key "\C-c a" 'org-agenda)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c r") #'org-refile)
(global-set-key (kbd "C-c s") #'org-schedule)

;; ORG-Roam
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/ORG/Notes")
  :config
  (org-roam-db-autosync-mode)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

;; Org Drag and drop support
(use-package org-download)

;; org bullets
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package ox-html
  :straight nil
  :defer 3
  :after org
  :custom
  (org-html-checkbox-type 'unicode))

(require 'ox-html)
(require 'ox-md)
(require 'ox-latex)
(straight-use-package 'ox-hugo)

;;; init.el ends here

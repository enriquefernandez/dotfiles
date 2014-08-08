;;; Packages:

(setq efg-packages
      '(
		ac-slime
		auto-complete
		ac-cider
		flx-ido
		projectile
		clojure-mode
		clojure-test-mode
		flycheck
		magit
		markdown-mode
		org
		solarized-theme
		autopair
		yaml-mode
		jedi
		ox-reveal
		htmlize
		ein
		multiple-cursors
		))


;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Mac meta keys
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; emacs setup file

(print "Loading enrique's .init.el emacs configuration file.")

(eval-when-compile (require 'cl))

(add-to-list 'load-path "~/.emacs.d/")

;; Setup packages
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))




(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg efg-packages)
  (when (and (not (package-installed-p pkg))
           (assoc pkg package-archive-contents))
    (package-install pkg)))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `efg-packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x efg-packages))
                            (not (package-built-in-p x))
                            (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))


;; Parenthesis highlighting
(require 'paren)
(show-paren-mode 1)

;; Ido
(require 'flx-ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess) 
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;; Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)


;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Windmove
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c j")    'windmove-up)
(global-set-key (kbd "C-c k")  'windmove-down)

;; Start to org mode directly
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; No toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Tabs
(setq tab-width 2
      indent-tabs-mode nil)

;; Autopair parenthesis
(require 'autopair)

;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; Magit GIT
(global-set-key (kbd "C-x g") 'magit-status)

;; org-reveal
(setq org-reveal-root (format "file://%s" (expand-file-name "~/dotfiles/reveal.js")))


;; Load MERS mars-toolkit setup file
(load "mars-setup")



;; Language hooks

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
;;(setq markdown-css-path (expand-file-name "markdown.css" abedra/vendor-dir))


;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Slime autocomplete
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; ac-cider
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

;; Python jedi
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Themes
(if window-system
    (load-theme 'solarized-dark t)
  (load-theme 'wombat t))


;; Start server
(server-start)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ecb-auto-activate t)
 '(ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*Apropos*") ("*Occur*") ("*shell*") ("\\*[cC]ompilation.*\\*" . t) ("\\*i?grep.*\\*" . t) ("*JDEE Compile Server*") ("*Help*") ("*Completions*") ("*Backtrace*") ("*Compile-log*") ("*bsh*") ("*Messages*") ("*slime-repl allegro*") ("[*]sldb allegro/[0-9]*[*]" . t) ("*slime-repl sbcl*") ("[*]sldb sbcl/[0-9]*[*]" . t) ("*slime-repl armedbear*") ("[*]sldb armedbear/[0-9]*[*]" . t))))
 '(ecb-compile-window-height 20)
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-grep-recursive-function (quote grep-find))
 '(ecb-layout-name "left15")
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-file-regexps (quote ((".*" ("") ("\\(\\(M\\|m\\)akefile\\|.*\\.\\(java\\|el\\|c\\|cc\\|h\\|hh\\|txt\\|html\\|texi\\|info\\|bnf\\|lisp\\|asd\\)\\)$")))))
 '(ecb-source-path (quote ("/Users/efernan/Dropbox/MIT/MERS/mars-toolkit" "" ("/Users/efernan/Dropbox/MIT/MERS/Projects/LearningLisp" "/LearningLisp"))))
 '(ecb-tip-of-the-day nil)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

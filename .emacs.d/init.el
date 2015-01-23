;;; Packages:

(setq efg-packages
      '(
        slime
        cpputils-cmake
        ac-slime
        auto-complete
        auto-complete-clang
        auto-complete-c-headers
        ac-cider
        ;; flx-ido
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
        yasnippet
        auctex
        auctex-latexmk
        exec-path-from-shell
        helm
        cmake-mode
        helm-gtags
        helm-projectile
        ag
        window-numbering
        ledger-mode
        ))


;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Mac meta keys
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; emacs setup file

(print "Loading enrique's .init.el emacs configuration file.")

(eval-when-compile (require 'cl))

;; (add-to-list 'load-path "~/.emacs.d/")

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


;; Yasnippet
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)


;; Parenthesis highlighting
(require 'paren)
(show-paren-mode 1)

;; Ido
;; (require 'flx-ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess) 
;; (flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Helm
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

;; Helm gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Projectile
(projectile-global-mode)
;; (setq projectile-enable-caching t)


;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; (global-set-key (kbd "C-c h")  'windmove-left)
;; (global-set-key (kbd "C-c l") 'windmove-right)
;; (global-set-key (kbd "C-c j")    'windmove-up)
;; (global-set-key (kbd "C-c k")  'windmove-down)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq windmove-wrap-around t)

;; Winner mode auto on
(winner-mode 1)

;; Window numbering
(require 'window-numbering)
;; highlight the window number in pink color
(custom-set-faces '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
(window-numbering-mode 1)

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

;; c indentation
(setq c-default-style "linux"
          c-basic-offset 4)

;;  cpp-utils-cmake
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))



;; ;; OPTIONAL, avoid typing full path when starting gdb
;; (global-set-key (kbd "C-c C-g")
;;  '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
;; OPTIONAL, some users need specify extra flags forwarded to compiler
;; (setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))

;; Auto-complete
(require 'auto-complete-config)
(require 'auto-complete-clang)
(ac-config-default)

;; (setq ac-auto-start nil)
;; (setq ac-quick-help-delay 0.5)
;; ;; (ac-set-trigger-key "TAB")
;; ;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
;; (defun my-ac-config ()
;;   (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;   (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;   ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;;   (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))
;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
;; (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ;; ac-source-gtags
;; (my-ac-config)


;; Magit GIT
(global-set-key (kbd "C-x g") 'magit-status)

;; org-reveal
(setq org-reveal-root (format "file://%s" (expand-file-name "~/dotfiles/reveal.js")))


;; Comment or uncomment
(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(global-set-key (kbd "C-;") 'comment-or-uncomment-current-line-or-region)


;; ORG MODE ;;;;;
;;;;;;;;;;;;;;;;;

;; org mode settings
;; from http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-45
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
(global-set-key (kbd "C-c O") 'org-open-at-point-global)
(global-set-key (kbd "<f9> <f9>") 'org-agenda-list)
(global-set-key (kbd "<f9> <f8>") (lambda () (interactive) (org-capture nil "r")))
;; (global-set-key (kbd "C-TAB") 'org-cycle org-mode-map)
;; (global-set-key (kbd "C-c v") 'org-show-todo-tree org-mode-map)
;; (global-set-key (kbd "C-c C-r") 'org-refile org-mode-map)
;; (global-set-key (kbd "C-c R") 'org-reveal org-mode-map)

(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
; (global-set-key (kbd "M-o") 'imenu)
(global-set-key (kbd "C-c j") 'org-clock-goto) ;; jump to current task from anywhere
(global-set-key (kbd "C-c C-w") 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/organizer.org")


(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)



(setq org-clock-idle-time nil)
(setq org-log-done 'time)
(setq org-clock-persist t)
(setq org-clock-report-include-clocking-task t)

(org-clock-persistence-insinuate)


;; (setq org-agenda-files
;;       (delq nil
;;             (mapcar (lambda (x) (and (file-exists-p x) x))
;;                     '("~/Dropbox/org/organizer.org"))))
(setq org-agenda-files '("~/Dropbox/org/"))


(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-blank-before-new-entry nil)


;; Load MERS mars-toolkit setup file
;; (load "mars-setup")
(defvar mtk-manager-emacs-path "~/dotfiles/.emacs.d/emacs-ide"
  "Set this to the folder containing the Emacs IDE repository.")
(setq slime-lisp-implementations
      `((allegro ,(if (string= system-type "windows-nt")
                      ;; We're on a Windows system. In order to use Allegro, we
                      ;; need a helper program. Include the full path to
                      ;; alisp.exe if it is not on your path.
                      (list (concat mtk-manager-emacs-path "/start-swank.bat") "alisp.exe")
                      ;; We're on GNU/Linux or OSX. No helper needed.
                      '("alisp")))
        (sbcl ("sbcl"))))

;; Set this to one of the implementations defined above to define your default
;; Lisp implementation.
(setq slime-default-lisp 'sbcl)
;; (setq slime-default-lisp 'allegro)

;; Add the folder containing the MERS Emacs libraries to the load-path.
(add-to-list 'load-path mtk-manager-emacs-path)

;; Load the MERS library. The 'mers-simple library tries to give you a
;; configuration that "just works." If you want to pick and choose pieces of the
;; MERS libraries, include them separately. But most people will not wnat to do
;; that.
(require 'mers-lisp)


;; Language hooks

;; .h in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
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


;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
 
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
 
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


;; Fix PATH on mac osx
;; so that it's the same as if executed from the terminal
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Stop extremely annoying beep in osx
(setq ring-bell-function #'ignore)
;; (setq visible-bell t)


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

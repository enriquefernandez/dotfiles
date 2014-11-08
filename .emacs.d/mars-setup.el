
;;;             ***  Customizations to Emacs  ***

;;;  Scribe: Brian Williams, Paul Elliot, Patrick Conrad, David Wang


;;; The following customizations provide hooks to Lisp (I) and to C++ (II).

(print "Loading MERS setup file ...")
(eval-when-compile (require 'cl))

;;; 0) Select the features you want                    ;;CUSTOMIZE
(let ((use-std-lisp nil)         ;(I)
      (use-cpp nil)              ;(II)

      ;; The following features will automatically be loaded if the library is found.
      ;; If 't', the feature will start with emacs.
      ;;
      ;; These features can be enabled and disabled interactively, by typing:
      ;;   "M-x featurename" and "M-x kill-featurename", respectively
      ;;   For example, "M-x paredit" and "M-x kill-paredit"
      ;; There are also convenience functions to enable and disable all 4 features:
      ;;   "M-x slime-editor" and "M-x kill-slime-editor"

      (use-paredit nil)            ;(III), parenthesis auto-completition and maintenance
      (use-tabbar t)             ;(IV), displays buffers by using tabs
      (use-ecb t)                ;(V), creates an IDE-style layout
      (use-slime t)              ;(VI), enables lisp through the standard method, (setq inferior-lisp-program ...)

      (use-win-allegro nil)      ;(VI), allows slime to interface with allegro on windows.
      (use-smooth-scrolling nil) ;(VII), enables smooth scrolling
	  
      ;; CUSTOMIZE THE FOLLOWING PATHNAMES
      ;; REQUIRED:
      (mars-toolkit-pathname "/Users/efernan/MERS/Mars toolkit/")
      ;; REQUIRED:
      (lisp-executable-filename "/Applications/AllegroCL64.app/Contents/Resources/alisp") ;;"/opt/acl82.64/alisp" "/usr/bin/sbcl" "abcl"
      ;; REQUIRED IF USING STD LISP:
      (lisp-init-filename "/Applications/AllegroCL64.app/Contents/Resources/eli/fi-site-init.el")
      ;; REQUIRED IF USING PAREDIT:
      ;; place the "paredit.el" file in your "site-lisp" folder, OR:
      (paredit-pathname "~/.emacs.d/site-lisp/")
      ;; REQUIRED IF USING TABBAR:
      ;; place the "tabbar.el" file in your "site-lisp" folder, OR:
      (tabbar-pathname "~/.emacs.d/site-lisp/")
      ;; REQUIRED IF USING EMACS CODE BROWSER:
      ;; place the "cedet" folder in your "site-lisp" folder, OR:
      (cedet-pathname "/Applications/Emacs.app/Contents/Resources/lisp/cedet/")
      ;; REQUIRED IF USING EMACS CODE BROWSER:
      ;; place the "ecb" folder in your "site-lisp" folder, OR:
      (ecb-pathname "~/.emacs.d/site-lisp/ecb-2.40/")
      ;; REQUIRED IF USING SLIME:
      ;; place the "slime" folder in your "site-lisp" folder, OR:
      (slime-pathname "~/.emacs.d/site-lisp/slime/")
      ;; REQUIRED IF USING SLIME ON WINDOWS:
      ;; Note: This path is interpreted by Allegro. If you do use the tilde (~),
      ;;        it will be what Allegro calls your HOME directory.
      (slime-config-filename "C:/Users/David Wang/.slime.lisp")

      
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I) Customizations to support the Emacs-Lisp interface (A - D):

  (when use-std-lisp 
;;; C') See the description at the end of C), below.  Suppresses a
;;;     message about setting up "M - .".
    (setq fi:find-tag-lock nil)

;;; A) Load the emacs to lisp interface.
    ;;(load lisp-init-filename) ;; Only load if in mars-editor mode.

;;; B) For the (M-x) command fi:commonlisp, setup the default arguments to:
;;;    o my working directory for lisp (the Mars Toolkit).
;;;    o the version of allegro commonlisp that is
;;;       case insensitive, and
;;;       does not includes the IDE (Integrated Development Environment).

    (setq 
     fi:common-lisp-directory mars-toolkit-pathname
     fi:common-lisp-image-name lisp-executable-filename)

;;; C) Modify M-. and M-, so that they perform edit lisp definition 
;;;    and edit next list definition.

;;; The standard keybinding for ``M-.'' invokes the Emacs tags file
;;; facility.  In all `fi' Lisp buffer modes there is an alternate
;;; mechanism for visiting the source for a definition based on source-file
;;; information obtained from the Lisp environment itself: ``C-c .''
;;; is bound to fi:lisp-find-definition and ``C-c ,'' is bound to
;;; fi:lisp-find-next-definition.  By default, ``M-.'' and ``M-,'' are
;;; unchanged from default Emacs and are bound to find-tag and
;;; tags-loop-continue.

;;; If you want ``M- .'' to find Lisp definitions in Lisp-moded buffers, then
;;;``C-c .'' and ``M- .'' can be switched in Lisp-moded buffers.  Put the
;;; following form in your ~/.emacs file:
    

    (setq fi:lisp-mode-hook
    (function
     (lambda ()
       (let ((map (current-local-map)))
         (define-key map "\C-c."  'find-tag)
         (define-key map "\C-c,"  'tags-loop-continue)
         (define-key map "\e."  'fi:lisp-find-definition)
         (define-key map "\e,"  'fi:lisp-find-next-definition))))))

;;; To prevent this message from appearing when find-tag or 
;;; find-tag-other-window are invoked, put this form in your ~/.emacs 
;;; before the LOAD of "fi-site-init":

;;; (setq fi:find-tag-lock nil)

;;; D) note that once common-lisp is loaded, it will load the file
;;;    .clinit.cl (contained in the same directory as .emacs?).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; II) Customizations to support C++, added by Paul Elliot:
    
;;redefine the tab width to be shorter
    (custom-set-variables
     '(tab-width 4))

  (when use-cpp
;;; A) ???


;;; B) ???
    (global-font-lock-mode t)
    (setq frame-title-format "%b")

;;; C) Changes default indentation of { and } to be 0 so they don't indent
    (defun c-hook ()
      "Changes default indentation of { and } to be 0 so they don't indent"
      (c-set-offset 'substatement-open 0)
      ;; Change basic offset to match our tab-width
      (setq c-basic-offset tab-width)
      t)

    (add-hook 'c-mode-common-hook 'c-hook t)

;;; E) Makes .h files load c++-mode instead of c-mode by default
    (defun find-h-hook (fNode node)
      (if (eq node '())
    '()
  (if (eq node fNode)
      '("\\.h\\'" . c++-mode)
    node)))

    (defun app-h-hook (node)
      (find-h-hook (assoc "\\.h\\'" auto-mode-alist) node))

    (if (assoc "\\.h\\'" auto-mode-alist)
  (setq auto-mode-alist (mapcar 'app-h-hook auto-mode-alist))
      (setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; III) Use paredit for structurally editing lisp code

  (ignore-errors
    (when (boundp 'paredit-pathname) (add-to-list 'load-path paredit-pathname))
    (autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code."
      t)
    ;; function to launch paredit
    (defun paredit ()
      (interactive)
      (ignore-errors 
        (paredit-mode +1) 
        ;; Activate full paren highlighting
        (require 'paren)
        (show-paren-mode 1)))
    ;; function to kill paredit
    (defun kill-paredit ()
      (interactive)
      (paredit-mode 0))
    ;; init
    (when use-paredit
    ;;activate it
    (mapc (lambda (mode)
      (let ((hook (intern (concat (symbol-name mode)
                            "-mode-hook"))))
  (add-hook hook #'paredit)))
  '(emacs-lisp lisp inferior-lisp))
    )
  ) ; end tabbar ignore-errors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IV) Use tabbar to see graphical tabs for each buffer
;;; added by: David Wang

  (ignore-errors
    (when (boundp 'tabbar-pathname) (add-to-list 'load-path tabbar-pathname))
    (require 'tabbar)
    ;; function to launch tabbar
    (defun tabbar ()
      (interactive)
      (tabbar-mode +1))
    (defun kill-tabbar ()
      (interactive)
      (tabbar-mode 0))
    ;; init
    ;;(when use-tabbar 
      ;;(tabbar))
  ) ; end tabbar ignore-errors


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; V) Use CEDET and the Emacs Code Browser (ECB) for the IDE
;;; added by : David Wang

  (ignore-errors
    ;; Load CEDET, a package required by the Emacs Code Browser.
    (when (boundp 'cedet-pathname) (add-to-list 'load-path cedet-pathname))
    (require 'cedet)
    (when (boundp 'ecb-pathname) (add-to-list 'load-path ecb-pathname))
    ;(require 'ecb) ; loads and starts ecb
    (require 'ecb-autoloads) ; just loads ecb
    ;; function to launch ecb
    (print "Loading ECB")
    (defun ecb ()
      (interactive)
      (ignore-errors
        ; configure cedet
        (global-ede-mode 1) ; Enable the Project management system 
        (semantic-load-enable-code-helpers) ; Enable prototype help and smart completion 
        (global-srecode-minor-mode 1) ; Enable template insertion menu 
        ; configure and init ecb
        (ecb-activate)
    ; add hook to redraw ecb layout
    (add-hook 'window-size-change-functions #'ecb-redraw-layout-hook)
      ))
    ;; function to kill ecb
    (defun kill-ecb ()
      (interactive)
      (remove-hook 'window-size-change-functions #'ecb-redraw-layout-hook)
      (ecb-deactivate))
    ;; function to re-layout the ecb windows when the frame size changes.
    ; In emacs, a frame is what the operating system calls a window. An 
    ; emacs frame can be divided into multiple windows. 
    ; The window-size-change-functions hook to which this lambda is added
    ; calls this method whenever ANY window or frame is resized, so 
    ; we need to compare against previous frame sizes to safely ignore
    ; when the user resizes a window.
  (setq prev-frame-width 0)
  (setq prev-frame-height 0)
    (defun ecb-redraw-layout-hook (&optional frame)
    (when (or (not (equal prev-frame-width (frame-width)))
        (not (equal prev-frame-height (frame-height))))
    (setq prev-frame-width (frame-width))
    (setq prev-frame-height (frame-height))
    (let ((window-size-change-functions nil))
      (ignore-errors (ecb-redraw-layout))
      )))
    ;; init
    (when use-ecb
      ; ECB must be started using the hook: emacs-startup-hook, because:
      ; 1. ECB MUST find calls to "custom-set-variables" and "custom-set-faces".
      ; 2. These two calls MUST appear at the end of the .emacs init file,
      ;    because that is where ECB places them when preferences are modified
      ;    and this .emacs file is auto-generated.
      ; 3. Using this hook ensures ECB is initialized AFTER this .emacs file
      ;    has been completely loaded.
      (add-hook 'emacs-startup-hook #'ecb))
  ) ; end cedet and ecb ignore-errors
  


;   (add-to-list 'load-path cedet-pathname)
;   (require 'cedet)
;   (semantic-mode 1)
;   (global-ede-mode 1)
;   ; (global-srecode-minor-mode 1)
; ; (semantic-load-enable-code-helpers) ; Enable prototype help and smart completion 
;  (add-to-list 'load-path
;                      "/Users/efernan/.emacs.d/site-lisp/ecb-2.40")
;  ; (load-file "/Users/efernan/.emacs.d/site-lisp/ecb-2.40/ecb.el")
;  (require 'ecb-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VI) Setup for SLIME 
;;; added by: Patrick Conrad, taken from http://www.franz.com/emacs/slime.html

  (ignore-errors
    (when (boundp 'slime-pathname) (add-to-list 'load-path slime-pathname))
    (require 'slime-autoloads)
    (require 'slime)

	
    (defun setup-slime ()
	  (slime-setup '(slime-fancy slime-banner))
	  (global-set-key "\C-cs" 'slime-selector)
	  (setq slime-complete-symbol*-fancy t)
	  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol))

	
	(unless use-win-allegro
	  ;; Optionally, specify the Lisp program you are using. Default is "lisp"
	  ;; If the Allegro directory is not in your PATH environment variable
	  ;; this should be a fully qualified path.
	  ;; choose one of the below based on Express or non-Express usage
	  (setq inferior-lisp-program lisp-executable-filename) 
	  ;;(setq inferior-lisp-program "allegro-express") 
	  )

    ;; function to quit slime buffers and background processes
    (defun kill-slime ()
      (interactive)
      (when (slime-connected-p)
        (slime-quit-lisp)
        (slime-disconnect))
      (slime-kill-all-buffers))
    ;; init
    ;;(when use-slime
      ;;(slime))
	) ; end slime ignore-errors
  
  ; (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ; ;; Replace "sbcl" with the path to your implementation
  ; (setq inferior-lisp-program lisp-executable-filename)

; (add-hook ’slime-mode-hook
;   (lambda ()
;     (unless (slime-connected-p)
;     (save-excursion (slime)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VII), enables smooth scrolling
;;; added by : David Wang

  (ignore-errors
  (require 'smooth-scroll)
  (when use-smooth-scrolling
    (smooth-scroll-mode t)))


  ;; Enrique's mars editor
  (defun mars-editor ()
	(interactive)
	(print "Loading lisp init file...")
	;;(load lisp-init-filename)
	(ignore-errors (paredit))
	(ignore-errors (tabbar))
	(ignore-errors (ecb))
	(ignore-errors (setup-slime))
	(slime)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONVENIENCE FUNCTIONS

(defun slime-editor ()
  (interactive)
  (ignore-errors (paredit))
  (ignore-errors (tabbar))
  (ignore-errors (ecb))
  (ignore-errors (slime))
)

(defun kill-slime-editor ()
  (interactive)
  (ignore-errors (kill-paredit))
  (ignore-errors (kill-tabbar))
  (ignore-errors (kill-ecb))
  (ignore-errors (kill-slime))
)

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THINGS YOU PROBABLY WANT IN EMACS

(add-to-list 'auto-mode-alist '("\\.\\(cl\\|system\\|asd\\)\\'" . lisp-mode))
(set-scroll-bar-mode 'right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VII) Emacs customizations

;; This line of code prevents emacs safety prompts when opening lisp files.
;; From the GNU Emacs documentation:
;; "If a file-local variable could specify an arbitrary function or Lisp expression 
;;  that would be called later, visiting a file could take over your Emacs."
;; A file local variable is any variable set with setq.
;; Setting the following value to 'nil', means emacs will incorporate all 
;; local variables, without prompting. This is *probably* safe when opening 
;; trusted lisp code.
(setq enable-local-variables nil)


;; The following code is AUTO GENERATED whenever preferences in Emacs are saved. 
;; Emacs, as well as some add-on packages (like Emacs Code Browser), 
;; saves its preference variables here. Since the code is auto-generated,
;; these "custom-..." function calls MUST always be top-level-forms.
;; They should NOT be nested in any other expressions.
;; CHANGE WITH CAUTION!

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(custom-enabled-themes (quote (wombat)))
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

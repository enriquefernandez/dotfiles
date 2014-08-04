;;A function used to start a swank server for SLIME when a new alisp is opened.
;;By Patrick Conrad, taken from http://www.franz.com/emacs/slime.lhtml
;;Change the directory to point to your slime.

;; Addendum by David Wang, this file will be evaluated by Allegro.
;; Therefore tilde (~) refers to what Allegro calls the home directory
;; To check what your home directory is, run allegro lisp (alisp.exe), 
;; and evaluate ":cd ~" (without the parenthesis) at the prompt.
;; Or, just use absolute paths.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(push "~/.emacs.d/site-lisp/site-lisp/slime/" asdf:*central-registry*) ;;CUSTOMIZE
(asdf:oos 'asdf:load-op :swank)

(swank-loader::init)

(sys:with-command-line-arguments (("p" :short port :required)
				  ("ef" :long ef :required))
  (restvar)
  (swank::create-server :port (parse-integer port :junk-allowed nil)
		        :style :spawn
	                :dont-close t
			:coding-system (or ef "latin-1")))

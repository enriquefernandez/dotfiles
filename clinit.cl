(in-package :CL-USER)

;;;             ***  Customizations to Common Lisp  ***

;;;  by Brian Williams, David Wang

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Option to globally set system to a higher debug level so that the local
;;; variable names get saved.
  (let ((good-debug t))
	(when good-debug
	  (proclaim '(optimize (speed 2) (safety 1) (space 1) (debug 3)))
	  (setq *load-local-names-info* t)))

;;; Define the cl-user::*mars-toolkit-pathname* to be the physical pathname 
;;;   to the root directory of the Mars toolkit (MTK).

  ;;   *** UPDATE THE BINDING OF THIS VARIABLE TO YOUR MTK ROOT DIRECTORY ***
  ;; NOTES: 
  ;;  o Make sure you include the trailing slash, i.e. ".../folder/"
  ;;    (not ".../folder")
  ;;  o SBCL does NOT resolve OS dependent symbols like "~" (for the home directory)
  ;;    for SBCL you MUST specify the complete/absolute pathname.
  (defconstant *mars-toolkit-pathname*
	"/Users/efernan/Dropbox (MIT)/MIT/MERS/mars-toolkit/"    ;;CUSTOMIZE
	"Location of the Mars Toolkit top-level directory.")

;;; Load the Mars Toolkit system definitions.
  ;; (load (merge-pathnames "load-MARS-systems.lisp" *mars-toolkit-pathname*))

  (pushnew :cplex *features*)
  (pushnew :ipopt *features*)
  (pushnew :gurobi *features*)
  (pushnew :lpsolve *features*)

  (defun load-mtk (&optional (ws "primary"))
	"Load the mars toolkit and set the active workspace to be WS."
	(load "/Users/efernan/Dropbox (MIT)/MIT/MERS/mtk/manager/setup.lisp")
	(funcall (read-from-string "mtk:change-workspace") ws))

  (load-mtk)
  (print "Finished loading ~/clinit.cl.")
)

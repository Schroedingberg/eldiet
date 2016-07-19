;;; package --- Summary
;; Author: Aaron Rebmann
;; Keywords: Nutrition, Org-mode, Diet
;; Homepage: https://github.com/Schroedingberg/eldiet
;;; Commentary:
;;; A nutrition planner for Emacs, built upon org mode.
;;; Plan so far: Use the capture functionality to capture a meal you eat.
;;; Then for example a fuzzy search in a database (or an org table, whatever I will come up with)
;;; could be executed...
;;; Org dashboard could be tweaked to show how much kcal are left...
;;; In modeline, kcal could as well be shown


;;; Code:
(require 'org)
(require 'helm)
(require 'helm-easymenu)
(require 'org-capture)

(defcustom eldiet-database nil
  "The file that contains the nutrition data. This is an org file, containing the foods as items with nutritional values as properties. This might allow as well to reference a file with recipes to be able to query favourite recipes by a food."
  :group 'eldiet
  :type '(choice directory (repeat directory)))

(defcustom eldiet-nutrients '("PROTEIN" "FAT" "CARBOHYDRATES" "ENERGY")
  "Defines the data fields of the food database. The standard is macro nutrients (as fractions) and energy as kcal per 100 g. It may also be set to e.g. only one value of interest (Think of carbs for diabetics).")

(defcustom eldiet-cached-candidates nil
    "The a list of candidates obtained when the configured food
files were last parsed.")

(defvar eldiet-food-hash nil
  "The hash of the content of the configured files. If this hash has not changed since the file was last parsed, a cached version of the parsed file is used.")


(defun eldiet-add-food-to-database (food)
  "Takes a string and adds it to the database as org item."
  (org-add-props (concat "* " food) '(eldiet-nutrients)))

(defun eldiet-prompt-for-food ()
  "Prompts for a food and the nutrients that are specified in
  eldiet-nutrients. It is a wrapper around eldiet add food to database.")

(defun eldiet-parse-org-database (filePath)
  "Parse the entries listed in eldiet-database using get-string-from-file."
  (with-temp-buffer
    (insert-file-contents filePath)
    (org-element-parse-buffer)))


(setq helm-source-eldiet
      '((name . "FOOD")
        (candidates . eldiet-parse-org-database)
        (action . (lambda (candidate )
                    (helm-marked-candidates)))))


	  
(defun eldiet-select-food ()
  "The helm dialog you can select the food for a meal from."
  (interactive)
  (insert
   (mapconcat 'identity
   (helm :sources '(helm-source-eldiet) :buffer "*helm eldiet select food*")
   ","))
  (unless (f-file? file)
    (user-error "eldiet file %s could not be found." file))
)

;;; Experimental
(define-minor-mode eldiet-capture-mode
  "Minor mode for special key bindings in a capture buffer.
So far this is only a copy of org-capture mode, this needs to be customized."
  nil " Rem" org-capture-mode-map
  (org-set-local
   'header-line-format
   (substitute-command-keys
    "\\<org-capture-mode-map>Capture buffer.  Finish \\[org-capture-finalize], \
refile \\[org-capture-refile], abort \\[org-capture-kill].")))

(provide 'eldiet)
;;; eldiet.el ends here










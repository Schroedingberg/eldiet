;;; package --- Summary
;; Author: Aaron Rebmann
;; Keywords: Nutrition, Org-mode, Diet
;; Homepage: https://github.com/Schroedingberg/eldiet
;;; Commentary: A nutrition planner for Emacs, built upon org mode.
;;; Plan so far: Use the capture functionality to capture a meal you eat.
;;; Then for example a fuzzy search in a database (or an org table, whatever I will come up with)
;;; could be executed...
;;; Org dashboard could be tweaked to show how much kcal are left...


;;; Code:
(require 'org)
(require 'helm)
(require 'helm-easymenu)


(defcustom eldiet-database nil
  "The file that contains the nutrition data. This is an org
  file, containing the foods as items with nutritional values as
  properties. This might allow as well to reference a file with
  recipes to be able to query favourite recipes by a food."
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

(defun get-string-from-file (filePath nutrients)
  "Return filePath's file content tailored to obtain property values from the org based database."
  (with-temp-buffer
    (insert-file-contents filePath)
    (mapcar 'org-property-values nutrients)
    ;; The parser needs to be transformed to return alists
    (mapcar ')))


(defun eldiet-parse-org-database ()
  "Parse the entries listed in eldiet-database using get-string-from-file."
  (setq db-file (get-string-from-file eldiet-database eldiet-nutrients)))


(defun eldiet-init ()
  "Check that files specified by user exist."
  (mapc (lambda (file)
	  (unless (f-file? file)
	    (user-error "eldiet file %s could not be found." file)))
	(-flatten (list eldiet-database))))


(defun eldiet-select-food ()
  "The helm dialog you can select the food for a meal from."
  (interactive)
  (helm :name "Hello"
	:buffer "*test*")
  )


(provide 'eldiet)
;;; eldiet.el ends here










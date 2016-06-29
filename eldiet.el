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

(defcustom eldiet-nutrients '("Protein" "Fat" "Carbohydrates" "Energy")
  "Defines the data fields of the food database. The standard is macro nutrients (as fractions) and energy as kcal per 100 g. It may also be set to e.g. only one value of interest (Think of carbs for diabetics).")

(defcustom eldiet-cached-candidates nil
    "The a list of candidates obtained when the configured food
files were last parsed.")

(defvar eldiet-food-hash nil
  "The hash of the content of the configured files. If this hash has not changed since the file was last parsed, a cached version of the parsed file is used.")


(defun eldiet-add-food-to-database ()
  "Prompts for a food and the nutrients that are specified in eldiet-nutrients.")


(defun eldiet-parse-org-database ()
  "Parse the entries listed in eldiet-database."
  )

(defun eldiet-init ()
  "Check that files specified by user exist."
  (mapc (lambda (file)
	  (unless (f-file? file)
	    (user-error "eldiet file %s could not be found." file)))
	(-flatten (list eldiet-database))))


(defun eldiet-select-food ()
  "Reads the food-data-file and returns a list of conses, one for each entry."
  (with-temp-buffer
    (mapc #'insert-file-contents
	  (-flatten (list eldiet-database)))
    (let ((food-hash (secure-hash 'sha256 (current-buffer)))))
    (unless (and eldiet-cached-candidates
		 (string= food-hash eldiet-food-hash)))
    (message "Loading food")
    ;; This is very under construction.
    ;; It is made in the style of helm-bibtex-candidates
    (let* ((entries (eldiet-parse-org-database))))
    ))



(provide 'eldiet)
;;; eldiet.el ends here


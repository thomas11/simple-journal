;;; simple-journal.el --- Keep a brief daily journal from Emacs.

;; Author: Thomas Kappler <tkappler@gmail.com>
;; Created: 2009 November 07
;; Keywords: journal, diary
;; URL: <http://github.com/thomas11/simple-journal/tree/master>

;; Copyright (C) 2009 Thomas Kappler

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; simple-journal.el is a very simple package for writing a daily
;; journal. It is not a mode. The format of the journal is fixed; it
;; keeps all entries in one file (until the user manually points
;; simple-journal to a new file) and is therefore better suited to
;; journals with many short entries than with fewer long entries. Here
;; is an excerpt of a journal produced with simple-journal:

;; 2009/Oct/07
;; 10:42 - About to read "In pursuit of desktop evolution" by Ravasio et al - ~/Downloads/10.1.1.96.398.pdf 
;; 12:56 - Finished reading and taking notes in ~/Documents/Gnome/design/articles.txt
;; + check why my subscription to opensuse-wiki didn't work
;; + Add autotools to gnome-activity-journal

;; This excerpt is originally from a blog post by Federico Mena
;; Quintero:
;; <http://www.gnome.org/~federico/news-2009-10.html#zeitgeist-vision-1>. This
;; post inspired me to write simple-journal, as I liked the journal
;; format but thought I'd need to make adding entries as easy as
;; possible to keep me journalling. Thanks, Federico!


;;; Dependencies: none.

;;; Installation:
;; Customize the variable at the top of the code section to point it
;; to your journal file, and (require 'simple-journal) in your init
;; file.

;;; History:
;; 2009-11:    First release.

;;; TODO

;; - new-item (the + thingies)

;;; Code:
(defun sj-new-log (right-here-p)
  (interactive "P")
  (unless right-here-p
    (sj-move-to-new-entry-position))
  (insert (format-time-string "%H:%M") " - "))

(defun sj-move-to-new-entry-position ()
  "If we can find a previous entry, start a new line after it and go there.
If we cannot, start the new entry at point if we're on an empty
line, on a new line after point otherwise."
  (let ((last (sj-find-last-entry)))
    (if last (goto-char last))
    (when (or last (sj-cur-line-not-empty-p))
      (end-of-line)
      (newline))))

(defun sj-cur-line-not-empty-p ()
  (beginning-of-line)
  (looking-at-p ".*[:word:]"))

(defun sj-find-last-entry ()
  (save-excursion
    (goto-char (point-max))
    (re-search-backward "^[0-9][0-9]:[0-9][0-9] - .*" nil t)))

(defun sj-new-log-here ()
  (interactive)
  (insert (format-time-string "%H:%M") " - "))


(provide 'simple-journal)
;;; simple-journal.el ends here

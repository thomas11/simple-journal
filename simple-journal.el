;;; simple-journal.el --- Keep a brief daily journal.

;; Author: Thomas Kappler <tkappler@gmail.com>
;; Created: 2009 November 07
;; Keywords: journal, diary, calendar
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

;;     2009/Oct/07
;;     10:42 - About to read "In pursuit of desktop evolution" by Ravasio et al - ~/Downloads/10.1.1.96.398.pdf 
;;     12:56 - Finished reading and taking notes in ~/Documents/Gnome/design/articles.txt
;;     + check why my subscription to opensuse-wiki didn't work
;;     + Add autotools to gnome-activity-journal

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

;;; Code:
(defconst sj-journal-file "~/Writing/journal.text"
  "Your journal file.")

(defun sj-new-item (right-here-p &rest items)
  (sj-visit-journal)
  (unless right-here-p
    (sj-move-to-new-entry-position))
  (apply 'insert items))

(defun sj-new-entry (right-here-p)
  (interactive "P")
  (sj-new-item right-here-p
               "- **"
               (format-time-string "%H:%M")
               "** - "))

(defun sj-move-to-new-entry-position ()
  "Move point to a suitable position for starting a new entry.
If we can find a previous entry, start a new line after it and go
there.  Otherwise, start the new entry on a new line after point.
In addition, start a new day if the last day stamp is not today."
  (let* ((last     (sj-find-last-entry))
         (last-day (sj-last-date last))
         (today    (sj-today-str)))
    (when last (goto-char last))
    (sj-move-past-current-entry)
    (when (not (string= today last-day))
      (insert "\n" "### " today "\n\n"))))

(defun sj-today-str ()
  (format-time-string "%Y-%m-%d"))

(defun sj-move-past-current-entry ()
  "Does one (forward-line 1), followed by more as long as we're
looking at indented lines (in case the current entry is on
multiple lines)."
  (interactive)
  (while (progn
           (forward-line 1)
           (looking-at "  ")))
  ; If we're on an empty line, we need one more; if not, two.
  (if (looking-at "^$")
    (progn (open-line 1) (forward-char 1))
    (open-line 2) (forward-char 2)))

(defun sj-find-last-entry ()
  (save-excursion
    (goto-char (point-max))
    (re-search-backward "^[-*] \\*\\*[0-9][0-9]:[0-9][0-9]\\*\\* - " nil t)))

(defun sj-last-date (&optional start-here-backwards)
  (save-excursion
    (goto-char (if start-here-backwards
                   start-here-backwards
                 (point-max)))
    (if (re-search-backward "^[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]" nil t)
        (match-string-no-properties 0)
      nil)))

(defun sj-visit-journal ()
  (let ((buffer (find-buffer-visiting sj-journal-file)))
    (if buffer
        (pop-to-buffer buffer)
      (find-file sj-journal-file))))


;; Unit tests, using el-expectations by rubikitch,
;; <http://www.emacswiki.org/emacs/EmacsLispExpectations>.
;; -------------------------------------------------------
(eval-when-compile
  (when (fboundp 'expectations)
    (expectations

      (desc "Finding stuff")
      (expect nil
        (with-temp-buffer
          (sj-find-last-entry)))
      (expect 12
        (with-temp-buffer
          (insert (sj-today-str) "\n- **22:22** - foo")
          (sj-find-last-entry)))
      (expect nil
        (with-temp-buffer
          (insert "bla")
          (sj-last-date)))
      (expect (sj-today-str)
        (with-temp-buffer
          (insert (sj-today-str))
          (sj-last-date)))

      (desc "Move past current entry")
      (expect 29
        (with-temp-buffer
          (insert (sj-today-str) "\n" "- **11:11** - bla")
          (sj-move-past-current-entry)
          (point)))

      (desc "position for new entry")
      (expect 13 ; Current day should be inserted automatically.
        (with-temp-buffer
          (sj-move-to-new-entry-position)
          (point)))
      (expect 29
        (with-temp-buffer
          (insert (sj-today-str) "\n" "- **11:11** - bla")
          (sj-move-to-new-entry-position)
          (point)))
      (expect 11
        (with-temp-buffer
          (insert (sj-today-str))
          (goto-char (point-min))
          (sj-move-to-new-entry-position)
          (point)))

      (desc "last date")
      (expect "2009-11-30"
        (with-temp-buffer
          (insert "2009-11-30 bla" "\n\n")
          (sj-last-date))))))

(provide 'simple-journal)
;;; simple-journal.el ends here

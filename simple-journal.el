;;; simple-journal.el --- Keep a brief daily journal.

;; Copyright (C) 2009 Thomas Kappler

;; Author: Thomas Kappler <tkappler@gmail.com>
;; Created: 2009 November 07
;; Keywords: journal, diary, calendar
;; URL: <http://github.com/thomas11/simple-journal/tree/master>

;; This file is not part of GNU Emacs.

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

;;; TODO
;; - sj-journal to just show the journal
;; - sj-todo to narrow to TODO entries
;; - rewrite docs, mention FMQ briefly, pointer to markdown conf

;;; Code:
(defconst sj-journal-file "~/Writing/journal.text"
  "Your journal file.")

(defun sj-new-entry ()
  (interactive)
  (sj-new-item "- **"
               (format-time-string "%H:%M")
               "** - "))

(defun sj-new-item (&rest items)
  (sj-update-daystamp (sj-journal))
  (apply 'insert items))

(defun sj-journal ()
  (interactive)
  (sj-visit-journal)
  (sj-move-to-new-entry-position))

(defun sj-move-to-new-entry-position ()
  "Move point to a suitable position for starting a new entry.
If we can find a previous entry, start a new line after it and go
there.  Otherwise, start the new entry on a new line after point.
In addition, start a new day if the last day stamp is not today."
  (let* ((last     (sj-find-last-entry))
         (last-day (sj-last-date last)))
    (when last (goto-char last))
    (sj-move-past-current-entry)
    last-day))

(defun sj-update-daystamp (last-day)
  (let ((today    (sj-today-str)))
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
    (sj-re-backward "^[-*] \\*\\*[0-9][0-9]:[0-9][0-9]\\*\\* - ")))

(defun sj-last-date (&optional start-here-backwards)
  (save-excursion
    (goto-char (if start-here-backwards
                   start-here-backwards
                 (point-max)))
    (if (sj-re-backward "^### \\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\)")
        (match-string-no-properties 1)
      nil)))

(defun sj-visit-journal ()
  (let ((buffer (find-buffer-visiting sj-journal-file)))
    (if buffer
        (pop-to-buffer buffer)
      (find-file sj-journal-file))))

(defun sj-re-backward (re)
  (re-search-backward re
                      nil  ; search bound
                      t    ; return nil if not succesful, don't throw error
                      ))


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
          (insert "### " (sj-today-str))
          (sj-last-date)))

      (desc "Move past current entry")
      (expect 31
        (with-temp-buffer
          (insert (sj-today-str) "\n" "- **11:11** - bla")
          (sj-move-past-current-entry)
          (point)))

      (desc "position for new entry")
      (expect 2 ; just inserts newlines in an empty buffer
        (with-temp-buffer
          (sj-move-to-new-entry-position)
          (point)))
      (expect 31
        (with-temp-buffer
          (insert (sj-today-str) "\n" "- **11:11** - bla")
          (sj-move-to-new-entry-position)
          (point)))
      (expect 13
        (with-temp-buffer
          (insert (sj-today-str))
          (goto-char (point-min))
          (sj-move-to-new-entry-position)
          (point)))

      (desc "last date")
      (expect "2009-11-30"
        (with-temp-buffer
          (insert "### 2009-11-30 bla" "\n\n")
          (sj-last-date))))))

(provide 'simple-journal)
;;; simple-journal.el ends here

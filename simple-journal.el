;;; simple-journal.el --- Keep a daily journal in Markdown.

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

;; simple-journal.el is a very simple helper for writing a daily
;; journal. It is not a mode. You say what file your journal is in,
;; and it offers convenience methods to go there (sj-journal), to
;; write a new entry (sj-new-entry), and to show all lines containing
;; "TODO". sj-new-entry is smart enough to find your last entry and
;; start the new one directly below it, so you can have other texts
;; like notes or TODO items at the end of the file.

;; It is very simple, and is supposed to stay that way. The format of
;; the journal is fixed. It keeps all entries in one file, until you
;; point the sj-journal-file variable to a new file.

;; Here is an excerpt of a journal produced with simple-journal. You
;; can use all of Markdown in your text. Titles for entries are
;; optional.

;; > ### 2010-01-10
;; >  
;; > #### 18:15
;; >
;; > "XML serializations should be hidden away from
;; > human view lest small children accidentally see them and become
;; > frightened."---from the paper *Representing disjunction and
;; > quantifiers in RDF*, McDermottDou02.pdf.
;; >
;; >
;; > ### 2010-01-17
;; > 
;; > #### 14:45 Wicket & Netbeans
;; >
;; > Set up a minimal Wicket application with Netbeans (a
;; > first for me, version 6.8) and Jetty. I want to try out working
;; > asynchronously with JSON using Wicket. Here are the steps to get the
;; > application running, serving up an empty page: [...]

;; simple-journal was inspired by a blog post by Federico Mena
;; Quintero:
;; <http://www.gnome.org/~federico/news-2009-10.html#zeitgeist-vision-1>.
;; Earlier versions produced the journal format described in that
;; post. Thanks, Federico!

;;; Dependencies
;; None.

;;; Installation:
;; Customize the variable at the top of the code section to point it
;; to your journal file, and (require 'simple-journal) in your init
;; file.

;;; History:
;; 2009-11:    First release.
;; 2010-04:    sj-todos; visits the journal and does (occur "TODO")
;;             New format: Days are ### headings, entries ####.

;;; Code:
(defconst sj-journal-file "~/Dropbox/journal.text"
  "Your journal file.")

(defun sj-new-entry (title)
  (interactive "sTitle:")
  (sj-update-daystamp (sj-journal))
  (insert "\n#### " (format-time-string "%H:%M"))
  (when title (insert " " title))
  (insert "\n\n"))

(defun sj-journal ()
  (interactive)
  (sj-visit-journal)
  (sj-move-to-new-entry-position))

(defun sj-todos ()
  (interactive)
  (sj-visit-journal)
  (occur "TODO"))

(defun sj-move-to-new-entry-position ()
  "Move point to a suitable position for starting a new entry.
If we can find a previous entry, start a new line after it and go
there.  Otherwise, start the new entry on a new line after point.
In addition, start a new day if the last day stamp is not today."
  (let* ((last     (sj-find-last-entry))
         (last-day (sj-last-date last)))
    (when last (goto-char last))
    (sj-move-past-current-entry)
    (open-line 2)
    last-day))

(defun sj-update-daystamp (last-day)
  (let ((today (sj-today-str)))
    (when (not (string= today last-day))
      (insert "\n\n" "### " today "\n"))))

(defun sj-today-str ()
  (format-time-string "%Y-%m-%d"))

(defun sj-move-past-current-entry ()
  "Move forward until either a journal entry, in which case we
stop before it, or the end of the buffer."
  (forward-line 2)
  (while (and (> (forward-line) 1)
              (not (looking-at "^###")))))

(defun sj-find-last-entry ()
  (save-excursion
    (goto-char (point-max))
    (sj-re-backward "^#### [0-9][0-9]:[0-9][0-9]")))

(defun sj-last-date (&optional start-here-backwards)
  (save-excursion
    (goto-char (if start-here-backwards
                   start-here-backwards
                 (point-max)))
    (if (sj-re-backward "^### \\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\)")
        (match-string-no-properties 1)
      nil)))

(defun sj-visit-journal ()
  "Pop to the journal buffer or visit the file."
  (let ((buffer (find-buffer-visiting sj-journal-file)))
    (if buffer
        (pop-to-buffer buffer)
      (find-file sj-journal-file))))

(defun sj-re-backward (re)
  (re-search-backward re nil t))


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
          (insert (sj-today-str) "\n" "#### 22:22 foo")
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
      (expect 26
        (with-temp-buffer
          (insert (sj-today-str) "\n" "#### 11:11 bla")
          (sj-move-past-current-entry)
          (point)))

      (desc "position for new entry")
      (expect 1 ; just inserts a newline
        (with-temp-buffer
          (sj-move-to-new-entry-position)
          (point)))
      (expect 26
        (with-temp-buffer
          (insert (sj-today-str) "\n" "#### 11:11 bla")
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
          (insert "### 2009-11-30 bla" "\n\n")
          (sj-last-date))))))

(provide 'simple-journal)
;;; simple-journal.el ends here

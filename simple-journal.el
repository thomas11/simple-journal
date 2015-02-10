;;; simple-journal.el --- Keep a daily journal in Markdown.

;; Copyright (C) 2015 Thomas Kappler

;; Author: Thomas Kappler <tkappler@gmail.com>
;; Created: 2009 November 07
;; Keywords: journal, diary, calendar, markdown
;; URL: <http://github.com/thomas11/simple-journal>

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
;; journal. It is not a mode. You tell it what file your journal is
;; in, and it offers convenience methods to go there (sj-journal), to
;; write a new entry (sj-new-entry), and to show all lines containing
;; "TODO" (sj-todos). sj-new-entry is smart enough to find your last
;; entry and start the new one directly below it, so you can have
;; other texts like notes or TODO items at the end of the file.

;; It is very simple, and is supposed to stay that way. The format of
;; the journal is fixed. It keeps all entries in one file, until you
;; point the sj-journal-file variable to a new file.

;; Here is an example entry in a journal produced with
;; simple-journal. You can use all of Markdown in your text. Titles
;; for entries are optional.
;;
;; > ### 2015-02-10  Contracts for Java
;; > 
;; > Looking a bit further I found
;; > [C4J|https://github.com/C4J-Team/C4J]. My first impression is great,
;; > this is how I'd imagine it should be done (not being an expert in
;; > this space). Everything is pure Java.
;;
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
;; 2009-11:   First release.
;; 2010-04:   sj-todos; visits the journal and does (occur "TODO")
;;            New format: Days are ### headings, entries ####.
;; 2015-02:   New format, because why not: Days and entries are not
;;            separate anymore. Each entry just gets a timestamp.
;;            Also, each entry is simply inserted at the end of the
;;            file, this is more understandable and allowed deleting
;;            half of the code.
;;            Finally, there is now some configurability: the
;;            timestamp and the markdown heading for entries can be
;;            defined.

;;; Code:
(defconst sj-journal-file "~/Documents/log.text"
  "Your journal file.")
(defconst sj-date-format "%Y-%m-%d"
  "Format of date to add to each entry.")
(defconst sj-markdown-heading "###"
  "The Markdown heading to use for each entry, such as ###.")

;; TODO With prefix arg, insert it as content?
(defun sj-new-entry (title)
  (interactive "sTitle:")
  (sj-journal)
  (insert sj-markdown-heading " " (format-time-string sj-date-format))
  (when title (insert "  " title))
  (insert "\n\n"))

(defun sj-journal ()
  (interactive)
  (sj-visit-journal)
  (sj-move-to-new-entry-position))

(defun sj-todos ()
  (interactive)
  (sj-visit-journal)
  (occur "TODO"))

;; TODO Insert the right number of newlines
(defun sj-move-to-new-entry-position ()
  "Move point to a suitable position for starting a new entry.
Currently this is simply the end of the file."
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (insert "\n\n\n"))

(defun sj-visit-journal ()
  "Pop to the journal buffer or visit the file."
  (let ((buffer (find-buffer-visiting sj-journal-file)))
    (if buffer
        (pop-to-buffer buffer)
      (find-file sj-journal-file))))

(provide 'simple-journal)
;;; simple-journal.el ends here

# simple-journal.el --- Keep a brief daily journal.

Copyright (C) 2009 Thomas Kappler

* Author: Thomas Kappler <tkappler@gmail.com>
* Created: 2009 November 07
* Keywords: journal, diary, calendar
* URL: <http://github.com/thomas11/simple-journal/tree/master>

This file is not part of GNU Emacs.

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary

simple-journal.el is a very simple package for writing a daily
journal. It is not a mode. The format of the journal is fixed; it
keeps all entries in one file (until the user manually points
simple-journal to a new file) and is therefore better suited to
journals with many short entries than with fewer long entries. Here
is an excerpt of a journal produced with simple-journal:

    2009/Oct/07
    10:42 - About to read "In pursuit of desktop evolution" by Ravasio et al - ~/Downloads/10.1.1.96.398.pdf 
    12:56 - Finished reading and taking notes in ~/Documents/Gnome/design/articles.txt
    + check why my subscription to opensuse-wiki didn't work
    + Add autotools to gnome-activity-journal

This excerpt is originally from a blog post by Federico Mena
* Quintero:
<http://www.gnome.org/~federico/news-2009-10.html#zeitgeist-vision-1>. This
post inspired me to write simple-journal, as I liked the journal
format but thought I'd need to make adding entries as easy as
possible to keep me journalling. Thanks, Federico!


# Dependencies: none.

# Installation
Customize the variable at the top of the code section to point it
to your journal file, and (require 'simple-journal) in your init
file.

# History
* 2009-11:    First release.

# TODO
- sj-journal to just show the journal
- sj-todo to narrow to TODO entries
- rewrite docs, mention FMQ briefly, pointer to markdown conf



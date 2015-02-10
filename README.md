# simple-journal.el --- Keep a daily journal in Markdown.

Copyright (C) 2015 Thomas Kappler

* Author: Thomas Kappler <tkappler@gmail.com>
* Created: 2009 November 07
* Keywords: journal, diary, calendar, markdown
* URL: <http://github.com/thomas11/simple-journal>

This file is not part of GNU Emacs.

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary

simple-journal.el is a very simple helper for writing a daily
journal. It is not a mode. You tell it what file your journal is
in, and it offers convenience methods to go there (sj-journal), to
write a new entry (sj-new-entry), and to show all lines containing
"TODO" (sj-todos). sj-new-entry is smart enough to find your last
entry and start the new one directly below it, so you can have
other texts like notes or TODO items at the end of the file.

It is very simple, and is supposed to stay that way. The format of
the journal is fixed. It keeps all entries in one file, until you
point the sj-journal-file variable to a new file.

Here is an example entry in a journal produced with
simple-journal. You can use all of Markdown in your text. Titles
for entries are optional.

> ### 2015-02-10  Contracts for Java
> 
> Looking a bit further I found
> [C4J|https://github.com/C4J-Team/C4J]. My first impression is great,
> this is how I'd imagine it should be done (not being an expert in
> this space). Everything is pure Java.

simple-journal was inspired by a blog post by Federico Mena
* Quintero:
<http://www.gnome.org/~federico/news-2009-10.html#zeitgeist-vision-1>.
Earlier versions produced the journal format described in that
post. Thanks, Federico!

# Dependencies
None.

# Installation
Customize the variable at the top of the code section to point it
to your journal file, and (require 'simple-journal) in your init
file.

# History
* 2009-11:   First release.
* 2010-04:   sj-todos; visits the journal and does (occur "TODO")
           New format: Days are ### headings, entries ####.
* 2015-02:   New format, because why not: Days and entries are not
           separate anymore. Each entry just gets a timestamp.
           Also, each entry is simply inserted at the end of the
           file, this is more understandable and allowed deleting
           half of the code.
           Finally, there is now some configurability: the
           timestamp and the markdown heading for entries can be
           defined.



# simple-journal.el --- Keep a daily journal in Markdown.

Copyright (C) 2009 Thomas Kappler

* Author: Thomas Kappler <tkappler@gmail.com>
* Created: 2009 November 07
* Keywords: journal, diary, calendar
* URL: <http://github.com/thomas11/simple-journal/tree/master>

This file is not part of GNU Emacs.

Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary

simple-journal.el is a very simple helper for writing a daily
journal. It is not a mode. You say what file your journal is in,
and it offers convenience methods to go there (sj-journal), to
write a new entry (sj-new-entry), and to show all lines containing
"TODO". sj-new-entry is smart enough to find your last entry and
start the new one directly below it, so you can have other texts
like notes or TODO items at the end of the file.

It is very simple, and is supposed to stay that way. The format of
the journal is fixed. It keeps all entries in one file, until you
point the sj-journal-file variable to a new file.

Here is an excerpt of a journal produced with simple-journal. You
can use all of Markdown in your text. Titles for entries are
optional.

> ### 2010-01-10
>  
> #### 18:15
>
> "XML serializations should be hidden away from
> human view lest small children accidentally see them and become
> frightened."---from the paper *Representing disjunction and
> quantifiers in RDF*, McDermottDou02.pdf.
>
>
> ### 2010-01-17
> 
> #### 14:45 Wicket & Netbeans
>
> Set up a minimal Wicket application with Netbeans (a
> first for me, version 6.8) and Jetty. I want to try out working
> asynchronously with JSON using Wicket. Here are the steps to get the
> application running, serving up an empty page: [...]

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
* 2009-11:    First release.
* 2010-04:    sj-todos; visits the journal and does (occur "TODO")
            New format: Days are ### headings, entries ####.



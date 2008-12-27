* General

xmtn is an Emacs Lisp package that provides a DVC backend for monotone
(the distributed version control system) as well as general facilities
for interacting with monotone from Emacs Lisp.

For more information about monotone, see http://monotone.ca/ .

xmtn's facilities for interacting with monotone are meant to be
reusable by code that is unrelated to DVC, even though they currently
depend on the subprocess handling utilities that DVC provides.

xmtn should work on GNU Emacs 21 or newer.  Work on supporting XEmacs
has started but is unfinished; patches welcome.  On XEmacs, xmtn
requires MULE.


* Download and installation

Follow the download and installation instructions for DVC.  xmtn is
part of DVC.

In addition, the variable `xmtn-executable' needs to point to the
monotone executable.  It defaults to "mtn", which will be sufficient
if mtn is in your PATH.  Depending on your configuration, the PATH
that Emacs sees can differ from the PATH that you see in your shell.
Try M-x getenv RET PATH RET if in doubt.

You may wish to set `dvc-debug' to nil; DVC tends to be a bit chatty.



* Brief tutorial

(DVC's tutorial does not apply to xmtn, it seems to be specific to
tla.)

Start Emacs.  Visit a file that is under version control by monotone.
Modify the file.  While in the file's buffer, press C-x V d to see the
diff for this file.

Pressing C-x V = will bring up the tree diff buffer.  (What monotone
calls a "workspace" is called a "tree" in DVC.)  This buffer shows the
list of all modified files in the tree as well as the diffs for those
files.  Use j to jump back and forth between the name of a file in the
list and the diffs for that file.  Use RET with point inside a diff
hunk to go to the corresponding file at the corresponding position.

Like many other DVC buffers, the contents of the tree diff buffer can
be refreshed using g.

In the tree diff buffer, files to commit can be marked and unmarked
with m and u.  Pressing c lets you commit the selected files; it will
bring up a log edit buffer where you can enter a commit message.

In the log edit buffer, the commit can be executed by pressing C-c
C-c.  To abort the commit, simply don't press C-c C-c -- just switch
away from the buffer or kill it.  The log edit buffer edits the file
_MTN/log.

To bring up the log edit buffer without going through the tree diff
buffer, use C-x V c.

To view the revision history, use C-x V l or C-x V L.  The former
shows the full commit message for each revision, while the latter only
shows the first line.  The resulting buffer is a so-called revlist
buffer.  In revlist buffers, use cursor up/down to move between
revisions, RET to show details on the revision at point, = to show its
diff from its parent.  Revisions can be marked and unmarked with m and
u.

M-x xmtn-view-heads-revlist shows a revlist buffer with just the heads
of the default branch of your tree.  To update your tree to one of the
revisions in a revlist buffer, move point to it and use M-x
xmtn-revlist-update.  To merge two head revisions, mark them and use
M-x xmtn-revlist-explicit-merge.

M-x xmtn-view-revlist-for-selector prompts for a monotone selector and
shows a revlist buffer with all matching revisions.

C-x V u performs mtn update.  C-x V m shows a revlist buffer with the
revisions that mtn update would apply to your tree.

C-x V f a performs mtn add.  M-x dvc-ignore-files and M-x
dvc-ignore-file-extensions can be used to add entries to .mt-ignore.
These commands can also be used from dired buffers.

C-x V s shows the status buffer.  This currently shows modified,
renamed and unknown files.  It's supposed to allow operations like
diff, commit, revert etc. (like pcl-cvs), but that's not implemented
yet.  C-x V = is preferable at the moment, although it doesn't show
unknown files.

C-x V a can be used to add a ChangeLog entry to _MTN/log.

There are other useful operations, but these should be enough to get
started.



* Known limitations

xmtn currently just bails out when it needs to operate on a head of a
branch and notices that the branch is unmerged.  It should prompt the
user to select a head instead.  To update to a head of an unmerged
revision graph, use M-x xmtn-view-heads-revlist and M-x
xmtn-revlist-update.

`xmtn-dvc-diff' breaks when called in a workspace that has no base
revision (e.g. a newly created project).  mtn diff works in this case.

Building a revlist buffer is currently a bit slow (or maybe very slow
for long histories?), and the revlist display is not very pretty.

For `dvc-ignore-files' and `dvc-ignore-file-extensions', xmtn operates
on the file .mtn-ignore.  This may fail to have the intended effect if
the user has customized monotone's ignore_file hook in a way that
changes the meaning of this file.

The ability to perform operations such as diff and commit from the
status buffer is missing.  For now, use the tree diff buffer for this.

xmtn doesn't define any key bindings for monotone-specific commands.
Only the backend-independent key bindings defined by DVC are available.

For now, I don't see the point of checking automate interface_version:
Many of xmtn's operations rely on non-automate commands, so a
compatible automate interface_version doesn't guarantee actual
compatibility; we have to check for a compatible command version
anyway, and that check subsumes the check of interface_version.  And
declaring incompatibility whenever we see an automate
interface_version that is too high for us yields false positives too
easily to be useful.

xmtn currently uses mtn automate get_revision in places where it
should be using mtn automate inventory.  This is because I was trying
to avoid having to implement a parser for mtn automate inventory, and
get_revision seemed to return almost the same information.  However,
get_revision fails if there are missing files -- I discovered this too
late.  This is part of the reason why many operations first check
whether files are missing from the tree, and abort if this is the
case.

DVC REVISION-IDs that refer to the "Nth ancestor" such as `(xmtn
(last-revision ...))' or `(xmtn (previous-revision ...))' are
ill-defined for non-linear history in monotone.  xmtn currently
throws an error when it encounters a node that has multiple parents
while trying to resolve such IDs.

The support for international character sets/coding systems is partly
based on guesswork but works for my tests.

xmtn does not entirely follow DVC's philosophy: It only implements
DVC's protocols, but doesn't provide its own UI that parallels DVC's.
Hence, much of xmtn's functionality is only available through DVC.
This is because xmtn currently provides only few features beyond what
DVC requires, and implementing a redundant UI was not a high priority
for me.

Currently, the following parts of the DVC protocol are not implemented
by xmtn:

  * xmtn-dvc-send-commit-notification, xmtn-dvc-submit-patch: These
    commands send an e-mail.  Probably useful to people who use a
    certain work flow, but not to me right now.  These will have to
    wait until someone comes along who actually has a use for them.

  * xmtn-insinuate-gnus: Need to find out what, precisely, this is
    supposed to do.  I don't use Gnus myself.

  * xmtn-dvc-save-diff: xhg seems to be the only backend that
    implements this.  It really seems this could be moved into the
    common part of DVC anyway.  Won't bother implementing it right
    now.

  * xmtn-dvc-pull: Should be easy.  But syncing via command line is
    acceptable to me at the moment.  The docstring looks like this
    needs to do both mtn pull and mtn update -- but I doubt that this
    is a good idea for monotone.



* Internals

This section describes some of the internals of xmtn and some of the
design decisions behind it.



** Conventions

monotone.el (from montone's contrib/ directory) already uses the
prefix mtn-.  monotone- is already taken by Wim Oudshoorn's e-monotone
package.  So this package is named xmtn.  xhg, xcg, xdarcs seem to be
in similar situations.

The prefix xmtn- is for definitions exported for the user or for DVC,
the prefix xmtn-- is for internal definitions.  Similarly,
xmtn-automate uses xmtn-automate- and xmtn-automate--, etc.

It seems like "monotone" is usually written in small letters.  The
manual capitalizes it at the beginnings of sentences, but e.g. the web
page or mtn --version never capitalize it at all -- then again, the
web page doesn't capitalize much at all.  In xmtn, we capitalize it
like a noun.  xmtn and mtn (as a command name) are always in lower
case.

Monotone uses the term "workspace", DVC uses the term "tree".  In our
UI, we use "tree" for consistency with DVC.  The idea behind this
decision was that consistency with DVC (and other aspects of Emacs'
UI) is more important than consistency with other monotone front-ends.
But I'm not so sure about this any more; the term "workspace" is so
much more clear... But I guess it makes little sense for version
control systems that don't distinguish between workspaces and
branches.



** Architecture

This section is unlikely to stay fully up-to-date as xmtn's
implementation evolves, but should remain useful as a general
introduction to xmtn's architecture.

xmtn consists of several modules.  One way of understanding their
relationship is to group them into layers.


    User-visible functionality:    xmtn-dvc.el, xmtn-revlist.el

    Domain-specific utilities:     xmtn-ids.el

    High-level interface to mtn:   xmtn-automate.el, xmtn-basic-io.el

    Low-level interface to mtn:    xmtn-run.el

    Monotone-related definitions:  xmtn-base.el

    Support libraries:             xmtn-compat.el

    Language extensions:           xmtn-match.el


Each module should only depend on modules at layers beneath it.  (At
least, that's the idea; the code might not satisfy this perfectly.)

xmtn-dvc.el implements the protocols required by DVC, except for
functionality related to interactive display and manipulation of
revision history, which is in xmtn-revlist.el.

xmtn-ids.el contains code to resolve symbolic revision ids in a
certain syntax to explicit hash ids.  DVC needs this, but xmtn
provides some useful extensions.  For example, a symbolic id `(xmtn
(previous-revision (previous-revision (revision
"75da2575dfc565f6976ed5dd1997bc7afc0ce908"))))' resolves to `(revision
"721c3ab9b5099d3ed7d8b807e08382f3c95badec")'; i.e. the parent of the
parent of revision 75da2575dfc565f6976ed5dd1997bc7afc0ce908 is
revision 721c3ab9b5099d3ed7d8b807e08382f3c95badec.

xmtn-automate.el and xmtn-basic-io.el implement an interface to
monotone's automate functionality and a parser for monotone's basic_io
output format.  These modules aren't specific to DVC and should be
reusable by other Emacs Lisp code that wants to use monotone.

xmtn-run.el provides functions for running individual (non-automate)
monotone commands and checking the version of the monotone executable.
The functionality of xmtn-run.el isn't specific to DVC, either, but
its current implementation depends on DVC's process handling
functions, so it's fairly heavyweight.

xmtn-base.el was supposed to contain definitions related to monotone
that are common to xmtn-run.el, xmtn-automate.el and/or
xmtn-basic-io.el, to avoid having to have dependencies on xmtn-run.el
in xmtn-automate.el or xmtn-basic-io.el.  This refactoring is not
complete (yet?), though.

xmtn-compat.el contains compatibility wrappers for some Emacs Lisp
functions that are not fully portable across Emacs versions.

xmtn-match.el provides a pattern-matching facility for Emacs Lisp that
is very useful for destructuring DVC REVISION-IDs and processing
basic_io stanzas the way xmtn-basic-io.el parses them.  But it is
rather generic and could also be useful for code entirely unrelated to
montone and DVC.

There are a few automated regression tests in
lisp/tests/xmtn-tests.el.




** Implementation details



*** Futures

For some subprocess interactions, xmtn uses a concept called
"futures".  In this context, a future is a concurrent computation
represented by a zero-argument anonymous function that, when called,
blocks until the concurrent computation finishes, and returns its
result.

For example, the function `xmtn--unknown-files-future' returns a
future for the list of unknown files instead of returning the list of
unknown files directly.  This allows Emacs Lisp code to ask monotone
for the list of unknown files, but then do something different while
monotone computes the list.  Only when Emacs actually needs the list
in order to continue, it calls the future and waits for monotone to
finish (if it hasn't finished already).

If a future is called a second time or more often, it will just keep
returning the same result.  (What a future does if the concurrent
computation terminates unsuccessfully isn't currently very
well-defined.  It should probably signal an error when it is called.)

Spawning computations in parallel has yielded tremendous speed-ups for
certain parts of xmtn (at least in some versions -- I haven't profiled
it recently).  Futures make this type of parallelism simple to deal
with.




*** Notes on variable names and dynamic bindings

In higher-order functions (functions that take functions as
arguments), xmtn attempts to avoid introducing spurious dynamic
bindings because they might shadow bindings that the caller wants to
provide to the argument function.  xmtn uses `lexical-let' for this
purpose.  Unfortunately, function arguments are always dynamic
bindings in Emacs Lisp.  That's why the argument names of higher-order
functions in xmtn always have the prefix xmtn-- and are immediately
re-bound to (pseudo-)lexical variables using `lexical-let'.  This
makes it unlikely that the arguments will collide with the caller's
variables.

The alternative would be to always use `lexical-let' for bindings that
should be passed through higher-order functions to closures.  This is
the most reliable approach, and xmtn also follows it.  But errors
resulting from accidental violations of this convention can be very
hard to debug, so the above is still useful for additional safety.




 LocalWords:  DVC minibuffer UI montone xmtn revlist unmerged docstring backend
 LocalWords:  backends destructuring mtn

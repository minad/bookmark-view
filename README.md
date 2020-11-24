# bookmark-view.el - Use bookmarks to persist the current view

This package provides functions which
allow storing the window configuration (called a view here) as Emacs bookmarks.
This way the view is automatically persisted
and can be restored after restart.

There are many alternative library like perspective and burly, which
are more ambitious. bookmark-view seems to be the simplest
thing that just works.

The library provides the command `bookmark-view`. If called
interactively it reads a bookmark name via `completing-read`.
If a view bookmark with the given name exists, open it,
otherwise the current view is saved under that name.

Furthermore there are the commands `bookmark-view-push` and
`bookmark-view-pop` to store a stack of views.
Please take a look at the source to see all of the provided commands.

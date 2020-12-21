# bookmark-view.el - Use bookmarks to persist the current view

This package provides functions which allow storing the window configuration
(called a view) as an Emacs bookmark. This way the view is automatically
persisted and can be restored after restart.

There are alternative view libraries like perspective and burly, which are more
ambitious. bookmark-view aims to be the simplest thing that marries bookmarks
and window configurations.

The library provides the command `bookmark-view`. If called interactively it
reads a bookmark name via `completing-read`. If a view bookmark with the given
name exists, open it, otherwise the current view is saved under that name.

Furthermore there are the commands `bookmark-view-push` and `bookmark-view-pop`
to store a stack of views. Please take a look at the source code to see all of
the provided commands.

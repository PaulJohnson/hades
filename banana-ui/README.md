# GTK Widgets with a Reactive API

Apart from the top level application window, the DSM user interface is based on this library.

The folders within here are:

base
: The source code for the `banana-ui-base` library. This is the abstract (i.e. not dependent on
   GTK) definition of the user interface "gadgets" (the term
   "widget" is reserved for GTK entities). Application developers will import
   `Reactive.Banana.ArrowDialog`, which defines composable UIs as
   [arrows](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows). There is also an older
   module `Reactive.Banana.Dialog` which provides less functionality and was being phased out.

demo-arrow
: A demo and test harness for the features of `Reactive.Banana.ArrowDialog` on GTK.

demo-dialog
: A similar demo for the features of `Reactive.Banana.Dialog` on GTK.

demo-webit
: The same for the web-based implementation of gadgets.

gtk
: A GTK3 implementation of the features of the `banana-ui-base` library.

webits
: A web-based implementation of `Reactive.Banana.ArrowDialog`. See inside for details.

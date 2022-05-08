# The Haskell Diagram Editing System (HADES)

HADES is a library for structured diagrams backed by models. This means that a diagram in HADES
is not just a picture with lines and boxes: a diagram is just one view of entities within a
wider model of some kind. Multiple diagrams can present overlapping views of the same model. If
an entity appears in several diagrams then a change to the entity will be reflected in all of those
diagrams.

The modelling functions also include powerful querying and reporting features.

## History

The HADES library was originally created for the Diametric Safety Case Manager (DSM), which was a
commercial product. However the DSM was not successful. Diametric Software Limited ceased trading
in January 2022, and the software has been made open source.

The version released as open source is not identical to the commercial product:

* The commercial version used a private GTK theme for its look and feel. The open source version
uses the locally configured theme.

* The commercial version included a number of licensed graphics, particularly an icon library.
These cannot be included in an open source version.

* The commercial version required a license purchase. The open source version has removed all
code associated with purchasing and verifying licenses.

* The commercial version included credits for the open source libraries incorporated in the
product. These are no longer necessary.

Because the Git history for the commercial version includes the licensed graphics it isn't possible
to make that history public. Instead a new repository has been created using the final code from
the commercial version but without any licensed graphics.

## Using the Library

HADES is a platform on which you can create new modelling and diagramming tools. The DSM and its
diagrams can be taken as examples. A tutorial on creating new types of diagrams would be something
to add in the future, if anyone wants to write one.

## Building the Software

### Linux

Red Hat family prerequisites:

* In CentOS:

    `dnf config-manager --set-enabled PowerTools`
    `dnf install gtk3-devel atk-devel gobject-introspection-devel cairo-gobject-devel librsvg2-devel`
    `dnf install pango-devel cairo-devel libicu-devel libpq-devel glib2-devel zlib-devel`

Debian family prerequisites:

    `apt-get install libgtk-3-dev librsvg2-dev libpq-dev gobject-introspection libgirepository1.0-dev`

Then install the Haskell Stack:

    `wget -qO- https://get.haskellstack.org/ | sh`


Download the source and then `stack install`. This will create an executable called `dsm`.

On Windows you can compile the DSM using ['MSYS2'](https://www.msys2.org/). If gi-gtk libraries
fail to compile with "Failed to load shared library" messages then copy `zlib1.dll` from
`mingw64/bin` to the GHC binaries folder.

## The Reactive Banana Fork

HADES depends on the [reactive-banana](https://hackage.haskell.org/package/reactive-banana) library.
However some extensions were needed:

* The `switchE` function had to be given an initial event stream (a bit like `switchB`), so the
`switchE1` function was defined.

* When an event stream is passed to `execute`, any `reactimate` calls in the events get executed
and are then running in perpetuity. HADES uses `execute` to refresh user interface elements in
response to new data, but this leaves `reactimate` calls from the old UI elements still running,
even though their effects cannot be seen. Over time these build up, causing the application to
run slower and leak memory (a "space-time leak"). To solve this a version of `reactimate` was
defined to return a "stop" action:

    ``reactimate_ :: Event (IO ()) -> MomentIO (IO ())``

    The `IO` action returned by this will stop the event action and allow any widgets associated
    with it to be garbage collected.

The `switchE1` functionality is going to be incorporated in a future version of *reactive-banana*.
However the `reactimate_` is not going to be included. Instead the current plan is to add a
different primitive so that programmers no longer need to manually collect and execute the stop
functions.

When these features have been incorporated into *reactive-banana* the plan is to migrate to the
mainstream version. Until then HADES will remain dependent on a
[separate fork](https://github.com/PaulJohnson/reactive-banana/commit/684dd9eeabfaa66928d51515634bae0e53d3773c).

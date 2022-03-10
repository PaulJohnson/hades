# Webits

This was intended as a web-based GUI for the DSM, but never got beyond the demo stage.
It implements the abstract UI primitives defined in `Reactive.Banana.GadgetPrimitives`
in the `banana-ui-base` package.

The name "webit" was chosen because "gadget" and "widget" were already taken.

See the `demo-webit` package for an example use of this package.

Despite its association with `banana-ui-base` this library does not use the
`reactive-banana` facilities. Instead it uses a collection of lightweight Haskell threads to
implement events and behaviours using Software Transactional Memory (STM). This was probably a
mistake: the architecture generates lots of threads communicating via STM. `reactive-banana`
would have been more efficient.

This folder includes copies of [JQuery-UI](https://releases.jquery.com/ui/) and
[AppendGrid](https://appendgrid.apphb.com/). Now that the software is open source these should
probably be replaced with references to CDNs.

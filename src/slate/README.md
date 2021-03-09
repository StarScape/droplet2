# Slate

Slate is the rich text editor engine that sits at the core of Droplet. It is designed as an internal library, meaning:

1. Code inside of the `slate` namespace should *never* be dependent on anything in the other parts of Droplet.
2. Even if not done currently, *in principle* it should be possible to pick up Slate and drop it into a totally separate application.

Most of the code you might need to consume from the main Droplet code is inside of the `slate.core` namespace.

```
TODO: add a brief overview of the high-level architecture of Slate (Runs, Paras, Doc, Selection, viewmodel layer, etc).
```

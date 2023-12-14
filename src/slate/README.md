# Slate

Slate is the rich text editor engine that sits at the core of Droplet. It is designed as an internal library, meaning:

1. Code inside of the `slate` namespace should *never* be dependent on anything in the other parts of Droplet.
2. Even if not done currently, _in principle_ it should be possible to pick up Slate and drop it into a totally separate application.

Most of the code you might need to consume from the main Droplet code is inside of the `slate.editor-ui-state` namespace.

> Yes, I accidentally named it Slate without realizing there's already another web-based rich text editor called [SlateJS](https://github.com/ianstormtaylor/slate). Whoops.

# Bird's Eye View

Slate does not use a `<textarea>`, [content editable](https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Editable_content), or any other built-in text editing mechanism or libraries. Instead, similar to VSCode, it:

- Maintains its own data-model for the state of the editor, behind the scenes
- Uses a hidden `<input>` to capture key presses and other events
- Updates the data model accordingly for each event
- Renders the data model as HTML. Everything -- text, the text cursor and selection, etc, is just HTML.

This means __a lot__ more flexibility, but also that there's more functionality that Slate has to implement itself. Basically all the browser is responsible for is (a) rendering text, and (b) handling keyboard input.

## Core Data Model

See `./model/`.

The atomic unit of Slate is the __`Run`__, which is a string with a set of formats attached, e.g. `#{:italic :bold}`.

A __`Paragraph`__ contains one or more `Runs`, plus a paragraph type, which default to `:body` (`:ol` and `:ul` are also supported, for ordered and unordered lists).

> __Note:__ a paragraph's `Run`s can never be adjacent to another `Run` with the exact same formatting, e.g. R1 having formatting `#{:italic}` cannot be directly next to R2 having formatting `#{:italic}`. If R2 instead has formatting of, for example, `#{:italic :bold}`, then this is a legal state. Enforcement of this constraint in handled automatically when inserting into paragraphs.

A __`Document`__ contains the list of all paragraphs in the document (and possibly some other metadata, if that ever becomes necessary).

A __`Selection`__ consists of a start paragraph (the paragraph's index), an offset into that paragraph, an end paragraph and an offset into that paragraph, plus a `:backwards?` field. If the start and end of the selection are the same, it is a single selection, i.e. a normal text caret. `Selection`s keep track of a few other things as well, see `selection.cljs` for details.

The __`EditorState`__ contains both the current `Document` and the current `Selection`. Most operations on a `EditorState` return a new `EditorState`.

> __Note:__ under the covers, the list of paragraphs is keeping track of modifications to itself and storing that in a changelist, so that paragraphs in the DOM can be selectively updated instead of rerendering everything, or doing expensive diffing. This requires a bit of magic, but is invisible to 99% of the code, and only in the `dll` NS and in the core render loop is it something that one needs to be aware of.

The __`history`__ object (currently just a plain map and not a record, as it implements no protocols) contains the editor history, defined as a succesion of `EditorState`s, along with the changelists between them. There is also a `:tip` field, a "current working state" that has not yet been integrated into the history backstack.

## UI Layer

The __`viewmmodel`__ is an intermediate stage for each paragraph where the width of each character is measured and the paragraph is split into lines. This information _has_ to be kept track of because some operations (up/down, goto start/end of line) are dependent on line layout.

The __`view`__ namespace contains all the code that has to deal directly with the DOM. There is a lot of tricky, highly-side-effectful code here.

The __`EditorUIState`__ contains the whole state needed to render the UI and handle updates. This is the only thing within Slate stored in an atom. The core update loop of Slate is in `editor_ui_state.cljs`.

## Other stuff

The interceptor systems (see `interceptors.clj(s)` and `default_interceptors.cljs`) handles shortcuts and input events.

The `dll.cljs` namespace contains the data structure that a `Document`s `Paragraph`s are stored in, which has some unique characteristics. See that NS's docstring for more details.

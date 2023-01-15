# Droplet 💧

Source code for the Droplet app.

<!-- # Status

https://user-images.githubusercontent.com/4681820/168147023-92a1065a-1467-45d1-b1f0-dc04c96760ca.mov

Droplet is under heavy development and should be considered in the pre-alpha stage. No UI work has been done yet, and Electron integration, file saving and loading, import/export, etc, are all still `TODO`™.

However, the rich text editor is _mostly_ complete and stable. It supports:

- Rich text formatting: _italics_, __bold__, `h1` and `h2` headings, bulleted and numbered lists. In place but not yet implemented: <ins>underlining</ins>, ~~strikethrough~~.
- Common text editing shortcuts: ⌥+→ / ⌥+← to jump between words, ⌘+→ / ⌘+← to jump to start/end of line, etc.
- Full undo and redo
- Copy and paste (plain text supported to and from Droplet to other apps; rich text currently only supported Droplet-to-Droplet)
- Find and replace (currently no UI)
- A rich "interceptor" system for handling shortcuts and editor actions in an easily-extensible manner
- A fully-immutable document models, which means __tests__! Rich text editors are finnicky and full of edges cases, and being able to trivially unit test any editor action in isolation is a life saver.
- Some fancy-pants completions, like completing -- to an em dash and auto-surrounding parens and quotations.

More details on the structure of the editor in `src/slate/README.md`. -->

# Development

**Start shadow-cljs builds:**

```
npm run dev-watch
```

Then, **start up Electron** (normally should be done in a separate terminal so that the electron process can be restarted, but if there's no need to modify the main NodeJS process's code then it can be started with `&&`):

```
electron .
```

REPL can then be jacked into in your editor.

## Testing

In order to help the tests run as fast as possible, tests which are not dependent on any browser APIs use `shadow-cljs`'s `:node-test` target, and should have a namespace name ending in `-test`. Browser-dependent tests use `:karma` and should have a namespace ending in `browser-test`. Every test will automatically be run as either a browser or Node test based on whether it ends in `-browser-test` or just `-test`.

> Note: in order for the browser tests to run correctly you will need to have the CHROME_BIN environment variable set, see [here](https://github.com/karma-runner/karma-chrome-launcher/issues/62).

__To run both suites of tests, run:__

```bash
npm run test
```

__To run just the Node tests:__

```bash
npm run node-test
```

__To run just the Karma tests:__

```bash
npm run browser-test
```

There's also a specific test build for the DLL tests (see `src/slate/dll.cljs`). Running the Node tests will run the DLL tests, but because it can be tiring to wait for the other tests to run while working on code for an independent data structure, you can do this if you want to run just the DLL tests:

```bash
shadow-cljs compile test-dll
```

That build is set to autorun `:autorun true`, so there is no need for any additional step.

# Release

Build installers for all platforms:

```bash
build-releases
```

Installers and binaries will be outputted to `dist/`.

It's a good idea not to have a dev instance of Droplet running before building the installers, as building a release will output JS files to the same directory that a developer session uses, and you want to make sure that the minified, production JS files are used.

## Output/other directories

- Browser code is output to `public/js/`.
- Test code is ouput to `dev_out/`.
- When building, installers are output to `dist/`
- `test_files/` contains `.drop`, `.rtf`, `.html` and other files used for running import and export tests
- `build_res/` contains resources needed to build the Electron binaries like icons
- `scripts/` contains bash scripts for running tests, etc.
- `art_assets/` contains the .afdesign files for icons I've made
- `public/` contains browser resources like the `index.html` file, CSS, fonts, and icons

# Droplet 💧

Source code for the Droplet app.

<!-- TODO: would be good to update the below with a feature-list, or better yet a list of what makes Droplet a technically interesting project. -->
<!-- # Status

- Rich text formatting: _italics_, __bold__, `h1` and `h2` headings, bulleted and numbered lists. In place but not yet implemented: <ins>underlining</ins>, ~~strikethrough~~.
- Common text editing shortcuts: ⌥+→ / ⌥+← to jump between words, ⌘+→ / ⌘+← to jump to start/end of line, etc.
- Full undo and redo
- Copy and paste (plain text supported to and from Droplet to other apps; rich text currently only supported Droplet-to-Droplet)
- Find and replace
- A rich "interceptor" system for handling shortcuts and editor actions in an easily-extensible manner
- A fully-immutable document model, which means __tests__! Rich text editors are finnicky and full of edges cases, and being able to trivially unit test any editor action in isolation is a life saver.
- Some fancy-pants completions, like completing -- to an em dash and auto-surrounding parens and quotations.

More details on the structure of the editor in `src/slate/README.md`. -->

# Development

**Start shadow-cljs builds:**

```
npm run dev-watch
```

Then, **start up Electron**. Normally this should be done in a separate terminal so that the main Electron process can be restarted as needed.

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

There's also a specific test build for the DLL tests (see `src/slate/model/dll.cljs`). Running the Node tests will run the DLL tests, but because it can be tiring to wait for the other tests to run while working on code for an independent data structure, you can do this if you want to run just the DLL tests:

```bash
shadow-cljs compile test-dll
```

That build is set to autorun `:autorun true`, so there is no need for any additional step.

# Release


## Building
Build installers for all platforms:

```bash
npm run build-releases
```

Installers and binaries will be outputted to `dist/`.

It's a good idea to close any dev instances of Droplet and stop watched compilation before building the installers, as building a release will output JS files to the same directory that a developer session uses, and you want to make sure that the minified, production JS files are used.

**Before making a release, remember to also bump the version in `package.json`.**

### Building a snapshot for Mac

To build a snapshot version of _only_ the macOS executable (not for final release, just for local testing):

```bash
npm run build-snapshot-mac
```

This will skip signing and notarizing the app.

## Signing Mac Apps

If a valid Developer ID Application certificate and a Developer ID Installer certificate are both present in Keychain, `electron-builder` should automatically detect them and sign the binary. 

## Notarizing Mac Apps

An `afterSign` script is registered with `electron-builder` to notarize the macOS releases. The env variables `APPLE_ID` (Apple ID email) and `APPLE_ID_PASSWORD` (app-specific password tied to my Apple ID for Droplet) need to be set, and can be done by creating a `.env` file as the `dotenv` package is installed.

## Deploying to S3 and Cloudfront
__After building the installers, they can be deployed with__ `npm run deploy-installers`, which will upload them to their S3 bucket, generate a JSON file with their URLs, and then copy that file to the `../droplet-website` folder. The website will then have to be redeployed from its own repo in order for the change to take affect.

The AWS CLI must be installed and configured in order for this to work.

## Demo video auto-recording

There is a script to automatically record the hero video seen on the [Droplet website](https://dropletwriter.com):

```bash
npm run record-demo-video
```

This will launch a pupeteered version of Droplet, play through the demo script, and save a `demo.mp4` video in the root directory. This capability may be expanded to automatically record other demo videos in the future.

## Output/other directories

- Browser code is output to `public/js/`.
- Test code is ouput to `dev_out/`.
- When building, installers are output to `dist/`
- `test_files/` contains `.drop`, `.rtf`, `.html` and other files used for running import and export tests
- `build_res/` contains resources needed to build the Electron binaries like icons
- `scripts/` contains bash scripts for running tests, etc.
- `art_assets/` contains the .afdesign files for icons I've made
- `public/` contains browser resources like the `index.html` file, CSS, fonts, and icons

# Changelog

### 0.0.2 - ([Released](https://github.com/StarScape/droplet2/releases/tag/v0.0.2))

- Added a confirmation dialog before saving file
- Added smart-nav actions to the application menu
- Enabled the smart-nav shortcuts for Windows
- Added 5px gap between paragraphs
- Switch file open/close handling from persistent-atom to Re-frame
- Remove gap between hightlighted lines (also temporarily remove border-radius from highlighting)
- Better tab handling, and increase tab width
- Decrease maximum width of editor area
- Change font size change delta from 5px to 1px
- Fix bug where cut action was not added to editor history
- Implement double click to select a word, and triple click to select a paragraph
- Don't grey out editor surface when losing focus to another window, only when losing focus to something else inside Droplet
- Removed editor history from .drop file; this is regrettable but the price of serializing and deserializing all that data was getting steep, with 5+ second load times for files with significant history. Files sizes were also balooning to ~45MB for a ~3500 word document.
- Fixed "click on file to open it in Droplet" behavior on macOS
- Fixed bug where double and triple click were not getting selection formatting set correctly
- Fixed bug where find and replace popover was not appearing
- Minor tweaks to colors
- Word count displays word count of selection when selection is range
- Open file when passed in ARGV
- Fixed bug where index in find and replace popup could become negative
- Focus is no longer lost on editor pane when find and replace popup is selected

### 0.0.3 - ([Released](https://github.com/StarScape/droplet2/releases/tag/v0.0.3))

- Fixed issue where undo/redo events could fire both for the find and replace inputs and the main editor pane simultaneously
- All text is escaped before being entered into the document
- Changes to fonts
- Added ability to auto-record demo video


### 0.0.4 - In Development

- Myriad UI improvements
- "Open Recent" in File menu
- Heavy changes to text editor internals
- .drop file version v2 -> v3
- Fix unexpected behavior when copying to plaintext
- Add commas to word-count number when >999 (e.g. 200,000)
- Fixed bug in find and replace

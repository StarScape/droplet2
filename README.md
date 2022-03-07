# Droplet 💧

Source code for the Droplet desktop app.

# Development

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

## Output directories

- Browser code is output to `public/js/`.
- Test code is ouput to `out/`.

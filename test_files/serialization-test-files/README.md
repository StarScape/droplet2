Test `.drop` files for Slate editor.

Note that the _file format version_ is separate from the Droplet version. Droplet might go through several releases without changing the file format, and so that version would stay the same, while the version number of Droplet itself would change each time.

On importing a file with an outdated version, Slate will migrate it to the latest version. It's important to keep files around from each version for this reason. Test files are organized according to the `.drop` version.

For current version of the `.drop` format, see `slate.serialization/current-version`. Not that this is just an integer, not a semver-style number like 1.2.0, etc.


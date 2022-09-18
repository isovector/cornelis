# Working on cornelis

The project can be built and tested with both `stack` and `cabal`.  In order to
successfully run the test suite, `nvim` needs to be on the `$PATH`.

## Development using Nix

Nix integration is provided in the form of a Nix Flake.  To build the project,
simply run `nix build`.  For interactive development, use one of the provided
shells:

```sh
# Launch a shell with all development dependencies installed
# (stack, system libraries, etc.):
nix develop

# Run tests from a shell that includes a Neovim binary:
nix develop '.#testShell' --command stack test
```

{ compiler ? "ghc921" }:
(import ./default.nix {
  inherit compiler;
}).shell

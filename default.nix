let

  pkgs = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/0eb0ddc4dbe3cd5415c6b6e657538eb809fc3778";
    sha256 = "09ammqxzqydj97lk5iwrlg4xnj7b2pnhj6qpxa0pbp9z0651yvz6";
  }) {
    config = {};
    overlays = [];
  };
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskell.packages.ghc882.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {
      on-demand-minecraft = self.callCabal2nix "on-demand-minecraft" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^.*\\.cabal$"
      ]) {};

      socket-activation = hlib.unmarkBroken (hlib.doJailbreak (hlib.appendPatch super.socket-activation (pkgs.fetchpatch {
        # https://github.com/ddfisher/haskell-socket-activation/pull/6
        url = "https://github.com/ddfisher/haskell-socket-activation/commit/7545480ed6f5e88dacf7041f73f6c1e15a269984.patch";
        sha256 = "1qw7zv29n1mdgv2asiwb4mwr7h5wpyv6a6mswf9rj1mchxahmgd8";
      })));
    });
  });
  
  env = hpkgs.shellFor {
    packages = p: [ p.on-demand-minecraft ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };
in hpkgs.on-demand-minecraft // {
  inherit env hpkgs;
}

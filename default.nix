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
        "^app.*"
        "^lib.*"
        "^tests.*"
        "^.*\\.cabal$"
      ]) {};

      socket-activation = hlib.unmarkBroken (hlib.doJailbreak (hlib.appendPatch super.socket-activation (pkgs.fetchpatch {
        # https://github.com/ddfisher/haskell-socket-activation/pull/6
        url = "https://github.com/ddfisher/haskell-socket-activation/commit/7e33b1f89e2eb7c4b6fd92fdbd89a65e93c25d3a.patch";
        sha256 = "1ih2y9gwi7ywa21mm497kgkynabd7y0pvwivfrgd9p84rw01haq3";
      })));

      polysemy = self.callHackage "polysemy" "1.3.0.0" {};

      polysemy-plugin = hlib.doJailbreak (hlib.unmarkBroken super.polysemy-plugin);
    });
  });

  env = hpkgs.shellFor {
    packages = p: [ p.on-demand-minecraft ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };

  pkg = hpkgs.on-demand-minecraft;

in hlib.justStaticExecutables pkg // {
  inherit env hpkgs;
}

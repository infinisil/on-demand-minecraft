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
        "^app$"
        "^lib$"
        "^tests$"
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

  pkg = hpkgs.on-demand-minecraft;

in pkg // {
  inherit env hpkgs;

  test =
    let
      testingLib = import (pkgs.path + "/nixos/lib/testing-python.nix") {
        system = builtins.currentSystem;
      };
    in
      testingLib.makeTest {
        machine = {
          systemd.sockets.on-demand-minecraft = {
            wantedBy = [ "sockets.target" ];
            socketConfig.ListenStream = "25565";
          };

          systemd.services.on-demand-minecraft = {
            serviceConfig.ExecStart = "${pkg}/bin/on-demand-minecraft";
          };
        };
        testScript = ''
          machine.start()
          machine.wait_for_unit("on-demand-minecraft.socket")

          machine.require_unit_state("on-demand-minecraft.service", "inactive")

          res = machine.succeed('echo "Hello\nquit" | nc localhost 25565')
          if res != "Hello\nSee ya\n":
              raise Exception('Did not get the expected output, but "{}" instead'.format(res))

          machine.sleep(0.1)
          machine.require_unit_state("on-demand-minecraft.service", "inactive")

          res = machine.succeed('echo "Hi there\nquit" | nc localhost 25565')
          if res != "Hi there\nSee ya\n":
              raise Exception('Did not get the expected output, but "{}" instead'.format(res))

          machine.sleep(0.1)
          machine.require_unit_state("on-demand-minecraft.service", "inactive")

          # Check that the service can accept multiple connections at a time
          res = machine.succeed(
              """
                  {
                    echo "level1"
                    sleep 1
                    echo "level2\nquit" | nc localhost 25565
                    sleep 1
                    echo "quit"
                  } | nc localhost 25565
              """
          )
          if res != "level1\nlevel2\nSee ya\nSee ya\n":
              raise Exception('Did not get the expected output, but "{}" instead'.format(res))

          machine.sleep(0.1)
          machine.require_unit_state("on-demand-minecraft.service", "inactive")
        '';

      };

}

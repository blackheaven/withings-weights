{
  description = "withings-weights";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    servant-prometheus = {
      url = "github:worm2fed/servant-prometheus";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    # flake-utils.lib.eachDefaultSystem
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskellPackages.override
          {
            overrides = hself: hsuper: {
              # prometheus-wai-middleware = jailbreakUnbreak hsuper.prometheus-wai-middleware;
              servant-prometheus = jailbreakUnbreak (hsuper.callCabal2nix "servant-prometheus" inputs.servant-prometheus { });
            };
          };

        assets = builtins.filterSource
          (path: type: pkgs.lib.strings.hasInfix "assets" path) ./.;
      in
      rec {
        packages.withings-weights =
          haskellPackages.callCabal2nix "withings-weights" ./. rec {
            # Dependency overrides go here
          };
        packages.withings-weights-image = pkgs.dockerTools.buildImage {
          name = "blackheaven/withings-weights";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths =
              [ pkgs.cacert self.packages.${system}.withings-weights assets ];
            pathsToLink = [ "/bin" "/etc" "/assets" ];
          };
          runAsRoot = ''
            #!${pkgs.runtimeShell}
            mkdir -p /assets
            mkdir -p /store
          '';
          config = {
            Entrypoint = [ "/bin/withings-weights" ];
            Env = [
              "OAUTH_STORE_PATH=/store/users.json"
              "SERVER_PORT=80"
              "SERVER_ASSETS_PATH=/assets"
            ];
            Volumes = {
              "/store" = { };
              "/assets" = { };
            };
          };
        };

        defaultPackage = packages.withings-weights;

        devShell =
          let
            scripts = pkgs.symlinkJoin {
              name = "scripts";
              paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin {
                ormolu-ide = ''
                  ${pkgs.ormolu}/bin/ormolu -o -XNoImportQualifiedPost -o -XOverloadedRecordDot $@
                '';
              };
            };
          in
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              scripts
              ormolu
            ];
            inputsFrom = [ self.defaultPackage.${system}.env ];
          };
      });
}

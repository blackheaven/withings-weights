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

        nixpkgsOverlay = _final: _prev: {
          withings-weights = self.packages.${system}.withings-weights;
        };

      in
      rec {
        packages.withings-weights =
          (haskellPackages.callCabal2nix "withings-weights" ./. {}).overrideAttrs (old: {
              postInstall = (old.postInstall or "") + ''
                mkdir -p $out/share/assets
                cp -dr "${assets}/assets" $out/share/
              '';
            });
        packages.withings-weights-image = pkgs.dockerTools.buildImage {
          name = "blackheaven/withings-weights";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths =
              [ pkgs.cacert self.packages.${system}.withings-weights ];
            pathsToLink = [ "/bin" "/etc" "/assets" ];
          };
          runAsRoot = ''
            #!${pkgs.runtimeShell}
            mkdir -p /store
            # Default assets
            cp -dr "${self.packages.${system}.withings-weights}/share/assets" /
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

        overlays.default = nixpkgsOverlay;

        nixosModules.default =
          { pkgs, lib, config, ... }:
          let
            cfg = config.services.withings-weights;
            defaultStoreRootPath = "/var/lib/withings-weights";
            defaultStorePath = "${defaultStoreRootPath}/users.json";
            defaultUser = "withings-weights";
            defaultGroup = "withings-weights";
          in
          {
            options = with lib; {
                services.withings-weights = {
                  enable = mkEnableOption "Simple withings weight stats WebUI";
                  package = lib.mkPackageOption pkgs "withings-weights" {};
                  assets = lib.mkOption {
                    type = types.path;
                    default = "${cfg.package}/share/assets";
                  };
                  store = lib.mkOption {
                    type = types.path;
                    default = defaultStorePath;
                  };
                  oauthCallbackUrl = lib.mkOption {
                    type = types.str;
                  };
                  oauthClientIdFile = lib.mkOption {
                    type = types.path;
                  };
                  oauthClientSecretFile = lib.mkOption {
                    type = types.path;
                  };
                  user = mkOption {
                    type = types.str;
                    default = defaultUser;
                  };
                  group = mkOption {
                    type = types.str;
                    default = defaultGroup;
                  };
                  openFirewall = lib.mkOption {
                    type = types.bool;
                    default = false;
                  };
                  port = lib.mkOption {
                    type = types.port;
                    default = 5555;
                  };
              };
            };
            config = lib.mkIf cfg.enable {
              nixpkgs.overlays = [ nixpkgsOverlay ];

              networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.port;

              users.users = lib.mkIf (cfg.user == defaultUser) {
                withings-weights = {
                  isSystemUser = true;
                  group = cfg.group;
                };
              };

              users.groups = lib.mkIf (cfg.group == defaultGroup) {
                withings-weights = {};
              };

                systemd.tmpfiles.rules = lib.mkIf (cfg.store == defaultStorePath) [
                  "d ${defaultStoreRootPath} 0755 ${cfg.user} ${cfg.group} -"
                ];

                systemd.services.withings-weights = {
                  description = "Withings Weights WebUI";
                  after = [ "network.target" ];
                  wantedBy = [ "multi-user.target" ];
                  script = ''
                    export OAUTH_CLIENT_ID=$(cat ${cfg.oauthClientIdFile})
                    export OAUTH_CLIENT_SECRET=$(cat ${cfg.oauthClientSecretFile})
                    exec ${lib.getExe cfg.package}
                  '';
                  serviceConfig = {
                    User = cfg.user;
                    Group = cfg.group;
                    Environment = [
                      "SERVER_PORT=${toString cfg.port}"
                      "SERVER_ASSETS_PATH=${cfg.assets}"
                      "OAUTH_CALLBACK_URL=${cfg.oauthCallbackUrl}"
                      "OAUTH_STORE_PATH=${cfg.store}"
                    ];
                  };
                };
            };
          };

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

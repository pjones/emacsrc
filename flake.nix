{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    desktop-scripts.url = "github:pjones/desktop-scripts";
    desktop-scripts.inputs.nixpkgs.follows = "nixpkgs";

    passmm = {
      url = "github:pjones/passmm";
      flake = false;
    };

    telega = {
      # Commit from May 9, 2024:
      url = "github:zevlg/telega.el/99477b075f4032a49283d730e909a26f11606ee5";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      # List of supported systems:
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "armv7l-linux"
        "i686-linux"
      ];

      # Function to generate a set based on supported systems:
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Like `forAllSystems` except just those that are Linux:
      forLinuxSystems = f: builtins.listToAttrs
        (builtins.filter (set: set ? name)
          (builtins.map
            (system:
              let pkgs = nixpkgsFor.${system}; in
              nixpkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
                name = system;
                value = f system;
              })
            supportedSystems));

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system:
        import nixpkgs { inherit system; });
    in
    {
      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        {
          default = self.packages.${system}.emacsrc-wayland;

          emacsrc-xorg = import ./. {
            inherit pkgs inputs;
            emacs = pkgs.emacs.override { withGTK3 = true; };
          };

          emacsrc-wayland = import ./. {
            inherit pkgs inputs;
            emacs = pkgs.emacs.override { withPgtk = true; };
          };
        });

      apps = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        {
          default = {
            type = "app";
            program = toString (pkgs.writeShellScript "emacsrc" ''
              ${self.packages.${system}.default}/bin/e -f
            '');
          };

          tutorial = {
            type = "app";
            program = toString (pkgs.writeShellScript "emacsrc" ''
              ${self.packages.${system}.default}/bin/e -f -- \
                --eval '(menu-bar-mode)' \
                --eval '(help-with-tutorial)'
            '');
          };
        });

      checks = forLinuxSystems (system: {
        default = import ./test {
          inherit home-manager;
          pkgs = nixpkgsFor.${system};
          module = self.homeManagerModules.default;
        };
      });

      homeManagerModules = {
        default = self.homeManagerModules.wayland;

        xorg = { pkgs, ... }: {
          imports = [
            (import ./nix/home.nix {
              emacsrc = self.packages.${pkgs.system}.emacsrc-xorg;
            })
          ];
        };

        wayland = { pkgs, ... }: {
          imports = [
            (import ./nix/home.nix {
              emacsrc = self.packages.${pkgs.system}.emacsrc-wayland;
            })
          ];
        };
      };

      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.mkShell {
            ENCHANT_CONFIG_DIR = "${self.packages.${system}.default}/share/enchant";
            inputsFrom = builtins.attrValues self.packages.${system};
            buildInputs = self.packages.${system}.default.propagatedUserEnvPkgs;
          };
        });
    };
}

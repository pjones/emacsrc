{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    anki-editor = {
      url = "github:anki-editor/anki-editor";
      flake = false;
    };

    corg = {
      url = "github:isamert/corg.el";
      flake = false;
    };

    meow-edit = {
      url = "github:meow-edit/meow";
      flake = false;
    };

    nextflow-mode = {
      url = "github:edmundmiller/nextflow-mode";
      flake = false;
    };

    org-clock-dbus = {
      url = "github:pjones/org-clock-dbus";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    org-capture-ref = {
      url = "github:yantar92/org-capture-ref";
      flake = false;
    };

    org-grader = {
      url = "github:pjones/org-grader";
      flake = false;
    };

    ox-ipynb = {
      url = "github:jkitchin/ox-ipynb";
      flake = false;
    };

    org-roam = {
      url = "github:org-roam/org-roam/v2.3.0";
      flake = false;
    };

    persid = {
      url = "github:pjones/persid/pjones/compile";
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
            emacs = pkgs.emacs30-gtk3;
          };

          emacsrc-wayland = import ./. {
            inherit pkgs inputs;
            emacs = pkgs.emacs30-pgtk;
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

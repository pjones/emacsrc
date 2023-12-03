{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    passmm = {
      url = "github:pjones/passmm";
      flake = false;
    };

    telega = {
      # Commit from Oct 30, 2023
      url = "github:zevlg/telega.el/5b49be485ae95d33b3e09c5cac4ff7e569a510f2";
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
          emacsrc = import ./. { inherit pkgs inputs; };
          default = self.packages.${system}.emacsrc;
        });

      apps = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        {
          default = {
            type = "app";
            program = toString (pkgs.writeShellScript "emacsrc" ''
              ${self.packages.${system}.emacsrc}/bin/e -f
            '');
          };

          tutorial = {
            type = "app";
            program = toString (pkgs.writeShellScript "emacsrc" ''
              ${self.packages.${system}.emacsrc}/bin/e -f -- \
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

      homeManagerModules.default = { pkgs, ... }: {
        imports = [
          (import ./nix/home.nix {
            emacsrc = self.packages.${pkgs.system}.emacsrc;
          })
        ];
      };

      devShells = forAllSystems (system: {
        default = nixpkgsFor.${system}.mkShell {
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
    };
}

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";

    home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    cape = { url = "github:minad/cape/0.5"; flake = false; };
    corfu = { url = "github:minad/corfu/0.17"; flake = false; };
    corfu-doc = { url = "github:galeo/corfu-doc"; flake = false; };
    neuron-mode = { url = "github:felko/neuron-mode/v1.0"; flake = false; };
    passmm = { url = "github:pjones/passmm"; flake = false; };
    vterm = { url = "github:akermu/emacs-libvterm"; flake = false; };
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

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        {
          emacsrc = import ./. { inherit pkgs inputs; };
        });

      defaultPackage = forAllSystems (system:
        self.packages.${system}.emacsrc);

      apps = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        {
          emacsrc = {
            type = "app";
            program = toString (pkgs.writeShellScript "emacsrc" ''
              ${self.packages.${system}.emacsrc}/bin/e -f
            '');
          };
        });

      defaultApp = forAllSystems (system:
        self.apps.${system}.emacsrc);

      checks.x86_64-linux.emacsrc = import ./test {
        inherit home-manager;
        pkgs = nixpkgsFor.x86_64-linux;
        module = self.homeManagerModule;
      };

      homeManagerModule = { pkgs, ... }: {
        imports = [
          (import ./nix/home.nix {
            emacsrc = self.packages.${pkgs.system}.emacsrc;
          })
        ];
      };
    };
}

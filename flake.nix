{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-parts.url = "github:hercules-ci/flake-parts";

    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    link-hint = {
      url = "github:pjones/link-hint.el/pjones/fix-avy-action";
      flake = false;
    };

    nextflow.url = "github:pjones/nextflow.nix";

    org-clock-dbus = {
      url = "github:pjones/org-clock-dbus";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    org-grader = {
      url = "github:pjones/org-grader";
      flake = false;
    };

    org-inline-image-mode = {
      url = "github:pjones/org-inline-image-mode";
      flake = false;
    };

    ox-ipynb = {
      url = "github:jkitchin/ox-ipynb";
      flake = false;
    };

    xref-project-history = {
      url = "git+https://codeberg.org/imarko/xref-project-history";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      imports = [
        nix/top-level.nix
        nix/home.nix
      ];
    };
}

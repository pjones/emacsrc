{
  self,
  config,
  lib,
  inputs,
  withSystem,
  ...
}:
{
  flake.pkgs = lib.mapAttrs (
    system: config: withSystem system ({ pkgs, ... }: pkgs)
  ) config.allSystems;

  perSystem =
    { pkgs, system, ... }:
    {
      packages.default = self.packages.${system}.emacsrc;

      packages.emacsrc = pkgs.callPackage ./. {
        emacs = self.packages.${system}.emacs;
      };

      packages.emacs = pkgs.callPackage ./emacs-and-packages.nix {
        inherit inputs system;
        emacs = pkgs.emacs-pgtk;
      };

      checks.emacsrc = import ../test {
        inherit self inputs pkgs;
      };

      apps.default = {
        type = "app";
        meta = self.packages.${system}.default.meta;
        program = toString (
          pkgs.writeShellScript "emacsrc" ''
            ${self.packages.${system}.emacs}/bin/emacs \
              --init-directory="${self.packages.${system}.emacsrc}/emacs.d" \
              "$@"
          ''
        );
      };

      apps.tutorial = {
        type = "app";
        meta = self.packages.${system}.default.meta;
        program = toString (
          pkgs.writeShellScript "emacsrc" ''
            ${self.apps.${system}.default.program} \
              --eval '(menu-bar-mode)' \
              --eval '(help-with-tutorial)'
          ''
        );
      };

      devShells.default = pkgs.mkShell {
        ENCHANT_CONFIG_DIR = "${self.packages.${system}.default}/share/enchant";
        inputsFrom = [ self.packages.${system}.default ];
        buildInputs = self.packages.${system}.default.propagatedUserEnvPkgs;
      };
    };
}

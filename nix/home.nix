{ emacsrc
}:
{ pkgs
, lib
, config
, ...
}:
let
  cfg = config.programs.pjones.emacsrc;

in
{
  options.programs.pjones.emacsrc = {
    enable = lib.mkEnableOption "Peter's Emacs Configuration";
  };

  config = lib.mkIf cfg.enable {
    home.stateVersion = lib.mkDefault "22.11";

    home.packages = [
      emacsrc
    ];

    xdg.mimeApps = {
      enable = lib.mkDefault true;

      defaultApplications = {
        "application/pdf" = "emacsclient.desktop";
      };
    };

    xdg = {
      enable = true;

      configFile = {
        "emacs/init.el".source = "${emacsrc}/emacs.d/dot.emacs.el";
        "enchant/enchant.ordering".source = "${emacsrc}/share/enchant/enchant.ordering";
        "enchant/nuspell".source = "${emacsrc}/share/enchant/nuspell";
      };
    };

    home.file = {
      ".local/share/applications/emacsclient.desktop".source =
        "${emacsrc}/share/applications/emacsclient.desktop";
    };
  };
}

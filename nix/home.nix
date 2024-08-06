{ emacsrc
}:
{ pkgs
, lib
, config
, ...
}:
let
  cfg = config.programs.pjones.emacsrc;

  desktopItems = {
    emacsclient = pkgs.makeDesktopItem {
      name = "emacsclient";
      desktopName = "Emacs Client";
      genericName = "Open a new Emacs window";
      icon = "emacs";
      exec = "e -c %u";
    };

    org-protocol = pkgs.makeDesktopItem {
      name = "org-protocol";
      desktopName = "Org Protocol Capture";
      genericName = "Capture passed information via org-protocol";
      icon = "emacs";
      exec = "org-protocol %u";
      mimeTypes = [ "x-scheme-handler/org-protocol" ];
    };
  };
in
{
  options.programs.pjones.emacsrc = {
    enable = lib.mkEnableOption "Peter's Emacs Configuration";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      emacsrc
    ] ++ lib.attrValues desktopItems;

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
  };
}

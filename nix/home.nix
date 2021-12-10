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
    home.packages = [
      emacsrc
      pkgs.dict

      (pkgs.aspellWithDicts (d: [
        d.en
        d.en-computers
        d.en-science
      ]))
    ];

    xdg.mimeApps = {
      enable = lib.mkDefault true;

      defaultApplications = {
        "x-scheme-handler/mailto" = "notmuch.desktop";
        "application/pdf" = "emacsclient.desktop";
      };
    };

    xdg = {
      enable = true;

      configFile = {
        "emacs/init.el".source = "${emacsrc}/emacs.d/dot.emacs.el";
      };
    };

    home.file = {
      ".local/share/applications/notmuch.desktop".source =
        "${emacsrc}/share/applications/notmuch.desktop";

      ".local/share/applications/emacsclient.desktop".source =
        "${emacsrc}/share/applications/emacsclient.desktop";
    };
  };
}

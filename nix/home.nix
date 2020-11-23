{ pkgs
, lib
, ...
}:
let
  emacsrc = import ../. { inherit pkgs; };

in
{
  config = {
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
        "emacs/init.el".source = "${emacsrc}/dot.emacs.el";
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

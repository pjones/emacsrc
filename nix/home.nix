{ self, ... }:
{
  flake.homeModules.emacsrc =
    { pkgs, lib, ... }:
    let
      system = pkgs.stdenv.hostPlatform.system;
      emacsrc = self.packages.${system}.default;

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

        emailto = pkgs.makeDesktopItem {
          name = "emailto";
          desktopName = "Mail to Emacs";
          genericName = "Use Emacs to send mail";
          icon = "emacs";
          exec = "emailto %u";
          mimeTypes = [ "x-scheme-handler/mailto" ];
        };
      };
    in
    {
      config = {
        home.packages = [ emacsrc ] ++ lib.attrValues desktopItems;
        home.sessionVariables.EMACS_SOCKET_NAME = "server";

        xdg = {
          enable = lib.mkDefault true;

          configFile = {
            "emacs/init.el".source = "${emacsrc}/emacs.d/init.el";
            "enchant/enchant.ordering".source = "${emacsrc}/share/enchant/enchant.ordering";
            "enchant/nuspell".source = "${emacsrc}/share/enchant/nuspell";
          };

          mimeApps = {
            enable = lib.mkDefault true;
            defaultApplications = {
              "application/pdf" = "emacsclient.desktop";
            };
          };
        };

        services.emacs = {
          enable = true;
          package = self.packages.${system}.emacs;
          client.enable = false;
          startWithUserSession = "graphical";
        };
      };
    };
}

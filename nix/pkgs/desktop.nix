{ pkgs, ... }:

{
  # List of other files to include:
  imports = [
    ./base.nix
    ./development.nix
  ];

  # Packages for a full user experience on a laptop or workstation.
  packages = with pkgs; [
    # Security:
    pwsafe

    # Terminals and core software:
    wmctrl gnome.gnome_keyring

    # Internet Utilities:
    uzbl chromium conkeror ssvnc firefoxWrapper /* asynk */

    # Media Players/Viewers:
    mpd mpc_cli ncmpcpp vlc mpg123 moc feh zathura

    # Media Ripping:
    handbrake makemkv

    # Media Editors:
    audacity gimp inkscape darktable imagemagick

    # Writing and Designing:
    aspell aspellDicts.en dict
    ghostscript /* FIXME: texLiveFull (broken) */
    gcolor2 impressive libreoffice
    graphviz mscgen xournal

    # Games:
    mednafen

    # Misc.
    tty-clock notify-osd libnotify
  ];
}

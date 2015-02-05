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
    uzbl chromium conkeror ssvnc pypyPackages.googlecl

    # Media Players/Viewers:
    mpd mpc_cli ncmpcpp vlc mpg123 moc feh zathura

    # Media Ripping:
    handbrake /* FIXME: makemkv (404) */

    # Media Editors:
    audacity gimp inkscape darktable

    # Writing and Designing:
    aspell aspellDicts.en dict
    ghostscript texLiveFull
    gcolor2 impressive libreoffice
    graphviz mscgen xournal

    # Games:
    mednafen

    # Misc.
    tty-clock notify-osd libnotify
  ];
}

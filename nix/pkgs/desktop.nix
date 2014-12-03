# Packages for a full user experience on a laptop or workstation.
{ pkgs, ... }:

with pkgs; {
  # Terminals and core software:
  inherit rxvt_unicode_with-plugins;

  # Internet Utilities:
  inherit uzbl chromium;

  # Media Players:
  inherit mpd mpc_cli ncmpcpp vlc mpg123 moc;

  # Media Ripping:
  inherit handbrake makemkv;

  # Helper tools for Xorg:
  xkbcomp = xorg.xkbcomp;
  xev = xorg.xev;

} // import ./base.nix {inherit pkgs;} //
     import ./development.nix {inherit pkgs;}

{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "ivy-exwm";
  version = "0.1";

  src = pkgs.fetchgit {
    url = "git://git.devalot.com/ivy-exwm.git";
    rev = "32f107374aef01b9ae00f1647233d50b4ea659e0";
    sha256 = "1shs1zh8nr2lwxlvrhnhxxjn5g0p21vkjxnjgha1sn07pg7v3iqq";
  };

  recipe = pkgs.writeText "recipe" ''
    (ivy-exwm :repo "pjones/ivy-exwm" :fetcher github)
  '';

  packageRequires = [
    self.exwm
    super.ivy
    super.ivy-rich
  ];
}

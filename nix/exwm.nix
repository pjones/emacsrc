{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "exwm";
  version = "0.19.20180808";

  src = pkgs.fetchFromGitHub {
    owner  = "ch11ng";
    repo   = "exwm";
    rev    = "aebcb0344f18b1aa284a432811175fde2d2feae5";
    sha256 = "0niwbzim029lg71y5rrg607zfiw1zmhk7zcyk5874gbrkfmyr52b";
  };

  packageRequires = [
    self.xelb
  ];
}

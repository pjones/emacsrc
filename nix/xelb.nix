{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "xelb";
  version = "0.15.20180808";

  src = pkgs.fetchFromGitHub {
    owner = "ch11ng";
    repo  = "xelb";
    rev   = "b0ad008f50e3cfe11bbfd496f84a2a1226bebfcb";
    sha256 = "1rd2702kygb8g0kpnnr48in6qywad52b1davigkv5p9wmrvm75jd";
  };

  packageRequires = [
    self.cl-generic
  ];
}

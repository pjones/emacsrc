{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "xelb";
  version = "0.15.20181003";

  src = pkgs.fetchFromGitHub {
    owner = "ch11ng";
    repo  = "xelb";
    rev   = "0494421c858aefbdf544ead22b6037b4877342da";
    sha256 = "037r7lzfcm3q318r6rgi82m0k9v8dm8xxjyh73gyfmfahy3picar";
  };

  packageRequires = [
    self.cl-generic
  ];
}

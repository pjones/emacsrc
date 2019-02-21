{ pkgs
, melpaBuild
, self
, polymode
, ...
}:

melpaBuild {
  pname = "poly-markdown";
  ename = "poly-markdown";
  version = "20181010.1437";
  src = pkgs.fetchFromGitHub {
    owner = "polymode";
    repo = "poly-markdown";
    rev = "bf41bd2f30066573f562c674d38b9e42a43ed016";
    sha256 = "0w2xy1cksik332qs1i26imxiyd89vbfy3ff7di4b3l14cxz6ybra";
  };
  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/3058351c4500fdcbe7f40b4c96ac8d6de9bbeb1d/recipes/poly-markdown";
    sha256 = "0pxai5x2vz6j742s3bpcy82dxja6441fsgclhz1hbv2ykazbm141";
    name = "recipe";
  };
  packageRequires = [ self.melpaPackages.markdown-mode polymode ];
  meta = {
    homepage = "https://melpa.org/#/poly-markdown";
    license = pkgs.lib.licenses.free;
  };
}

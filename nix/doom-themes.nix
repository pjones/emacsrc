{ pkgs
, melpaBuild
, self
, ...
}:

melpaBuild {
  pname = "doom-themes";
  ename = "doom-themes";
  version = "20181219.1820";
  src = pkgs.fetchFromGitHub {
    owner = "hlissner";
    repo = "emacs-doom-themes";
    rev = "2f4a0cdf287a086d45a1d9e8536ace6a2e152318";
    sha256 = "1rvqiyc7i2zzzip3aqv8s3ik9qa4qav04fiyps1bvbsv7flzsfg0";
  };
  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/c5084bc2c3fe378af6ff39d65e40649c6359b7b5/recipes/doom-themes";
    sha256 = "0plqhis9ki3ck1pbv4hiqk4x428fps8qsfx72mamdayyx2nncdrs";
    name = "recipe";
  };
  packageRequires = with self.melpaPackages; [ all-the-icons ];
  meta = {
    homepage = "https://melpa.org/#/doom-themes";
    license = pkgs.lib.licenses.free;
  };
}

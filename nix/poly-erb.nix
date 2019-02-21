{ pkgs
, melpaBuild
, polymode
, ...
}:

melpaBuild {
  pname = "poly-erb";
  ename = "poly-erb";
  version = "20181019.702";
  src = pkgs.fetchFromGitHub {
    owner = "polymode";
    repo = "poly-erb";
    rev = "61fa4640a1cb08120c2c70bfc32029cc79b31b79";
    sha256 = "15k2gmjkn9w5gn7njh8nyr8whhn8xc1hcqqn2as2p1b6m2jh0xcl";
  };
  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/3058351c4500fdcbe7f40b4c96ac8d6de9bbeb1d/recipes/poly-erb";
    sha256 = "01c1z2jll497k1y8835pp54n121y0gkyz1pdxcdjjqv7ia8jwfyy";
    name = "recipe";
  };
  packageRequires = [ polymode ];
  meta = {
    homepage = "https://melpa.org/#/poly-erb";
    license = pkgs.lib.licenses.free;
  };
}

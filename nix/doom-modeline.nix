{ pkgs
, melpaBuild
, self
, shrink-path
, ...
}:

melpaBuild {
  pname = "doom-modeline";
  ename = "doom-modeline";
  version = "20190103.535";
  src = pkgs.fetchFromGitHub {
    owner = "seagle0128";
    repo = "doom-modeline";
    rev = "804167cf5a05f0b0332fc9bdb8275cefb76622f2";
    sha256 = "15mqn38w6x2wamwp0llg5m9j57cnhm0mzczxp68ni74dwksgrgk7";
  };
  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/f4f610757f85fb01bd9b1dd212ddbea8f34f3ecd/recipes/doom-modeline";
    sha256 = "0pscrhhgk4wpz1f2r94ficgan4f9blbhqzvav1wjahwp7fn5m29j";
    name = "recipe";
  };
  packageRequires = with self.melpaPackages;
    [ all-the-icons dash eldoc-eval ] ++
    [ shrink-path ];
  meta = {
    homepage = "https://melpa.org/#/doom-modeline";
    license = pkgs.lib.licenses.free;
  };
}

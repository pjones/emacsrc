{ pkgs
, melpaBuild
, ...
}:

melpaBuild {
  pname = "shrink-path";
  ename = "shrink-path";
  version = "20170812.1947";
  src = pkgs.fetchurl {
    url = "https://gitlab.com/bennya/shrink-path.el/-/archive/a94c80743280fe317cf56cd4d4cd6385ce9e3dfb/shrink-path.el-a94c80743280fe317cf56cd4d4cd6385ce9e3dfb.tar.gz";
    sha256 = "12hqw3ly2kh1wjcg8gs9jfq28z4g9bfwgh97gzhyhrp9d6ssmwq9";
  };
  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/86b0d105e8a57d5f0bcde779441dc80b85e170ea/recipes/shrink-path";
    sha256 = "0fq13c6g7qbq6f2ry9dzdyg1f6p41wimkjcdaj177rnilz77alzb";
    name = "recipe";
  };
  packageRequires = [  ];
}

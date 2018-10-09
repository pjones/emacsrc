{ pkgs
, melpaBuild
, ...
}:

melpaBuild {
  pname = "mu4e-query-fragments";
  version = "20170923.622";

  src = pkgs.fetchurl {
    url = "https://gitlab.com/wavexx/mu4e-query-fragments.el/-/archive/5f2b195dad2d74f38ff35b93edea5dd133112012/mu4e-query-fragments.el-5f2b195dad2d74f38ff35b93edea5dd133112012.tar.gz";
    sha256 = "1nkqcpdi8g0kprrmsws7ic5qlha7159xj2d7bfx4r0zlqc1x29f7";
  };

  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/c1cf98dff029d494007fe25d29bd8bcfecc5b8e6/recipes/mu4e-query-fragments";
    sha256 = "1gckwfgw7jvr6dbikcmy07i07wjhlvq66swhac2laaj6w567vc7w";
    name = "recipe";
  };

  packageRequires = [];
}

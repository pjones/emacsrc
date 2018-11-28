{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "passmm";
  version = "20180624.0";

  src = pkgs.fetchgit {
    url = "git://git.devalot.com/passmm.git";
    rev = "a464a3450edff3cb9a20aa763b437d6d339168ef";
    sha256 = "1qk0glz8c3k55ckflgp5rs41yanjsald3cwpxvabp86p3lbvyqi4";
  };

  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/8ae2a1e10375f9cd55d19502c9740b2737eba209/recipes/passmm";
    sha256 = "0p6qps9ww7s6w5x7p6ha26xj540pk4bjkr629lcicrvnfr5jsg4b";
    name = "passmm";
  };

  packageRequires = [ self.password-store ];
}

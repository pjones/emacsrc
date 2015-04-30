let
  pkgs   = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in rec {
  cltc-phoenix = stdenv.mkDerivation rec {
    name = "cltc-claims";
    version = "0.0";
    src = ./.;

    buildInputs = with pkgs; [
      ruby_2_1 bundler git mysql zlib
    ];

    shellHook = ''
      bundle config --local build.mysql2     \
        --with-zlib=${pkgs.zlib} \
        --with-mysqlclientlib=${pkgs.mysql} \
        --with-mysql-dir=${pkgs.mysql} > /dev/null

      bundle install --path vendor/bundle
    '';
  };
}

let
  pkgs   = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  ruby   = pkgs.ruby_1_9_3;
in rec {
  cltc-phoenix = stdenv.mkDerivation rec {
    name = "cltc-phoenix";
    version = "0.0";
    src = ./.;

    buildInputs = with pkgs; [
      ruby bundler git libxml2 libxslt libffi pkgconfig mysql
      inetutils /* for hostname(1) */
    ];

    shellHook = ''
      bundle config --local build.nokogiri                  \
        --use-system-libraries                              \
        --with-xml2-lib=${pkgs.libxml2}/lib                 \
        --with-xml2-include=${pkgs.libxml2}/include/libxml2 \
        --with-xslt-lib=${pkgs.libxslt}/lib                 \
        --with-xslt-include=${pkgs.libxslt}/include > /dev/null

      bundle config --local build.mysql     \
        --with-mysqlclientlib=${pkgs.mysql} \
        --with-mysql-dir=${pkgs.mysql} > /dev/null

      bundle install --path vendor/bundle

      if [ ! -r config/database.yml ]; then
        grep -v password config/database.yml.sample > config/database.yml

        mysqladmin -u root -f drop cltc_phoenix_development
        mysqladmin -u root create  cltc_phoenix_development
        mysqladmin -u root -f drop cltc_phoenix_test
        mysqladmin -u root create  cltc_phoenix_test

        ${ruby}/bin/rake db:schema:load
        ${ruby}/bin/rake db:test:clone
        env RAILS_ENV=test ${ruby}/bin/rake db:fixtures:load
      fi
    '';
  };
}

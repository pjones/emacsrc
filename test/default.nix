# nix flake check --print-build-logs
#
# nix build .\#checks.x86_64-linux.default.driverInteractive
# ./result/bin/nixos-test-driver
{ pkgs
, home-manager
, module
}:
let
  tests = pkgs.stdenvNoCC.mkDerivation {
    name = "emacsrc-test-files";
    phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
    src = ./.;

    installPhase = ''
      mkdir -p "$out/bin" "$out/share"
      install -m 0555 runner.sh "$out/bin/emacsrc-test-runner.sh"
      install -m 0444 assertions.el "$out/share/assertions.el"
    '';
  };
in
pkgs.nixosTest {
  name = "test-emacsrc";

  nodes.emacsrc = { ... }: {
    imports = [ home-manager.nixosModules.home-manager ];

    users.users.pjones = {
      createHome = true;
      isNormalUser = true;
      password = "password";
      group = "users";
    };

    home-manager = {
      # Don't load nixpkgs from the environment:
      useGlobalPkgs = true;

      users.pjones = { ... }: {
        imports = [ module ];
        programs.pjones.emacsrc.enable = true;
        home.packages = [ tests ];
      };
    };
  };

  testScript =
    let home = "/home/pjones";
    in
    ''
      # Boot
      start_all()
      emacsrc.wait_for_unit("multi-user.target")
      emacsrc.wait_for_unit("home-manager-pjones.service")
      emacsrc.wait_for_unit("getty@tty1.service")

      # Login
      emacsrc.wait_until_tty_matches("1", "login: ")
      emacsrc.send_chars("pjones\n")
      emacsrc.wait_until_tty_matches("1", "Password: ")
      emacsrc.send_chars("password\n")
      emacsrc.wait_until_tty_matches("1", "pjones@emacsrc:")

      # Run the tests:
      emacsrc.send_chars("emacsrc-test-runner.sh\n")
      emacsrc.succeed("sleep 1")  # Wait for script to start.
      emacsrc.wait_until_fails("pgrep --uid pjones -f emacsrc-test-runner")
      print(emacsrc.succeed("cat ${home}/log"))
      emacsrc.succeed("test -e ${home}/PASSED")
    '';
}

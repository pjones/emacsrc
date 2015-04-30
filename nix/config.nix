{
  allowUnfree = true;

  # Where to get binaries from:
  extra-binary-caches = [
    http://hydra.nixos.org
    http://hydra.cryp.to
  ];

  chromium = {
    icedtea = true;
  };

  firefox = {
    icedtea = true;
    enableAdobeFlash = true;
  };
}

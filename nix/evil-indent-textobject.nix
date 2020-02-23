{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "evil-indent-textobject";
  version = "20200223.0";

  src = pkgs.fetchgit {
    url = "https://code.devalot.com/mirrors/evil-indent-textobject.git";
    rev = "350246086fd1aad574c67b1df341bf2e9168c5ed";
    sha256 = "0c5kmjg8pbhhn37sj11vgihh8zva1f521y7irxgxba2x3whyz8ln";
  };

  recipe = pkgs.writeText "recipe" ''
    (evil-indent-textobject
      :fetcher git
      :url "https://code.devalot.com/mirrors/evil-indent-textobject.git")
  '';

  packageRequires = [ self.evil ];
}

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    frontend = ./frontend;
    common = ./common;
  };

  shells = {
    ghc = ["common" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})

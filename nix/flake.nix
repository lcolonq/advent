{
  description = "aoc";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    ps-tools.follows = "purs-nix/ps-tools";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      ps-tools = inputs.ps-tools.legacyPackages.${system};
      purs-nix = inputs.purs-nix { inherit system; };
      runtime = [
        pkgs.xorg.libX11
        pkgs.xorg.libXcursor
        pkgs.xorg.libXi
        pkgs.xorg.libXrandr
        pkgs.xorg.libXtst
        pkgs.xorg.libXinerama
        pkgs.libglvnd
      ];
      purescript = purs-nix.purs {
        dependencies = [
          "console"
          "effect"
          "prelude"
          "random"
          "refs"
          "node-fs"
          "debug"
        ];
        dir = ./../2024/19;
        srcs = [ "src" ];
      };
      hy-python = pkgs.python3.withPackages (ppkgs: [
        ppkgs.networkx
        ppkgs.hy
      ]);
      haskellPackages = pkgs.haskell.packages.ghc94;
      ghc = haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
        base
        base64
        binary
        bytestring
        containers
        directory
        errors
        filepath
        lens
        megaparsec
        mtl
        safe-exceptions
        text
        time
        transformers
        unordered-containers
        vector
      ]);
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = runtime ++ [
          pkgs.cbqn
          pkgs.uiua
          pkgs.j
          pkgs.cmake
          pkgs.glfw
          pkgs.idris2
          pkgs.vim
          pkgs.factor-lang
          pkgs.leiningen
          pkgs.lua54Packages.fennel
          pkgs.guile
          pkgs.jdk23
          pkgs.maven
          (purescript.command {})
          ps-tools.for-0_15.purescript-language-server
          purs-nix.purescript
          hy-python
          pkgs.rust-analyzer
          pkgs.rustc
          pkgs.cargo
          pkgs.sbcl
          ghc
          haskellPackages.haskell-language-server
        ];
        FACTOR_ROOT = "${pkgs.factor-lang}";
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath runtime;
      };
    };
}

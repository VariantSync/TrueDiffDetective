{
  system ? builtins.currentSystem,
  pkgs ?
    import (builtins.fetchTarball {
      name = "sources";
      url = "https://github.com/nixos/nixpkgs/archive/6832d0d99649db3d65a0e15fa51471537b2c56a6.tar.gz";
      sha256 = "1ww2vrgn8xrznssbd05hdlr3d4br6wbjlqprys1al8ahxkyl5syi";
    }) {
      inherit system;
      config = {};
      modules = [];
    },
  lib ? pkgs.lib,
  callPackage ? pkgs.callPackage,
  symlinkJoin ? pkgs.symlinkJoin,
  sbt ?
    pkgs.sbt.override {
      jre = pkgs.jdk17;
    },
  buildSbtPackage ?
    callPackage (import ./nix/buildSbtPackage.nix) {
      inherit sbt;
    },
  truediff ?
    callPackage (import ./nix/truediff.nix) {
      inherit buildSbtPackage;
    },
  DiffDetective ?
    import (builtins.fetchTarball {
      name = "DiffDetective";
      url = "https://github.com/VariantSync/DiffDetective/archive/07756a1b6c3b89049ba04d4980a220c499fc7f36.tar.gz";
      sha256 = "1gl1q63dg392a4ihhkwvcmzqg7x6sh5cjhg4c5fadbyg0zhqag6n";
    }) {
      inherit system pkgs;
    },
  dependenciesHash ? "sha256-/TAPDh2eX2IveF5bufkh3OAIcl2SDmTS4INH8fslbSo=",
}:
buildSbtPackage {
  pname = "TrueDiffDetective";
  # The version is duplicated in `build.sbt`.
  version = "0.1.0-SNAPSHOT";
  src = with lib.fileset;
    toSource {
      root = ./.;
      fileset = gitTracked ./.;
    };

  mavenRepo = symlinkJoin {
    name = "TrueDiffDetective-maven-dependencies";
    paths = [
      DiffDetective.maven
      truediff.maven
    ];
  };

  inherit dependenciesHash;
}

{
  lib,
  stdenvNoCC,
  sbt,
  maven,
}: {
  pname,
  version,
  src,
  dependenciesHash ? lib.fakeHash,
  mavenRepo ? null,
}:
stdenvNoCC.mkDerivation {
  inherit pname;
  inherit version;
  inherit src;

  outputs = ["out" "maven"];

  nativeBuildInputs = [
    sbt
  ];

  fetchedDependencies = stdenvNoCC.mkDerivation {
    pname = "${pname}-dependencies";
    inherit version;
    inherit src;

    nativeBuildInputs = [
      sbt
      maven
    ];

    # According to the wisdom of the internet
    # (multiple issues, stack overflow etc., who mentioned this command along the way, but there is no actually useful documentation what that command should do),
    # `sbt update` should be enough to download all necessary dependencies to build offline.
    # However, it doesn't. Hence, we have to also do a fake compilation.
    # In order to actually cache all depencies [1] without build artifacts
    # (this could cause annoying reproducibility bugs caused by previous build outputs),
    # we only add an empty file such that `sbt` "builds" without complains.
    #
    # [1]: https://stackoverflow.com/questions/52355642/sbt-compile-compiler-bridge#comment107617368_52430243
    buildPhase = ''
      runHook preBuild

      # Find source directories
      source_directories=($(find . -path "*/src/main/scala"))

      # Delete all files except `build.sbt` and the `project/` directory to keep
      # dependency fetching independet of other source changes.
      find . -mindepth 1 -maxdepth 1 -not \( -name "build.sbt" -o -name "project" \) -exec rm -r {} +

      # Add an empty file into each source directory such that sbt actually
      # downloads all necessary dependencies.
      for source_directory in "''${source_directories[@]}"
      do
        mkdir -p "$source_directory"
        touch "$source_directory/empty.scala"
      done

      mkdir cache
      ${
        lib.optionalString (mavenRepo != null) ''
          cp -L -r ${mavenRepo} cache/maven-repo
          chmod u+w -R cache/maven-repo
        ''
      }

      # Download necessary dependencies and tell sbt to cache them in a `cache/`
      # directory.
      sbt -Dsbt.home=cache/sbt -Dsbt.boot.directory=cache/sbt-boot -Dsbt.coursier.home=cache/coursier -Dsbt.ivy.home=cache/ivy -Dmaven.repo.local=cache/maven-repo update compile

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      cp -r cache "$out"

      if [ -d "$out/maven-repo" ]
      then
        # Keep only *.{pom,jar,sha1,nbm} and delete all ephemeral files with lastModified timestamps inside.
        find "$out/maven-repo" -type f \
          \( -not \( -name "*.pom" -o -name "*.jar" -o -name "*.sha1" -o -name "*.nbm" \) \
              -o -name "maven-metadata*" \) \
          -delete
      fi

      if [ -d "$out/ivy" ]
      then
        # Delete unnecessary files with impurities (timestamps).
        find "$out/ivy" \
          -type f \
          -name "ivydata*.properties" \
          -delete
        # Override all other timestamps with a fixed value.
        find "$out/ivy" \
          -type f \
          -name "*.xml" \
          -exec sed -i 's/publication="[[:digit:]]*"/publication="19700101000001"/' -- {} +
      fi

      runHook postInstall
    '';

    dontFixup = true;
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = dependenciesHash;
  };

  buildPhase = ''
    runHook preBuild

    # sbt needs write access to the cache.
    cp -r "$fetchedDependencies" cache
    chmod u+w -R cache

    sbt -Dsbt.home=cache/sbt -Dsbt.boot.directory=cache/sbt-boot -Dsbt.coursier.home=cache/coursier -Dsbt.ivy.home=cache/ivy -Dmaven.repo.local=cache/maven-repo compile test package publishM2

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/share/java"
    find . -name cache -prune -o -name "*.jar" -exec cp -t "$out/share/java" {} +

    mv cache/maven-repo "$maven"

    # Keep only *.{pom,jar,sha1,nbm} and delete all ephemeral files with lastModified timestamps inside.
    find "$maven" -type f \
      \( -not \( -name "*.pom" -o -name "*.jar" -o -name "*.sha1" -o -name "*.nbm" \) \
          -o -name "maven-metadata*" \) \
      -delete

    runHook postInstall
  '';
}

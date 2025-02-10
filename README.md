# TrueDiffDetective
TrueDiffDetective extends [DiffDetective](https://github.com/VariantSync/DiffDetective) by the option to chose [truediff](https://gitlab.rlp.net/plmz/truediff) as diffing algorithm.
It is (mainly) implemented in Scala and using sbt as build tool (like truediff), but can easily be used in Java/Maven projects as well.

## Nix Setup
You can obtain a local maven repository with TrueDiffDetective and all necessary
dependencies by running `nix-build -A maven`. It can be used by passing
`-Dmaven.repo.local=result` to `sbt` or `mvn`.

Alternatively, the plain TrueDiffDetective jars can be build by running
`nix-build`. The jar can be installed into your Maven cache using something like
```bash
mvn install:install-file -DgroupId=org.variantsync -DartifactId=TrueDiffDetective -Dversion=0.1.0-SNAPSHOT -Dpackaging=jar -Dfile=./result/share/java/truediffdetective_2.13-0.1.0-SNAPSHOT-javadoc.jar
```

## Manual Setup
### Requirements and Dependencies
- Scala 2 and sbt (Scala 3 will only work with some modifications)
- Java 17 and Maven (for DiffDetective)
- DiffDetective (see https://github.com/VariantSync/DiffDetective)
- truediff (see https://gitlab.rlp.net/plmz/truediff)

### Install Dependencies
DiffDetective and truediff are required in the local Maven repository. 

To install DiffDetective using Maven:
```shell
    git clone https://github.com/VariantSync/DiffDetective.git
    cd DiffDetective
    mvn clean install
```
Be sure to actually (re-)compile DiffDetective with version 17 of Java.
In case of doubt or obscure errors later on, try removing the maven cache with `rm -rf ~/.m2` before installing DiffDetective.
    
To install truediff using sbt:
```shell
    git clone https://gitlab.rlp.net/plmz/truediff.git
    cd truediff
    sbt publishM2
```

### Install TrueDiffDetective
To install truediff using sbt:
```shell
    git clone https://github.com/VariantSync/TrueDiffDetective.git
    cd TrueDiffDetective
    sbt publishM2
```

## Using TrueDiffDetective
To use TrueDiffDetective in a sbt project add dependency:
```
    "org.variantsync" % "truediffdetective_2.13" % "0.1.0-SNAPSHOT"
```

To use TrueDiffDetective in a Maven project add dependency:
```
    <dependency>
      <groupId>org.variantsync</groupId>
      <artifactId>truediffdetective_2.13</artifactId>
      <version>0.1.0-SNAPSHOT</version>
    </dependency>
```

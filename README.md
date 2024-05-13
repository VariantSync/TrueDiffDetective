# TrueDiffDetective
TrueDiffDetective extends [DiffDetective](https://github.com/VariantSync/DiffDetective) by the option to chose [truediff](https://gitlab.rlp.net/plmz/truediff) as diffing algorithm.
It is (mainly) implemented in Scala and using sbt as build tool (like truediff), but can easily be used in Java/Maven projects as well.

## Setup
### Requirements and Dependencies
- Scala 2 and sbt (Scala 3 will only work with some modifications)
- Java 17 and Maven (for DiffDetective)
- DiffDetective (see https://github.com/VariantSync/DiffDetective)
- truediff (see https://gitlab.rlp.net/plmz/truediff)

### Install Dependencies
DiffDetective and truediff are required in the local Maven repository. 

To install DiffDetective using Maven:

    git clone https://github.com/VariantSync/DiffDetective.git
    cd DiffDetective
    mvn install
    
To install truediff using sbt:

    git clone https://gitlab.rlp.net/plmz/truediff.git
    cd truediff
    sbt publishM2

### Install TrueDiffDetective
To install truediff using sbt:

    git clone https://github.com/VariantSync/TrueDiffDetective.git
    cd TrueDiffDetective
    sbt publishM2

### Using TrueDiffDetective
To use TrueDiffDetective in a sbt project add dependency:

    "org.variantsync" % "truediffdetective_2.13" % "0.1.0-SNAPSHOT"

To use TrueDiffDetective in a Maven project add dependency:

    <dependency>
      <groupId>org.variantsync</groupId>
      <artifactId>truediffdetective_2.13</artifactId>
      <version>0.1.0-SNAPSHOT</version>
    </dependency>
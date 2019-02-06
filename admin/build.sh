#!/bin/bash

set -e

# Builds of tagged revisions are published to sonatype staging.

# Travis runs a build on new revisions and on new tags, so a tagged revision is built twice.
# Builds for a tag have TRAVIS_TAG defined, which we use for identifying tagged builds.
# Checking the local git clone would not work because git on travis does not fetch tags.

# The version number to be published is extracted from the tag, e.g., v1.2.3 publishes
# version 1.2.3 using all Scala versions in build.sbt's `crossScalaVersions`.

# When a new, binary incompatible Scala version becomes available, a previously released version
# can be released using that new Scala version by creating a new tag containing the Scala and the
# JVM version after hashes, e.g., v1.2.3#2.13.0-M1#8. The JVM version needs to be listed in
# `.travis.yml`, otherwise the required build doesn't run.

verPat="[0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9-]+)?"
tagPat="^v$verPat(#$verPat#[0-9]+)?$"

publish=no

if [[ "$TRAVIS_TAG" =~ $tagPat ]]; then
  currentJvmVer=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}' | sed 's/^1\.//' | sed 's/[^0-9].*//')

  tagVer=$(echo $TRAVIS_TAG | sed s/#.*// | sed s/^v//)
  publishVersion='set every version := "'$tagVer'"'

  scalaAndJvmVer=$(echo $TRAVIS_TAG | sed s/[^#]*// | sed s/^#//)
  if [ "$scalaAndJvmVer" != "" ]; then
    scalaVer=$(echo $scalaAndJvmVer | sed s/#.*//)
    jvmVer=$(echo $scalaAndJvmVer | sed s/[^#]*// | sed s/^#//)
    if [ "$jvmVer" != "$currentJvmVer" ]; then
      echo "Not publishing $TRAVIS_TAG on Java version $currentJvmVer."
      exit 0
    fi
    publishScalaVersion='set every ScalaModulePlugin.scalaVersionsByJvm := Map('$jvmVer' -> List("'$scalaVer'" -> true))'
    echo "Releasing $tagVer using Scala $scalaVer on Java version $jvmVer."
  else
    echo "Releasing $tagVer on Java version $currentJvmVer according to 'scalaVersionsByJvm' in build.sbt."
  fi

  publish=yes
elif [[ "$TRAVIS_BRANCH" = master ]]; then
  publish=yes
fi

if [[ "$publish" = yes ]]; then
  extraTarget="+publish-signed"
  cat admin/gpg.sbt >> project/plugins.sbt
  cp admin/publish-settings.sbt .

  # Copied from the output of genKeyPair.sh
  K=$encrypted_97ebac4c5d62_key
  IV=$encrypted_97ebac4c5d62_iv

  openssl aes-256-cbc -K $K -iv $IV -in admin/secring.asc.enc -out admin/secring.asc -d
fi

sbt "$publishVersion" "$publishScalaVersion" clean update +test +publishLocal $extraTarget

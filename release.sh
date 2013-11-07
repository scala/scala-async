#! /bin/bash -e
#
# Build, test, and release Scala Async.
#
# Requires credentials:
#
#   % cat ~/.sbt/0.13/publish.sbt
#   credentials += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", "<user>", "<pass>")
#
# Also requires the sbt-pgp plugin installed globally to provide the `publishSigned` command.
#
#   % cat ~/.sbt/0.13/plugins/gpg.sbt
#   addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

function sbt211() {
  sbt 'set scalaVersion := "2.11.0-M6"' 'set scalaBinaryVersion := scalaVersion.value' $@
  return $?
}
die () {
  echo "$@"
  exit 1
}

CHECK=";clean;test;publishLocal"
RELEASE=";clean;test;publishSigned"
VERSION=`gsed -rn 's/version :=.*"(.+).*"/\1/p' build.sbt`
[[ -n "$(git status --porcelain)" ]] && die "working directory is not clean!"

sbt211 $CHECK
sbt $CHECK
sbt $RELEASE
sbt211 $RELEASE

cat <<EOM
Released! For non-snapshot releases:
 - tag: git tag -s -a v$VERSION -m "scala-async $VERSION"
 - push tag: git push origin v$VERSION
 - close and release the staging repository: https://oss.sonatype.org
 - change the version number in build.sbt to a suitable -SNAPSHOT version
EOM
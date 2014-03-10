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

die () {
  echo "$@"
  exit 1
}

CHECK=";clean;test;publishLocal"
RELEASE=";clean;test;publishSigned"
VERSION=`gsed -rn 's/version :=.*"(.+).*"/\1/p' build.sbt`
[[ -n "$(git status --porcelain)" ]] && die "working directory is not clean!"

sbt $CHECK
sbt $RELEASE

cat <<EOM
Released! For non-snapshot releases:
 - tag: git tag -s -a v$VERSION_2.10 -m "scala-async $VERSION for Scala 2.10"
 - push tag: git push origin v$VERSION_2.10
 - close and release the staging repository: https://oss.sonatype.org
 - change the version number in build.sbt to a suitable -SNAPSHOT version
EOM
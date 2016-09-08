#!/bin/bash

# prep environment for publish to sonatype staging if the HEAD commit is tagged

# git on travis does not fetch tags, but we have TRAVIS_TAG
# headTag=$(git describe --exact-match ||:)

if [ "$IS_PUBLISH_JDK" == "true" ] && [[ "$TRAVIS_TAG" =~ ^v[0-9]+\.[0-9]+\.[0-9]+(-[A-Za-z0-9-]+)? ]]; then
  echo "Going to release from tag $TRAVIS_TAG!"
  myVer=$(echo $TRAVIS_TAG | sed -e s/^v// | sed -e 's/_[0-9]*\.[0-9]*//')
  publishVersion='set every version := "'$myVer'"'
  extraTarget="publish-signed"

  cat admin/gpg.sbt >> project/plugins.sbt
  admin/decrypt.sh sensitive.sbt
  (cd admin/ && ./decrypt.sh secring.asc)
fi

sbt ++$TRAVIS_SCALA_VERSION "$publishVersion" clean update compile test $extraTarget

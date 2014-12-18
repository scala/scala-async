#!/bin/bash

# Based on https://gist.github.com/kzap/5819745:

echo "This will encrypt the cleartext sensitive.sbt and admin/secring.asc, while making the encrypted versions available for decryption on Travis."
echo "Update your .travis.yml as directed, and delete the cleartext versions."
echo "Press enter to continue."
read

# 1. create a secret, put it in an environment variable while encrypting files -- UNSET IT AFTER
export SECRET=$(cat /dev/urandom | head -c 10000 | openssl sha1)

# 2. add the "secure: ..." line under the env section -- generate it with ``  (install the travis gem first)
travis encrypt SECRET=$SECRET

admin/encrypt.sh admin/secring.asc
admin/encrypt.sh sensitive.sbt

echo "Remember to rm sensitive.sbt admin/secring.asc -- once you do, they cannot be recovered (except on Travis)!"
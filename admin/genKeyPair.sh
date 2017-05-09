#!/bin/bash
#
# Generates a key pair for this repository to sign artifacts.
# Encrypt the private key and its passphrase in trusted builds
# on Travis CI.
#
set -e

# Based on https://gist.github.com/kzap/5819745:
function promptDelete() {
    if [[ -f "$1" ]]; then
        echo About to delete $1, Enter for okay / CTRL-C to cancel
        read
        rm "$1"
    fi
}
for f in admin/secring.asc.enc admin/secring.asc admin/pubring.asc; do promptDelete "$f"; done

echo Generating key pair. Please enter 1. repo name 2. scala-internals@googlegroups.com, 3. a new passphrase
echo Be careful when using special characters in the passphrase, see http://docs.travis-ci.com/user/encryption-keys/#Note-on-escaping-certain-symbols
cp admin/gpg.sbt project
sbt 'set pgpReadOnly := false' \
    'set pgpPublicRing := file("admin/pubring.asc")' \
    'set pgpSecretRing := file("admin/secring.asc")' \
    'pgp-cmd gen-key'
rm project/gpg.sbt

echo ============================================================================================
echo Encrypting admin/secring.asc. Update K and IV variables in admin/build.sh accordingly.
echo ============================================================================================
travis encrypt-file admin/secring.asc
rm admin/secring.asc
mv secring.asc.enc admin

echo ============================================================================================
echo Encrypting environment variables. Add each to a line in .travis.yml. Include a comment
echo with the name of the corresponding variable
echo ============================================================================================
read -s -p 'PGP_PASSPHRASE: ' PGP_PASSPHRASE
travis encrypt PGP_PASSPHRASE="$PGP_PASSPHRASE"


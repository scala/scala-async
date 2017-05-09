#!/bin/bash
#
# Encrypt sonatype credentials so that they can be
# decrypted in trusted builds on Travis CI.
#
set -e

read -s -p 'SONA_USER: ' SONA_USER
travis encrypt SONA_USER="$SONA_USER"
read -s -p 'SONA_PASS: ' SONA_PASS
travis encrypt SONA_PASS="$SONA_PASS"

#!/bin/bash
openssl aes-256-cbc -pass "pass:$SECRET" -in $1.enc -out $1 -d -a
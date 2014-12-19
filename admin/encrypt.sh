#!/bin/bash
openssl aes-256-cbc -pass "pass:$SECRET" -in $1 -out $1.enc -a
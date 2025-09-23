#!/bin/sh

if [ ! -e server.crt ]; then
    openssl req -x509 -newkey rsa:4096 -keyout server.key -out server.crt -days 3650 -nodes -batch
fi

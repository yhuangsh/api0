#!/bin/sh

docker network create \
    --driver=bridge \
    --subnet=172.28.0.0/24 \
    --ip-range=172.28.0.0/24 \
    --gateway=172.28.0.1 \
    dev-net
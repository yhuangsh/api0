#!/bin/sh

docker network create \
    --driver=bridge \
    --subnet=172.29.0.0/24 \
    --ip-range=172.29.0.0/24 \
    --gateway=172.29.0.1 \
    api0-net
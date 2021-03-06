#!/bin/sh
docker run \
    --rm \
    --net dev-net \
    --name api0-1 \
    --ip 172.28.0.13 \
    --hostname api0-1.api0.default.svc.cluster.local \
    --add-host api0-0.api0.default.svc.cluster.local:172.28.0.12 \
    --add-host api0-1.api0.default.svc.cluster.local:172.28.0.13 \
    --add-host api0-2.api0.default.svc.cluster.local:172.28.0.14 \
    -v `pwd`:/project \
    -p 8001:8000 \
    -it \
    yhuangsh/dev-alpine-erlang:latest
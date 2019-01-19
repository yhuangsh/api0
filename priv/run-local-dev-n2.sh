#!/bin/sh
docker run \
    --rm \
    --net dev-net \
    --name api0-2 \
    --ip 172.28.0.12 \
    --hostname api0-2.api0.default.svc.cluster.local \
    --add-host api0-0.api0.default.svc.cluster.local:172.29.0.2 \
    --add-host api0-1.api0.default.svc.cluster.local:172.29.0.3 \
    --add-host api0-2.api0.default.svc.cluster.local:172.29.0.4 \
    -v `pwd`:/project \
    -p 8002:8000 \
    -it \
    yhuangsh/dev-alpine-erlang:latest
#!/bin/sh
docker run \
    --rm \
    --net api0-net \
    --name api0-1 \
    --ip 172.29.0.3 \
    --hostname api0-1.api0.default.svc.cluster.local \
    --add-host api0-0.api0.default.svc.cluster.local:172.29.0.2 \
    --add-host api0-1.api0.default.svc.cluster.local:172.29.0.3 \
    --add-host api0-2.api0.default.svc.cluster.local:172.29.0.4 \
    -v `pwd`:/project \
    -p 8001:8000 \
    -it \
    yhuangsh/dev-alpine-erlang:latest
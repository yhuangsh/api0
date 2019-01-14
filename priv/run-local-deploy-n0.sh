#!/bin/sh
docker run \
    --rm \
    --net api0-net \
    --name api0-0 \
    --ip 172.29.0.2 \
    --hostname api0-0.api0.default.svc.cluster.local \
    --add-host api0-0.api0.default.svc.cluster.local:172.29.0.2 \
    --add-host api0-1.api0.default.svc.cluster.local:172.29.0.3 \
    --add-host api0-2.api0.default.svc.cluster.local:172.29.0.4 \
    -p 8000:8000 \
    -it yhuangsh/api0-dev-build:latest \
    /deploy/api0/bin/api0 console
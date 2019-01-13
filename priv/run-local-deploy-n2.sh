#!/bin/sh

#!/bin/sh
docker run \
    --rm \
    --net api0-net \
    --name api0-2 \
    --ip 172.29.0.4 \
    --hostname api0-2.api0.default.svc.cluster.local \
    --add-host api0-0.api0.default.svc.cluster.local:172.28.0.2 \
    --add-host api0-1.api0.default.svc.cluster.local:172.28.0.3 \
    --add-host api0-2.api0.default.svc.cluster.local:172.28.0.4 \
    -p 8002:8000 \
    -it yhuangsh/api0-dev-build:latest \
    /deploy/api0/bin/api0 console
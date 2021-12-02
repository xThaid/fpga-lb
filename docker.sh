#!/bin/bash

IMAGE_NAME=fpga_lb_env:1.0
CONTAINER_NAME=fpga_lb_env

LOCAL_WORKDIR=~/fpga/workdir/fpga-lb
TARGET_WORKDIR=/home/user/workdir

create() {
    docker create \
        --name $CONTAINER_NAME \
        --hostname $CONTAINER_NAME \
        -i \
        -t \
        -v $LOCAL_WORKDIR:$TARGET_WORKDIR \
        -w "$TARGET_DIR" \
        $IMAGE_NAME
}

start() {
    docker start $CONTAINER_NAME
}

run() {
    if [ $# -eq 1 ]; then
        args="bash"
    else
        args="${@:2}"
    fi
    
    start
    docker exec -i -t -w "$TARGET_WORKDIR" $CONTAINER_NAME $args
}

stop() {
    docker stop $CONTAINER_NAME
}

destroy() {
    stop
    docker rm $CONTAINER_NAME
}

case "$1" in
    create)
        create "$@"
        ;;
    start)
        start "$@"
        ;;
    sh)
        run "$@"
        ;;
    stop)
        stop "$@"
        ;;
    destroy)
        destroy "$@"
        ;;
    *)
        echo "Usage: $0 create|start|sh|stop|destroy"; exit 1;
        ;;
esac

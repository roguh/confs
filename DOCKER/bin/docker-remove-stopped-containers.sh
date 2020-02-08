#!/bin/sh
# Should throw warning about being unable to remove running container
docker rm $(docker ps -a -q)

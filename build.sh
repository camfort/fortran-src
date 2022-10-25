#!/bin/bash

docker build -f Dockerfile.openai-python -t openai:python3.10 \
  --build-arg USER_ID=$(id -u) \
  --build-arg GROUP_ID=$(id -g) \
  .

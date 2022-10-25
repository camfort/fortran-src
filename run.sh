#!/bin/bash

docker run -i --rm \
  -e OPENAI_API_KEY="$OPENAI_API_KEY" \
  -w /work \
  -v "$PWD":/work \
  openai:python3.10 "$@"

#!/bin/bash
mkdir -p res
for f in src/sql/*.sql; do
  zip "res/$(basename "$f" .sql).zip" "$f"
done

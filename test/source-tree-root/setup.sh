#!/bin/bash
TEST_DIR="$(pwd)/$(mktemp -d .tmp.XXXXXXXXX)"

cleanup() {
  rm -rf $TEST_DIR
}

trap cleanup EXIT

export TEST_DIR
cp inline_tests_runner* *.ml $TEST_DIR

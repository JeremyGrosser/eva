#!/bin/bash

set -m

expect() {
    let err=$?
    if [ ! $err -eq $1 ]; then
        echo "unexpected error: $err"
        exit $err
    fi
}

wait_exit() {
    while
        fg
        [[ $? -eq 0 ]]
    do true; done
}

rm -rf *.srctrace gnatcov_out
gcc -shared -fPIC -o inject_fail.so inject_fail.c -ldl
#alr clean
alr build && alr gnatcov instrument --level=stmt+mcdc --dump-trigger=atexit --projects=test.gpr
alr build -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

TEST_DURATION=5.0 bin/test & 
hey -c 100 -z 1s http://localhost:9999/version
expect 0
hey -c 100 -z 1s -disable-keepalive http://localhost:9999/version
expect 0
python3 malformed.py
expect 0
curl --silent 'http://localhost:9999/version?q=query#frag'
expect 0
curl --silent 'http://localhost:9999/version?q=test&a=bad'
expect 0
curl --silent 'http://localhost:9999/version?q==='
expect 0
curl --silent 'http://localhost:9999/version?&q&&=='
expect 0
curl --silent 'http://localhost:9999/empty'
expect 1
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=epoll_ctl LD_PRELOAD=./inject_fail.so bin/test &
curl --silent http://localhost:9999/version >/dev/null
expect 56
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=epoll_ctl:2 LD_PRELOAD=./inject_fail.so bin/test &
curl --silent http://localhost:9999/version >/dev/null
expect 56
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=epoll_wait LD_PRELOAD=./inject_fail.so bin/test &
curl --silent http://localhost:9999/version >/dev/null
expect 56 
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=send LD_PRELOAD=./inject_fail.so bin/test &
curl --silent http://localhost:9999/version >/dev/null
expect 52
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=send=32 LD_PRELOAD=./inject_fail.so bin/test &
curl --silent http://localhost:9999/version >/dev/null
expect 52
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=recv LD_PRELOAD=./inject_fail.so bin/test &
curl --silent http://localhost:9999/version >/dev/null
expect 56
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=setsockopt LD_PRELOAD=./inject_fail.so bin/test
expect 1

TEST_DURATION=0.1 MOCK_FAIL=setsockopt:3 LD_PRELOAD=./inject_fail.so bin/test &
curl --silent http://localhost:9999/version >/dev/null
expect 56
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=bind LD_PRELOAD=./inject_fail.so bin/test
expect 1

TEST_DURATION=0.1 MOCK_FAIL=listen LD_PRELOAD=./inject_fail.so bin/test
expect 1

TEST_DURATION=0.4 MOCK_FAIL=accept LD_PRELOAD=./inject_fail.so bin/test &
curl --silent http://localhost:9999/version >/dev/null
expect 56
wait_exit

TEST_DURATION=0.1 bin/test &
curl --silent http://localhost:9999/fault1 >/dev/null
expect 52
wait_exit

TEST_DURATION=0.1 bin/test &
curl --silent http://localhost:9999/fault2 >/dev/null
expect 52
wait_exit

TEST_DURATION=8.0 bin/test &
sleep 1
time nc -v localhost 9999
expect 0
wait_exit

alr gnatcov coverage --annotate=html+ --output-dir gnatcov_out --level=stmt+mcdc --projects test.gpr --keep-reading-traces --units Eva.* *.srctrace
alr gnatcov coverage --annotate=xcov+ --output-dir gnatcov_out --level=stmt+mcdc --projects test.gpr --keep-reading-traces --units Eva.* *.srctrace

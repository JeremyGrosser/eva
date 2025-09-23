#!/bin/bash

set -m
set -x

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
alr build --validation && alr gnatcov instrument --level=stmt+mcdc --dump-trigger=atexit --projects=test.gpr
alr build --validation -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

ulimit -n 65536

TEST_DURATION=3.0 bin/test & 
hey -c 100 -n 200 -t 1 -disable-keepalive https://localhost:9999/version
expect 0
wait_exit

TEST_DURATION=1.0 bin/test & 
python3 malformed.py
expect 0
curl -k --silent 'https://localhost:9999/version?q=query#frag'
expect 0
curl -k --silent 'https://localhost:9999/version?q=test&a=bad'
expect 0
curl -k --silent 'https://localhost:9999/version?q==='
expect 0
curl -k --silent 'https://localhost:9999/version?&q&&=='
expect 0
curl -k --silent -X POST -d test 'https://localhost:9999/'
expect 0
wait_exit

TEST_DURATION=0.1 MOCK_FAIL=epoll_ctl LD_PRELOAD=./inject_fail.so bin/test &
curl -k --silent https://localhost:9999/version >/dev/null
expect 7
wait_exit

TEST_DURATION=0.5 MOCK_FAIL=epoll_ctl:2 LD_PRELOAD=./inject_fail.so bin/test &
curl -k --silent https://localhost:9999/version >/dev/null
expect 7
wait_exit

TEST_DURATION=0.5 MOCK_FAIL=epoll_wait LD_PRELOAD=./inject_fail.so bin/test &
curl -k --silent https://localhost:9999/version >/dev/null
expect 7
wait_exit

#TEST_DURATION=0.5 MOCK_FAIL=write LD_PRELOAD=./inject_fail.so bin/test &
#curl -k --silent https://localhost:9999/version >/dev/null
#expect 7
#wait_exit

#TEST_DURATION=0.5 MOCK_FAIL=write=32 LD_PRELOAD=./inject_fail.so bin/test &
#curl -k --silent https://localhost:9999/version >/dev/null
#expect 52
#wait_exit

#TEST_DURATION=0.5 MOCK_FAIL=read LD_PRELOAD=./inject_fail.so bin/test &
#curl -k --silent https://localhost:9999/version >/dev/null
#expect 56
#wait_exit

#TEST_DURATION=0.5 MOCK_FAIL=read=0 LD_PRELOAD=./inject_fail.so bin/test &
#curl -k --silent https://localhost:9999/version >/dev/null
#expect 56
#wait_exit

#TEST_DURATION=0.5 MOCK_FAIL=setsockopt LD_PRELOAD=./inject_fail.so bin/test
#expect 7
#wait_exit

TEST_DURATION=0.5 MOCK_FAIL=setsockopt:3 LD_PRELOAD=./inject_fail.so bin/test &
curl -k --silent https://localhost:9999/version >/dev/null
expect 7
wait_exit

TEST_DURATION=0.5 MOCK_FAIL=bind LD_PRELOAD=./inject_fail.so bin/test
expect 1
wait_exit

TEST_DURATION=0.5 MOCK_FAIL=listen LD_PRELOAD=./inject_fail.so bin/test
expect 1
wait_exit

TEST_DURATION=0.4 MOCK_FAIL=accept:1 LD_PRELOAD=./inject_fail.so bin/test &
curl -k --silent https://localhost:9999/version >/dev/null
expect 7
curl -k --silent https://localhost:9999/version >/dev/null
expect 0
wait_exit

TEST_DURATION=0.5 bin/test &
curl -k --silent https://localhost:9999/fault1 >/dev/null
expect 7
wait_exit

TEST_DURATION=0.5 bin/test &
curl -k --silent https://localhost:9999/fault2 >/dev/null
expect 7
wait_exit

alr gnatcov coverage --annotate=html+ --output-dir gnatcov_out --level=stmt+mcdc --projects test.gpr --keep-reading-traces --units Eva.* *.srctrace
alr gnatcov coverage --annotate=xcov+ --output-dir gnatcov_out --level=stmt+mcdc --projects test.gpr --keep-reading-traces --units Eva.* *.srctrace

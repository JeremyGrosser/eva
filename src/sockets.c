#define _GNU_SOURCE
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <string.h>
#include <stdint.h>

int eva_bind(uint16_t port) {
    struct sockaddr_in6 addr;
    const int val = 1;
    int sock;
    int err;

    sock = socket(AF_INET6, SOCK_STREAM | SOCK_NONBLOCK, 0);
    if(sock < 0) {
        return sock;
    }

    err = setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val));
    if(err != 0) {
        return err;
    }

    err = setsockopt(sock, SOL_SOCKET, SO_REUSEPORT, &val, sizeof(val));
    if(err != 0) {
        return err;
    }

    memset(&addr, 0, sizeof(addr));
    addr.sin6_family = AF_INET6;
    addr.sin6_addr = in6addr_any;
    addr.sin6_port = htons(port);

    err = bind(sock, (struct sockaddr *)&addr, sizeof(addr));
    if(err != 0) {
        return err;
    }

    err = listen(sock, 256);
    if(err != 0) {
        return err;
    }

    return sock;
}

int eva_accept(int listen_sock) {
    struct sockaddr_in6 addr;
    socklen_t len = sizeof(addr);
    const int val = 1;
    int sock;
    int err;

    memset(&addr, 0, sizeof(addr));
    sock = accept4(listen_sock, (struct sockaddr *)&addr, &len, SOCK_NONBLOCK);
    if(sock == -1) {
        return sock;
    }

    err = setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val));
    if(err == -1) {
        return err;
    }

    return sock;
}

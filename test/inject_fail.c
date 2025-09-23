#define _GNU_SOURCE
#include <sys/epoll.h>
#include <sys/socket.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

static int should_fail(const char *func_name, int *error_code) {
    const char *mock_fail = getenv("MOCK_FAIL");
    if (!mock_fail) return 0;
    
    static int counters[32] = {0}; // support up to 32 different functions
    static const char *func_names[32] = {0};
    static int num_funcs = 0;
    
    // Find or add function counter
    int func_idx = -1;
    for (int i = 0; i < num_funcs; i++) {
        if (func_names[i] && strcmp(func_names[i], func_name) == 0) {
            func_idx = i;
            break;
        }
    }
    if (func_idx == -1 && num_funcs < 32) {
        func_idx = num_funcs++;
        func_names[func_idx] = func_name;
    }
    if (func_idx == -1) return 0; // too many functions
    
    counters[func_idx]++;
    
    // Parse MOCK_FAIL for this function
    char *mock_copy = strdup(mock_fail);
    char *token = strtok(mock_copy, ",");
    
    while (token) {
        char *colon = strchr(token, ':');
        if (colon) {
            *colon = '\0';
            if (strcmp(token, func_name) == 0) {
                char *after_colon = colon + 1;
                char *error_spec = strchr(after_colon, '=');
                int target_call;
                if (error_spec) {
                    *error_spec = '\0';
                    target_call = atoi(after_colon);
                    if (error_code) *error_code = atoi(error_spec + 1);
                } else {
                    target_call = atoi(after_colon);
                }
                free(mock_copy);
                return counters[func_idx] == target_call;
            }
        } else {
            char *error_spec = strchr(token, '=');
            if (error_spec) {
                *error_spec = '\0';
                if (strcmp(token, func_name) == 0) {
                    if (error_code) *error_code = atoi(error_spec + 1);
                    free(mock_copy);
                    return 1; // fail all calls
                }
            } else {
                if (strcmp(token, func_name) == 0) {
                    free(mock_copy);
                    return 1; // fail all calls
                }
            }
        }
        token = strtok(NULL, ",");
    }
    
    free(mock_copy);
    return 0;
}

int epoll_create(int size) {
    int custom_errno = EMFILE;
    if (should_fail("epoll_create", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static int (*real_epoll_create)(int) = NULL;
    if (!real_epoll_create) {
        real_epoll_create = dlsym(RTLD_NEXT, "epoll_create");
    }
    return real_epoll_create(size);
}

int epoll_create1(int flags) {
    int custom_errno = EMFILE;
    if (should_fail("epoll_create1", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static int (*real_epoll_create1)(int) = NULL;
    if (!real_epoll_create1) {
        real_epoll_create1 = dlsym(RTLD_NEXT, "epoll_create1");
    }
    return real_epoll_create1(flags);
}

int epoll_ctl(int epfd, int op, int fd, struct epoll_event *event) {
    int custom_errno = EBADF;
    if (should_fail("epoll_ctl", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static int (*real_epoll_ctl)(int, int, int, struct epoll_event *) = NULL;
    if (!real_epoll_ctl) {
        real_epoll_ctl = dlsym(RTLD_NEXT, "epoll_ctl");
    }
    return real_epoll_ctl(epfd, op, fd, event);
}

int epoll_wait(int epfd, struct epoll_event *events, int maxevents, int timeout) {
    int custom_errno = EBADF;
    if (should_fail("epoll_wait", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static int (*real_epoll_wait)(int, struct epoll_event *, int, int) = NULL;
    if (!real_epoll_wait) {
        real_epoll_wait = dlsym(RTLD_NEXT, "epoll_wait");
    }
    return real_epoll_wait(epfd, events, maxevents, timeout);
}

ssize_t send(int sockfd, const void *buf, size_t len, int flags) {
    int custom_errno = ECONNRESET;
    if (should_fail("send", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static ssize_t (*real_send)(int, const void *, size_t, int) = NULL;
    if (!real_send) {
        real_send = dlsym(RTLD_NEXT, "send");
    }
    return real_send(sockfd, buf, len, flags);
}

ssize_t recv(int sockfd, void *buf, size_t len, int flags) {
    int custom_errno = ECONNRESET;
    if (should_fail("recv", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static ssize_t (*real_recv)(int, void *, size_t, int) = NULL;
    if (!real_recv) {
        real_recv = dlsym(RTLD_NEXT, "recv");
    }
    return real_recv(sockfd, buf, len, flags);
}

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
    int custom_errno = EADDRINUSE;
    if (should_fail("bind", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static int (*real_bind)(int, const struct sockaddr *, socklen_t) = NULL;
    if (!real_bind) {
        real_bind = dlsym(RTLD_NEXT, "bind");
    }
    return real_bind(sockfd, addr, addrlen);
}

int listen(int sockfd, int backlog) {
    int custom_errno = EADDRINUSE;
    if (should_fail("listen", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static int (*real_listen)(int, int) = NULL;
    if (!real_listen) {
        real_listen = dlsym(RTLD_NEXT, "listen");
    }
    return real_listen(sockfd, backlog);
}

int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen) {
    int custom_errno = EINVAL;
    if (should_fail("setsockopt", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static int (*real_setsockopt)(int, int, int, const void *, socklen_t) = NULL;
    if (!real_setsockopt) {
        real_setsockopt = dlsym(RTLD_NEXT, "setsockopt");
    }
    return real_setsockopt(sockfd, level, optname, optval, optlen);
}

int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
    int custom_errno = ECONNABORTED;
    if (should_fail("accept", &custom_errno)) {
        errno = custom_errno;
        return -1;
    }
    static int (*real_accept)(int, struct sockaddr *, socklen_t *) = NULL;
    if (!real_accept) {
        real_accept = dlsym(RTLD_NEXT, "accept");
    }
    return real_accept(sockfd, addr, addrlen);
}
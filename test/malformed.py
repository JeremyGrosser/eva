from socket import *

addr = ('localhost', 9999)

def only_newlines():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'\r\n\r\n\r\n\r\n')

def unending():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'G' * 256)
    sock.sendall(b'G' * 4096)

def missing_space_after_method():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET/ HTTP/1.0\r\n\r\n')


def nothing_after_method():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET \r\n\r\n')


def missing_space_after_target():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET /\n\n')


def send_fragment():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET /#frag HTTP/1.1\r\nHost: localhost\r\n\r\n')


def method_only():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET')


def target_only():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET /\r\n\r\n')


def no_crlf():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1Host:localhost\r\n\r\n')


def key_only():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1\r\nHost:\r\n\r\n')


def no_colon():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1\r\nHost localhost\r\n\r\n')


def incomplete_headers():
    # Test line 35: Req.End_Headers = 0 (no double CRLF found)
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1\r\nHost: localhost')
    # Don't send the final \r\n\r\n


def unexpected_end_after_method():
    # Test line 52: I + 1 > Req.End_Headers after finding method space
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET \r\n\r\n')  # Space right before end


def unexpected_end_after_target():
    # Test line 63: I + 1 > Req.End_Headers after finding target space
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / \r\n\r\n')  # Space right before end


def missing_crlf_after_protocol():
    # Test line 68: No CRLF found after protocol
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1extra\r\n\r\n')  # No space/CRLF after HTTP/1.1


def missing_crlf_after_header_value():
    # Test line 103: No CRLF found after header value
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1\r\nHost: localhostextra\r\n\r\n')


def search_string_too_long():
    # Test line 17: Str'Length > (Req.End_Headers - First + 1)
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    # Send minimal valid request where search will be too long
    sock.sendall(b'A\r\n\r\n')  # Very short, searching for longer strings will fail


def empty_line_in_headers():
    # Test line 77-80: Empty line detection (exit from header loop)
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1\r\nHost: localhost\r\n\r\n\r\n')


def header_without_value():
    # Test various header validation conditions
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1\r\nHost:\r\n\r\n')


def constraint_error_trigger():
    # Try to trigger Constraint_Error (line 127)
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    # Send something that might cause index out of bounds
    sock.sendall(b'GET / HTTP/1.1\r\n' + b'A' * 8000 + b'\r\n\r\n')


def client_went_away():
    sock = socket(AF_INET, SOCK_STREAM)
    sock.connect(addr)
    sock.sendall(b'GET / HTTP/1.1\r\n\r\n')
    sock.close()


if __name__ == '__main__':
    only_newlines()
    unending()
    missing_space_after_method()
    nothing_after_method()
    missing_space_after_target()
    send_fragment()
    method_only()
    target_only()
    no_crlf()
    key_only()
    no_colon()
    incomplete_headers()
    unexpected_end_after_method()
    unexpected_end_after_target()
    missing_crlf_after_protocol()
    missing_crlf_after_header_value()
    search_string_too_long()
    empty_line_in_headers()
    header_without_value()
    constraint_error_trigger()
    client_went_away()

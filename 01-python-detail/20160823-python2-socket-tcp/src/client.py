# -*- coding: utf-8 -*-
# /usr/bin/env python

if __name__ == '__main__':
    import socket
    import time
    while True:
        msg = raw_input("Please input your message: ")
        try:
            # 每次循环建立一个新的连接
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.connect(('localhost', 10000))
            time.sleep(1)
            sock.send(msg)
            print 'get message from server: ' + sock.recv(1024)
            sock.close()
            if 'q' == msg:
                break
        except socket.error:
            print 'socket error!'

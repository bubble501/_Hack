# -*- coding: utf-8 -*-
# /usr/bin/env python

if __name__ == '__main__':
    import socket
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM) 
    sock.bind(('localhost', 10000))
    sock.listen(5)
    while True:
        connection, address = sock.accept()
        try:
            connection.settimeout(5)
            buf = connection.recv(1024)
            if 'q' != buf:
                connection.send('server recv success')
                print '[' + address[0] + ':' + str(address[1]) + '] send a message to server: ' + buf
            else:
                connection.send('bye bye!')
                print '[' + address[0] + ':' + str(address[1]) + '] quit!'
        except socket.timeout:
            print 'time out!'
        connection.close()

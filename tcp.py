
#! should eventually hold the peerwire implementation. 
#! easier to connect to and all that. 
#! currently works with just one client.
# TODO: Ask Akshat


import socket
import sys
import hashlib
HOST = '127.0.0.1'  # Localhost
PORT = eval(sys.argv[1])  # Port to bind to


my_id = hashlib.sha1(b'cmprssnenthsst').digest()
# Create a TCP/IP socket
server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# Bind the socket to the address and port
server_socket.bind((HOST, PORT))

# Listen for incoming connections
server_socket.listen(1)
print(f"Server listening on {HOST}:{PORT}...")

# Accept a connection
connection, client_address = server_socket.accept()
print(f"Connected by {client_address}")


ids = {
    1: 'choke',
    2: 'unchoke',
    3: 'interested',
    4: 'not interested',
    5: 'have',
    6: 'bitfield',
    7: 'request',
    8: 'piece',
    9: 'cancel',
    10: 'port'
}
# Receive data from the client
i  = 0
try:
    data = connection.recv(1024)
    # print(data.hex())
    #! Code for Handshake
    if data[0] == 19:
        print("handshake received")
        proto_pstr = data[:20]
        info_hash = data[20:28]
        info_hash = data[28:48]
        peer_id = data[48:68]

        #! In an ideal world the peer checks if it has info_hash available, but we dont
        if info_hash:
            response = bytearray()
            response.extend(proto_pstr)
            response.extend(b'\x00' * 8)  # Reserved bytes
            response.extend(info_hash)
            response.extend(my_id)
            connection.sendall(response)
    
    if data[:4] == b'\x00' * 4:
        print(f"{client_address} sent a keep-alive. echoing.")
        response = bytearray()
        response.extend(data[:4])
        connection.sendall(response)
    
    if data[:4] == b'\x00\x00\x00\x01':
        print("MMMMEMEMEMEME")
        import time
        time.sleep(5)
        print(data[4])
        #! again, in an ideal world the tcp peer checks for symmetry and stuff
        print(f"{client_address} sent a {ids[data[4]]}. echoing.")
        response = bytearray()
        response.extend(data)
        connection.sendall(response)

    if data[:4] == b'\x00\x00\x00\x0D' and data[4] == 6:
        print(f"{client_address} requested to download a chunk")
        idx_bytes = data[5:9]
        begin_bytes = data[9:13]
        length_bytes = data[13:17]


        #! need to rewrite this to conform to the bittorrent spec
        #! checks would go here if i were a coward
        begin_int = int.from_bytes(begin_bytes, byteorder='big')
        length_int = int.from_bytes(length_bytes, byteorder='big')
        print(f"Sending {begin_bytes.hex()} to {(begin_int + length_int):x} of chunk {idx_bytes.hex()} to {client_address}")
        response = bytearray()
        response.extend(b'\x00')
        response.extend(b'wlovefromjha')
        connection.sendall(response)
    
    elif data == b'210802':
        print("close signal received. bye.")
        connection.close()

    else:
        print(data)
        connection.close()
        # connection.sendall(data)  # Echo the received data back to the client
except Exception as e:
    connection.close()
    print(f"An error occurred: {e}")
finally:
    connection.close()
    server_socket.close()


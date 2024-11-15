
#! should eventually hold the peerwire implementation. 
#! easier to connect to and all that. 
#! currently works with just one client.
# TODO: Ask Akshat


import socket
import sys
import hashlib
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')


HOST = '127.0.0.1'  # Localhost
PORT = eval(sys.argv[1])  # Port to bind to


my_id = hashlib.sha1(b'cmprssnenthsst').digest()
server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.bind((HOST, PORT))
server_socket.listen(1)
logging.info(f"Server listening on {HOST}:{PORT}...")
connection, client_address = server_socket.accept()
logging.info(f"Connected by {client_address}")


ids = {
    0: 'choke',
    1: 'unchoke',
    2: 'interested',
    3: 'not interested',
    4: 'have',
    5: 'bitfield',
    6: 'request',
    7: 'piece',
    8: 'cancel',
    9: 'port'
}
# Receive data from the client
i  = 0
try:
    while True:
        data = connection.recv(1024)
        #! Code for Handshake
        if data[0] == 19:
            logging.info("handshake received")
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
            logging.info(f"{client_address} sent a keep-alive. echoing.")
            response = bytearray()
            response.extend(data[:4])
            connection.sendall(response)
        
        if data[:4] == b'\x00\x00\x00\x01':
            #! again, in an ideal world the tcp peer checks for symmetry and stuff
            logging.info(f"{client_address} sent a {ids[data[4]]}. echoing.")
            response = bytearray()
            response.extend(data)
            connection.sendall(response)

        if data[:4] == b'\x00\x00\x00\x0D' and data[4] == 6:
            logging.info(f"{client_address} requested to download a chunk")
            idx_bytes = data[5:9]
            begin_bytes = data[9:13]
            length_bytes = data[13:17]


            #! need to rewrite this to conform to the bittorrent spec
            #! checks would go here if i were a 
            

            #! maybe add actual file logic. nope. i am done.
            begin_int = int.from_bytes(begin_bytes, byteorder='big')
            length_int = int.from_bytes(length_bytes, byteorder='big')
            # xxx = input()
            logging.info(f"Sending {begin_bytes.hex()} to {(begin_int + length_int):x} of chunk {idx_bytes.hex()} to {client_address}")
            response = bytearray()
            response.extend(b'\x00\x00\x00\x00')  # Length prefix placeholder
            response.append(7)  # Message ID for 'piece'
            response.extend(idx_bytes)
            response.extend(begin_bytes)
            response.extend(b'\x00' * length_int)
            
            # Update the length prefix
            length_prefix = length_int + 9
            response[0:4] = length_prefix.to_bytes(4, byteorder='big')
            connection.sendall(response)
                
                # Construct the piece message to send back
                
        
        
        if data == b'210802':
            logging.info("close signal received. bye.")
            connection.close()
            break

    # if:
    #     logging.info(data)
    #     connection.close()
        # connection.sendall(data)  # Echo the received data back to the client
except Exception as e:
    connection.close()
    logging.info(f"An error occurred: {e}")
finally:
    connection.close()
    server_socket.close()


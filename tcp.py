import socket


#! should eventually hold the peerwire implementation. 
#! easier to connect to and all that. 
# TODO: Ask Akshat


# Define the server address and port
HOST = '127.0.0.1'  # Localhost
PORT = 5000  # Port to bind to

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

# Receive data from the client
try:
    while True:
        data = connection.recv(1024)
        if not data:
            break
        print(f"Received: {data.decode()}")
        connection.sendall(data)  # Echo the received data back to the client
finally:
    # Clean up the connection
    connection.close()

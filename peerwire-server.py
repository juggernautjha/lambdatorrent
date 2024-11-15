import socket
import struct
import hashlib
import random
from dataclasses import dataclass
from typing import Dict, Optional, Set
import threading
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

@dataclass
class PeerInfo:
    """Store information about connected peers"""
    connection: socket.socket
    peer_id: Optional[bytes] = None
    info_hash: Optional[bytes] = None
    am_choking: bool = True
    am_interested: bool = False
    peer_choking: bool = True
    peer_interested: bool = False
    available_pieces: Set[int] = None
    
    def __post_init__(self):
        self.available_pieces = set()

class PeerWireProtocol:
    # Message types
    CHOKE = 0
    UNCHOKE = 1
    INTERESTED = 2
    NOT_INTERESTED = 3
    HAVE = 4
    BITFIELD = 5
    REQUEST = 6
    PIECE = 7
    CANCEL = 8
    
    def __init__(self, host: str, port: int):
        self.host = host
        self.port = port
        self.server_socket = None
        self.peers: Dict[str, PeerInfo] = {}
        self.my_peer_id = b'-PY0001-' + bytes(random.randint(0, 255) for _ in range(12))
        self.pieces = {}  # Dictionary to store piece data
        self.piece_size = 16384  # Default piece size (16KB)
        
    def start_server(self):
        """Initialize and start the server"""
        self.server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server_socket.bind((self.host, self.port))
        self.server_socket.listen(5)
        logging.info(f"Server listening on {self.host}:{self.port}...")
        
        while True:
            try:
                connection, address = self.server_socket.accept()
                peer_handler = threading.Thread(
                    target=self.handle_peer_connection,
                    args=(connection, address)
                )
                peer_handler.daemon = True
                peer_handler.start()
            except Exception as e:
                logging.error(f"Error accepting connection: {e}")
    
    def handle_peer_connection(self, connection: socket.socket, address: tuple):
        """Handle incoming peer connections"""
        peer_info = PeerInfo(connection=connection)
        self.peers[address] = peer_info
        
        try:
            # Handle handshake
            if not self.handle_handshake(peer_info):
                logging.warning(f"Handshake failed for {address}")
                self.disconnect_peer(address)
                return
            
            logging.info(f"Successful handshake with {address}")
            
            # Main message loop
            while True:
                message = self.receive_message(peer_info)
                if message is None:
                    break
                    
                self.handle_message(peer_info, message)
                
        except Exception as e:
            logging.error(f"Error handling peer {address}: {e}")
        finally:
            self.disconnect_peer(address)
    
    def handle_handshake(self, peer_info: PeerInfo) -> bool:
        """Handle BitTorrent handshake protocol"""
        try:
            # Receive handshake
            data = peer_info.connection.recv(68)  # Handshake is exactly 68 bytes
            if len(data) != 68:
                return False
            
            # Parse handshake
            pstrlen = data[0]
            if pstrlen != 19:
                return False
                
            protocol = data[1:20]
            if protocol != b'BitTorrent protocol':
                return False
                
            # Extract info_hash and peer_id
            info_hash = data[28:48]
            peer_id = data[48:68]
            
            peer_info.info_hash = info_hash
            peer_info.peer_id = peer_id
            
            # Send handshake response
            response = struct.pack(
                '>B19s8x20s20s',
                19,
                b'BitTorrent protocol',
                info_hash,
                self.my_peer_id
            )
            peer_info.connection.sendall(response)
            
            return True
            
        except Exception as e:
            logging.error(f"Handshake error: {e}")
            return False
    
    def receive_message(self, peer_info: PeerInfo) -> Optional[tuple]:
        """Receive and parse a message from peer"""
        try:
            # Read message length (4 bytes)
            length_prefix = peer_info.connection.recv(4)
            if not length_prefix:
                return None
                
            length = struct.unpack('>I', length_prefix)[0]
            
            if length == 0:  # Keep-alive message
                return (None, b'')
                
            # Read message ID and payload
            message = peer_info.connection.recv(length)
            if not message:
                return None
                
            message_id = message[0]
            payload = message[1:] if length > 1 else b''
            
            return (message_id, payload)
            
        except Exception as e:
            logging.error(f"Error receiving message: {e}")
            return None
    
    def handle_message(self, peer_info: PeerInfo, message: tuple):
        """Handle different types of peer wire protocol messages"""
        if message is None or len(message) != 2:
            return
            
        message_id, payload = message
        
        try:
            if message_id == self.CHOKE:
                peer_info.peer_choking = True
                logging.info(f"Peer {peer_info.peer_id} choked us")
                
            elif message_id == self.UNCHOKE:
                peer_info.peer_choking = False
                logging.info(f"Peer {peer_info.peer_id} unchoked us")
                
            elif message_id == self.INTERESTED:
                peer_info.peer_interested = True
                logging.info(f"Peer {peer_info.peer_id} is interested")
                
                # Automatically unchoke the peer
                self.send_message(peer_info, self.UNCHOKE)
                peer_info.peer_choking = False
                logging.info(f"Unchoking peer {peer_info.peer_id}")
                
            elif message_id == self.NOT_INTERESTED:
                peer_info.peer_interested = False
                logging.info(f"Peer {peer_info.peer_id} is not interested")
                
            elif message_id == self.HAVE:
                piece_index = struct.unpack('>I', payload)[0]
                peer_info.available_pieces.add(piece_index)
                logging.info(f"Peer {peer_info.peer_id} has piece {piece_index}")
                
            elif message_id == self.BITFIELD:
                self.handle_bitfield(peer_info, payload)
                
            elif message_id == self.REQUEST:
                self.handle_request(peer_info, payload)
                
            elif message_id == self.PIECE:
                self.handle_piece(peer_info, payload)
                
            elif message_id == self.CANCEL:
                self.handle_cancel(peer_info, payload)
                
        except Exception as e:
            logging.error(f"Error handling message: {e}")
    
    def handle_bitfield(self, peer_info: PeerInfo, payload: bytes):
        """Process bitfield message"""
        for i in range(len(payload) * 8):
            if payload[i // 8] & (1 << (7 - (i % 8))):
                peer_info.available_pieces.add(i)
        logging.info(f"Received bitfield from {peer_info.peer_id}, available pieces: {len(peer_info.available_pieces)}")
    
    def handle_request(self, peer_info: PeerInfo, payload: bytes):
        index, begin, length = struct.unpack('>III', payload)
        
        if index in self.pieces:
            piece_data = self.pieces[index]
            if begin + length <= len(piece_data):
                response = struct.pack('>IBI', len(payload) + 1 + len(piece_data[begin:begin+length]), self.PIECE, index) + \
                        struct.pack('>I', begin) + piece_data[begin:begin+length]
                peer_info.connection.sendall(response)
                logging.info(f"Sent piece {index} to peer {peer_info.peer_id}")
    
    def handle_piece(self, peer_info: PeerInfo, payload: bytes):
        """Handle received piece"""
        if len(payload) < 8:
            return
            
        index, begin = struct.unpack('>II', payload[:8])
        block = payload[8:]
        
        if index not in self.pieces:
            self.pieces[index] = bytearray(self.piece_size)
            
        self.pieces[index][begin:begin + len(block)] = block
        logging.info(f"Received piece {index} from peer {peer_info.peer_id}")
    
    def handle_cancel(self, peer_info: PeerInfo, payload: bytes):
        """Handle cancel request"""
        index, begin, length = struct.unpack('>III', payload)
        logging.info(f"Received cancel request for piece {index} from peer {peer_info.peer_id}")
    
    def disconnect_peer(self, address: tuple):
        """Clean up peer connection"""
        if address in self.peers:
            try:
                self.peers[address].connection.close()
            except Exception as e:
                logging.error(f"Error closing connection: {e}")
            del self.peers[address]
            logging.info(f"Disconnected peer {address}")
    
    def send_message(self, peer_info: PeerInfo, message_id: int, payload: bytes = b''):
        """Send a message to a peer"""
        try:
            length = len(payload) + 1  # +1 for message_id
            message = struct.pack('>IB', length, message_id) + payload
            peer_info.connection.sendall(message)
        except Exception as e:
            logging.error(f"Error sending message: {e}")
    
    def shutdown(self):
        """Shutdown the server and clean up"""
        for address in list(self.peers.keys()):
            self.disconnect_peer(address)
            
        if self.server_socket:
            self.server_socket.close()
            logging.info("Server shutdown complete")

def main():
    # Create and start the server
    protocol = PeerWireProtocol('127.0.0.1', 5001)
    
    try:
        protocol.start_server()
    except KeyboardInterrupt:
        logging.info("Shutting down server...")
        protocol.shutdown()
    except Exception as e:
        logging.error(f"Server error: {e}")
        protocol.shutdown()

if __name__ == "__main__":
    main()

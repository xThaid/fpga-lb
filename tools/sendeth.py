from socket import *
from struct import *

def sendeth(data, interface = "tap0"):
  """Send raw Ethernet packet on interface."""
  s = socket(AF_PACKET, SOCK_RAW)

  # From the docs: "For raw packet
  # sockets the address is a tuple (ifname, proto [,pkttype [,hatype]])"
  s.bind((interface, 0))
  return s.send(data)

def create_arp_request(smac, sip, ip_addr):
  frame = [
    ### ETHERNET header###
    # Destination MAC address (=broadcast) :
    pack("!6B", *(0xFF,) * 6),
    # Source MAC address :
    bytes.fromhex(smac),
    # Type of protocol (=ARP) :
    pack("!H", 0x0806),
    ### ARP payload###
    # Type of protocol hw/soft (=Ethernet/IP) :
    pack("!HHBB", 0x0001, 0x0800, 0x0006, 0x0004),
    # Operation (=ARP Request) :
    pack("!H", 0x0001),
    # Source MAC address :
    bytes.fromhex(smac),
    # Source IP address :
    bytes.fromhex(sip),
    # Destination MAC address (what we are looking for) (=00*6) :
    pack("!6B", *(0,) * 6),
    # Target IP address:
    bytes.fromhex(ip_addr)
  ]

  return b"".join(frame)

def icmp_ping_req():
  # src=fe:ed:fa:ce:be:ef, dst=52:54:00:12:35:02, type=0x0800 (IP)
  ethernet_packet = [0x52, 0x54, 0x00, 0x12, 0x35, 0x02, 0xfe, 0xed, 0xfa,
                      0xce, 0xbe, 0xef, 0x08, 0x00]

  # src=10.0.2.15, dst=195.88.54.16 (vg.no), checksum, etc.
  ipv4_header = [0x45, 0x00, 0x00, 0x54, 0x05, 0x9f, 0x40, 0x00, 0x40, 0x01,
                  0x2f, 0x93, 0x0a, 0x00, 0x02, 0x0f, 0xc3, 0x58, 0x36, 0x10]

  # echo (ping) request, checksum 2b45, etc
  icmp_ping = [0x08, 0x00, 0x2b, 0x45, 0x11, 0x22, 0x00, 0x02, 0xa9, 0xf4, 0x5c,
              0x53, 0x00, 0x00, 0x00, 0x00, 0xf5, 0x7b, 0x01, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
              0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23,
              0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e,
              0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37]

if __name__ == "__main__":
  n = sendeth(create_arp_request("aabbccddeeff", "aaaabbbb", "0a640001"))
  print("Sent packet of length %d bytes" % n)

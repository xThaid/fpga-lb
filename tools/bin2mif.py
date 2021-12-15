import argparse
import sys
import binascii

def read_input(input_path):
    data = []
    if input_path != '-':
        with open(input_path, 'rb') as f:
            data = f.read()
    else:
        data = sys.stdin.buffer.read()

    return data

def dump_mif(data, width, endianess):
    bytes_per_word = width // 8
    depth = len(data) // bytes_per_word

    print("-- Created by bin2mif.py")
    print("DEPTH         = {};".format(depth))
    print("WIDTH         = {};".format(width))
    print("ADDRESS_RADIX = HEX;")
    print("DATA_RADIX    = HEX;")
    print("CONTENT BEGIN")

    addr = 0
    chunks = [data[i:i+bytes_per_word] for i in range(0, len(data), bytes_per_word)]
    for chunk in chunks:
        if endianess == 'lsb':
            chunk = bytes(reversed(chunk))
        print("  {:x}: {};".format(addr, chunk.hex()))
        addr += 1

    if len(chunks) < depth:
        print("  {:x}...{:x}: {};".format(len(chunks), depth - 1, 0))

    print("END;")

def main():
    parser = argparse.ArgumentParser(description='Dumps binary as MIF', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-w', '--width', type=int, help='Word width in bits from 1 to 128 (must be divisible by 8)', default=32)
    parser.add_argument('-f', '--first', type=str, help='LSB/MSB first (little/big-endian)', default='lsb', choices=['lsb', 'msb'])
    parser.add_argument('input', type=str, nargs='?', help='Input file or standard input (-)', default='-')

    args = parser.parse_args()

    if args.width % 8 != 0:
        raise argparse.ArgumentTypeError('Width must be divisible by 8')

    data = read_input(args.input)
    dump_mif(data, args.width, args.first)


if __name__ == "__main__":
    sys.exit(main())
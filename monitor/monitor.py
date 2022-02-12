#! /usr/bin/env python3

import sys
import time
import curses
import intel_jtag_uart

REAL_NAMES = {
    10: "192.168.0.230",
    11: "192.168.0.231",
    12: "192.168.0.232",
    13: "192.168.0.233",
    14: "192.168.0.234",
    15: "192.168.0.235",
    16: "192.168.0.236",
    17: "192.168.0.237",
}

LG_ENABLED = 1
LG_TX_PERIOD = 25
LG_PAYLOAD_SIZE = 1400

def conv_unit(val):
    if val >= 1000000000.0:
        return val / 1000000000, 'G'
    if val >= 1000000.0:
        return val / 1000000, 'M'
    if val >= 1000.0:
        return val / 1000, 'k'
    return val, ''

def main(stdscr):
    stdscr.clear()

    try:
        ju = intel_jtag_uart.intel_jtag_uart()

    except Exception as e:
        print(e)
        sys.exit(0)

    time.sleep(1)

    ju.write((f'{LG_ENABLED} {LG_TX_PERIOD} {LG_PAYLOAD_SIZE}\n').encode())

    last_stats = None
    stats = {}
    last_read = time.time()

    while True:
        bytes_raw = ju.read()
        if len(bytes_raw) == 0:
            time.sleep(0.05)
            continue

        uart_str = bytes_raw.decode('utf-8')
        for entry in uart_str.split('\n'):
            comp = entry.split(' ')
            if len(comp) != 4:
                continue
            stats[int(comp[0])] = (int(comp[1]), (int(comp[2]) << 32) | int(comp[3]))

        if last_stats is None:
            last_stats = dict(stats)

        now = time.time()
        if now - last_read >= 0.5:
            pos = 0
            dt = now - last_read

            def print_stats(stats, last_stats):
                pps = (stats[0] - last_stats[0]) / dt
                bps = (stats[1] - last_stats[1]) / dt

                pps, pps_unit = conv_unit(pps)
                bps, bps_unit = conv_unit(bps * 8)

                p, p_unit = conv_unit(stats[0])
                b, b_unit = conv_unit(stats[1])

                return '{:7.3f} {: >1}pps / {:7.3f} {: >1}p    {:7.3f} {: >1}bps / {:7.3f} {: >1}B'.format(
                            pps, pps_unit, p, p_unit, bps, bps_unit, b, b_unit)

            for real, real_name in REAL_NAMES.items():
                info = print_stats(stats[real], last_stats[real])

                stdscr.addstr(pos, 0, '{: >15} - {}'.format(real_name, info))
                pos += 1

            stdscr.addstr(20, 0, 'LOAD GENERATOR - {}'.format(print_stats(stats[100], last_stats[100])))

            stdscr.refresh()

            last_read = now
            last_stats = dict(stats)

    ju.close()

curses.wrapper(main)

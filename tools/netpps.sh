#!/bin/bash

time="1"     # one second
int="enp2s0"   # network interface

while true
    do
        txpkts_old="`cat /sys/class/net/$int/statistics/tx_packets`" # sent packets
        rxpkts_old="`cat /sys/class/net/$int/statistics/rx_packets`" # recv packets
        sleep $time
        txpkts_new="`cat /sys/class/net/$int/statistics/tx_packets`" # sent packets
        rxpkts_new="`cat /sys/class/net/$int/statistics/rx_packets`" # recv packets
        txpkts="`expr $txpkts_new - $txpkts_old`"		     # evaluate expressions for sent packets
        rxpkts="`expr $rxpkts_new - $rxpkts_old`"		     # evaluate expressions for recv packets
        echo "tx $txpkts pkts/s - rx $rxpkts pkt/s on interface $int"
    done

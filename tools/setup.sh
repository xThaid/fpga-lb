#!/bin/bash
sudo ip netns add fpga_lb
sudo ip link set enp2s0 netns fpga_lb
sudo ip netns exec fpga_lb ip link set up enp2s0
sudo ip netns exec fpga_lb ip addr add 10.0.100.1 dev enp2s0

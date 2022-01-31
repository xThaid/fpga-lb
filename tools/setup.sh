#!/bin/bash
sudo ip netns add fpga_lb
sudo ip link set enp2s0 netns fpga_lb

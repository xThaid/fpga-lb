`timescale 1ns / 1ps
`default_nettype none

module arp_cache #(
  parameter integer CacheAddrWidth = 5
)(
  input         clk,
  input         rst,

  output        query_req_ready_o,
  input         query_req_valid_i,
  input  [31:0] query_ip_i,

  input         query_resp_ready_i,
  output        query_resp_valid_o,
  output [47:0] query_mac_o,
  output        query_err_o,

  input         write_valid_i,
  input  [31:0] write_ip_i,
  input  [47:0] write_mac_i
);

  wire [31:0] query_req_hash, write_req_hash;

  reg  [CacheAddrWidth-1:0] query_ptr, write_ptr;
  wire [CacheAddrWidth-1:0] query_ptr_next, write_ptr_next;

  wire store_query, finalize_query, query_valid_next;
  reg  query_valid;

  wire query_req_ready, query_resp_valid_next, query_err_next;
  reg query_resp_valid, query_err;
  reg [31:0] query_ip;
  reg [47:0] query_mac;

  wire store_write, write_en, write_valid_next;
  reg write_valid;
  reg [31:0] write_ip;
  reg [47:0] write_mac;

  reg valid_mem[(2**CacheAddrWidth)-1:0];
  reg [31:0] ip_addr_mem[(2**CacheAddrWidth)-1:0];
  reg [47:0] mac_addr_mem[(2**CacheAddrWidth)-1:0];

  integer i;

  initial begin
    for (i = 0; i < 2**CacheAddrWidth; i = i + 1) begin
        valid_mem[i] = 1'b0;
        ip_addr_mem[i] = 32'd0;
        mac_addr_mem[i] = 48'd0;
    end
  end

  assign query_req_ready_o = query_req_ready;

  assign query_resp_valid_o = query_resp_valid;
  assign query_mac_o = query_mac;
  assign query_err_o = query_err;

  crc32 crc32_query (
    .state_in(32'hffffffff),
    .data_in(query_ip_i),
    .state_out(query_req_hash)
  );

  crc32 crc32_write (
    .state_in(32'hffffffff),
    .data_in(write_ip_i),
    .state_out(write_req_hash)
  );

  assign query_req_ready = (~query_valid || ~query_resp_valid || query_resp_ready_i);

  assign store_query = (query_req_valid_i && (~query_valid || ~query_resp_valid || query_resp_ready_i));
  assign finalize_query = (query_valid && (~query_resp_valid || query_resp_ready_i));

  assign query_ptr_next = store_query ? query_req_hash[CacheAddrWidth-1:0] : query_ptr;
  assign query_err_next = finalize_query ? (!valid_mem[query_ptr] || ip_addr_mem[query_ptr] != query_ip) : query_err;

  assign query_valid_next = store_query ? 1'b1 : (finalize_query ? 1'b0 : query_valid);
  assign query_resp_valid_next = finalize_query || (query_resp_valid & ~query_resp_ready_i);

  assign store_write = write_valid_i;
  assign write_ptr_next = store_write ? write_req_hash[CacheAddrWidth-1:0] : write_ptr;
  assign write_valid_next = store_write;
  assign write_en = write_valid;

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      write_ptr <= {CacheAddrWidth{1'b0}};
      query_ptr <= {CacheAddrWidth{1'b0}};
      query_valid <= 1'b0;
      write_valid <= 1'b0;
      query_resp_valid <= 1'b0;
      query_err <= 1'b0;
      query_ip <= 32'b0;
      query_mac <= 48'b0;
      write_valid <= 1'b0;
      write_ip <= 32'b0;
      write_mac <= 48'b0;
    end else begin
      write_ptr <= write_ptr_next;
      query_ptr <= query_ptr_next;
      query_valid <= query_valid_next;
      query_resp_valid <= query_resp_valid_next;
      query_err <= query_err_next;
      write_valid <= write_valid_next;

      if (store_query) query_ip <= query_ip_i;
      if (finalize_query) query_mac <= mac_addr_mem[query_ptr];

      if (store_write) begin
        write_ip <= write_ip_i;
        write_mac <= write_mac_i;
      end

      if (write_en) begin
        valid_mem[write_ptr] <= 1'b1;
        ip_addr_mem[write_ptr] <= write_ip;
        mac_addr_mem[write_ptr] <= write_mac;
      end
    end
  end

endmodule
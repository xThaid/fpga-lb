open Base
open Hardcaml

let gen_masks lfsr_poly lfsr_width data_width bit_reverse =
  let open Bits in
  let lfsr_poly_bit = Bits.of_int ~width:lfsr_width lfsr_poly in
  let lfsr_poly_bitlist = split_lsb lfsr_poly_bit ~part_width:1 in
  
  let rec loop i lfsr_masks_state lfsr_masks_data =
    let (state_head, state_val) = List.split_n lfsr_masks_state (lfsr_width - 1) in
    let (data_head, data_val) = List.split_n lfsr_masks_data (data_width - 1) in
    let state_val = List.hd_exn state_val in
    let data_val = List.hd_exn data_val in
    
    let data_val = data_val ^: (sll (one data_width) i) in
    
    let state_head = List.map2_exn state_head (List.tl_exn lfsr_poly_bitlist) ~f:(fun mask bit ->
      if is_vdd bit then mask ^: state_val else mask) in
    let data_head = List.map2_exn data_head (List.tl_exn lfsr_poly_bitlist) ~f:(fun mask bit ->
      if is_vdd bit then mask ^: data_val else mask) in

    let lfsr_masks_state = state_val :: state_head in
    let lfsr_masks_data = data_val :: data_head in

    if i = 0 then (lfsr_masks_state, lfsr_masks_data) else (loop (i - 1) lfsr_masks_state lfsr_masks_data)
  in

  let rev_masks masks = List.rev (List.map masks ~f:(fun mask -> Bits.reverse mask)) in

  let lfsr_masks_state_init = List.init lfsr_width ~f:(fun i -> (sll (one lfsr_width) i)) in
  let lfsr_masks_data_init = List.init lfsr_width ~f:(fun _ -> (zero data_width)) in 
  let (lfsr_masks_state, lfsr_masks_data) = loop 31 lfsr_masks_state_init lfsr_masks_data_init in
  
  if bit_reverse then (rev_masks lfsr_masks_state, rev_masks lfsr_masks_data) else (lfsr_masks_state, lfsr_masks_data)

let crc32 (type a) (module B : Comb.S with type t = a) state_in data_in =
  let open B in
  let (lfsr_masks_state, lfsr_masks_data) = gen_masks 0x04c11db7 32 32 true in

  let state_out_bits = List.map2_exn lfsr_masks_state lfsr_masks_data ~f:(fun state_mask data_mask ->
    let state_bits = state_in &: (B.constb (Hardcaml.Bits.to_bstr state_mask)) in
    let data_bits = data_in &: (B.constb (Hardcaml.Bits.to_bstr data_mask)) in
    let all_bits = bits_lsb (state_bits @: data_bits) in
    List.reduce_exn all_bits ~f:(^:))
  in
  B.concat_lsb state_out_bits

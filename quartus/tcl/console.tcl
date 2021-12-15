# Provides a hexdump procedure for a list of byte values

proc element { l i } {
  return [ lrange $l $i $i ]
}

proc hexdump { base_addr values } {
  set address 0
  while { 1 } {
    set bytes_per_line 16
    set current [ lrange $values $address [ expr $address + $bytes_per_line ] ]
    if { [ llength $current ] == 0 } {
      break
    }
    set hex_line ""
    set ascii_line ""
    for { set cur_byte 0 } { $cur_byte < $bytes_per_line } { incr cur_byte } {
      set byte [ element $current $cur_byte ]
      if { [ string length $byte ] == 0 } {
        break
      }
      if { 0 == [ expr $cur_byte % 8 ] } {
        append hex_line "" " "
        append ascii_line "" " "
      }
      append hex_line " " [ format {%02x} $byte ]
      if { $byte >= 0x20 && $byte < 0x7f } {
        set char [ binary format H2 [ format %02x $byte ] ]
        append ascii_line "" $char
      } else {
        append ascii_line "" "."
      }
    }
    puts stdout [format {%08x%-52s%-16s} [ expr $address + $base_addr ] $hex_line $ascii_line ]
    set address [ expr $address + $bytes_per_line ]

  }
}

# =================================================================
# JTAG access
# =================================================================
#
proc jtag_open {{index 0}} {
	global jtag

	# Close any open service
	if {[info exists jtag(master)]} {
		close_service master $jtag(master)
	}

	# Get the list of masters
	set masters [get_service_paths master]
	if {[llength $masters] == 0} {
		error "Error: No JTAG-to-Avalon-MM device found!"		
	}
	
	# Access the first master in the masters list
	set jtag(master) [lindex $masters $index]
	open_service master $jtag(master)

	return
}

proc jtag_close {} {
	global jtag

	if {[info exists jtag(master)]} {
		close_service master $jtag(master)
		unset jtag(master)
	}
	return
}

proc jtag_read {addr} {
	global jtag
	
	# Check the argument is a valid value by reformatting
	# the address as an 8-bit hex value
	set addr [expr {$addr & 0xFFFFFFFF}]
	if {[catch {format "0x%.8X" $addr} addr]} {
		error "Error: Invalid address\n -> '$addr'"
	}

	if {![info exists jtag(master)]} {
		jtag_open
	}

	# Read 32-bits
	if {[catch {master_read_32 $jtag(master) $addr 1} result]} {
		# JTAG connection lost?
		jtag_close
		error "Error: Check the JTAG interface\n -> '$result'"
	}
	return $result
}

proc jtag_hexdump {addr bytes_count} {
	global jtag

	# Check the argument is a valid value by reformatting
	# the address as an 8-bit hex value
	set addr [expr {$addr & 0xFFFFFFFF}]
	if {[catch {format "0x%.8X" $addr} addr]} {
		error "Error: Invalid address\n -> '$addr'"
	}

	if {![string is integer -strict $bytes_count]} {
		error "\"$bytes_count\" isn't a proper integer"
	}

	if {![info exists jtag(master)]} {
		jtag_open
	}

	# Read 32-bits
	if {[catch {master_read_memory $jtag(master) $addr $bytes_count} result]} {
		# JTAG connection lost?
		jtag_close
		error "Error: Check the JTAG interface\n -> '$result'"
	}

	hexdump $addr $result
	return
}

proc jtag_write {addr data} {
	global jtag

	# Check the arguments are valid values by reformatting
	# them as 8-bit hex values
	set addr [expr {$addr & 0xFFFFFFFF}]
	if {[catch {format "0x%.8X" $addr} addr]} {
		error "Error: Invalid address\n -> '$addr'"
	}
	set data [expr {$data & 0xFFFFFFFF}]
	if {[catch {format "0x%.8X" $data} data]} {
		error "Error: Invalid write data\n -> '$data'"
	}

	if {![info exists jtag(master)]} {
		jtag_open
	}

	# Write 32-bits
	if {[catch {master_write_32 $jtag(master) $addr [list $data]} result]} {
		# JTAG connection lost?
		jtag_close
		error "Error: Check the JTAG interface\n -> '$result'"
	}		
	return
}
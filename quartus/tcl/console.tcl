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
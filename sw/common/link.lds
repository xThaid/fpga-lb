OUTPUT_ARCH( "riscv" )
ENTRY( _start )

_STACK_SIZE = DEFINED(_STACK_SIZE) ? _STACK_SIZE : 0x1000;

MEMORY 
{
	imem : ORIGIN = 0x00000000, LENGTH = 0x8000
	dmem : ORIGIN = 0x00008000, LENGTH = 0x8000
}

SECTIONS
{
	.init :
	{
		_text = .;
		KEEP (*(SORT_NONE(.init)))
	} >imem
	
	.text :
	{
		*(.text.unlikely .text.unlikely.*)
		*(.text.startup .text.startup.*)
		*(.text .text.*)
		*(.gnu.linkonce.t.*)
	} >imem
	
	.fini :
	{
		KEEP (*(SORT_NONE(.fini)))
		_etext = .;
	} >imem

	.rodata : {
       __rodata_start = .;
	   *(.rdata)
       *(.rodata)
       *(.rodata.*)
       *(.gnu.linkonce.r.*)
       __rodata_end = .;
	} >dmem

	.data :
	{
		. = ALIGN(4);
		*(.data .data.*)
		*(.gnu.linkonce.d.*)
		. = ALIGN(8);
		__global_pointer$ = . + 0x800;
		*(.sdata .sdata.*)
		*(.sdata2 .sdata2.*)
		*(.gnu.linkonce.s.*)
		. = ALIGN(8);
		*(.srodata.cst16)
		*(.srodata.cst8)
		*(.srodata.cst4)
		*(.srodata.cst2)
		*(.srodata .srodata.*)

		. = ALIGN(4);
	} >dmem

	.bss :
	{
		. = ALIGN(4);
		__bss_start = .;
		*(.sbss*)
		*(.gnu.linkonce.sb.*)
		*(.bss .bss.*)
		*(.gnu.linkonce.b.*)
		*(COMMON)

		. = ALIGN(4);
		__bss_end = .;
	} >dmem

	.stack :
	{
		__stack_end = .;
		. += _STACK_SIZE;
		. = ALIGN(4);
		__stack_top = .;
	} >dmem

	_end = .;
}

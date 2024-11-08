INCLUDE memory.ld

SECTIONS
{
    .rom : {
        *(.bios)
    } > rom
    .ram : {
        *(.cold .warm)
    } > ram
    .bss : {
        . = ABSOLUTE(0xFE00);
        *(.bss)
    } > ram
}

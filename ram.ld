INCLUDE memory.ld

SECTIONS
{
    INCLUDE rc2014.ld
    .rom : {
        *(.bios)
    } > rom
    .ram : {
        *(.cold .warm)
    } > ram
    .bss : {
        . = ABSOLUTE(SYSTEM_TABLE);
        *(.system)
    } > ram
}

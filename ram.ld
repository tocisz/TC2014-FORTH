INCLUDE memory.ld

SECTIONS
{
    INCLUDE RC2014.ld
    .ram : {
        *(.cold .warm)
    } > ram
    .bss : {
        . = ABSOLUTE(SYSTEM_TABLE);
        *(.system)
    } > ram
}

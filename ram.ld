INCLUDE memory.ld

SECTIONS
{
    INCLUDE RC2014-no_blocks.ld
    .ram : {
        *(.cold .warm)
    } > ram
    .bss : {
        . = ABSOLUTE(SYSTEM_TABLE);
        *(.system)
    } > ram
}

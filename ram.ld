INCLUDE memory.ld

SECTIONS
{
    INCLUDE RC2014-with_basic.ld
    .ram : {
        *(.cold .warm)
    } > ram
    .bss : {
        . = ABSOLUTE(SYSTEM_TABLE);
        *(.system)
    } > ram
}

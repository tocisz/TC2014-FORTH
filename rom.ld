INCLUDE memory.ld

SECTIONS
{
    INCLUDE rc2014.ld
    .rom : {
        *(.bios)
        . = ABSOLUTE(COLD_START);
        *(.cold)
        . = ABSOLUTE(WARM_START);
        *(.warm)
    } > rom
    .ram : {
        . = ABSOLUTE(SYSTEM_TABLE);
        *(.system)
    } > ram
}

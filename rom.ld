INCLUDE memory.ld

SECTIONS
{
    INCLUDE RC2014-in_rom.ld
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

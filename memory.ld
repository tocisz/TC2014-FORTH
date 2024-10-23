MEMORY
{
    rom (rx) : ORIGIN = 0x0000, LENGTH = 0x2000 /* ROM memory */
    reserved (rx) : ORIGIN = 0x8000, LENGTH = 0x0400 /* BASIC, IO buffer, etc. */
    ram (wx) : ORIGIN = 0x8400, LENGTH = 0x7C00 /* RAM memory */
}

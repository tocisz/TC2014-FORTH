COLD_START	= 0x0150;
WARM_START	= COLD_START+3;

VOCAB_BASE	= 0xF000;	/* Dictionary grows up from here */
DATA_STACK	= 0xFD80;	/* Data stack grows down */
SYSTEM_TABLE	= 0xFE00;	/* SYSTEM table goes here */
MASS_STORE	= 0xFEA0;	/* Mass storage buffer (default) */
DISK_START	= 0xA000;	/* Pseudo disk buffer start */
DISK_END	= 0xF000;	/* Pseudo disk buffer end */
BLOCK_SIZE	= 0x0200;	/* Pseudo disk block size */
BUFFERS		= 0x0001;	/* Pseudo disk buffers per block */

MAX_DISK_BLOCKS = (DISK_END-DISK_START)/BLOCK_SIZE;
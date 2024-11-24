RAM_START	= 0x8000; /* Bottom of 32k RAM */
COLD_START	= 0x0150;
WARM_START	= COLD_START+3;

VOCAB_BASE	= 0xA000;	/* Dictionary grows up from here */
DATA_STACK	= 0xFD80;	/* Data stack grows down */
SYSTEM_TABLE	= 0xFE00;	/* SYSTEM table goes here */
MASS_STORE	= 0xFEA0;	/* Mass storage buffer (default) */
DISK_START	= 0xA000;	/* Pseudo disk buffer start */
DISK_END	= 0xA000;	/* Pseudo disk buffer end */
BLOCK_SIZE	= 0x0200;	/* Pseudo disk block size */
BUFFERS		= 0x0001;	/* Pseudo disk buffers per block */

MAX_DISK_BLOCKS = (DISK_END-DISK_START)/BLOCK_SIZE;

/* Interrupt vectors (offsets) for Z80 RST, INT0, and NMI interrupts */
VECTOR_BASE         =   RAM_START; /* RAM vector address for Z80 RST Table */

/* Squeezed between INT0 0x0038 and NMI 0x0066 */
VECTOR_PROTO        =   0x003C;
VECTOR_SIZE         =   0x24;

/* Buffers */
serRxInPtr      =     VECTOR_BASE+VECTOR_SIZE;
serRxOutPtr     =     serRxInPtr+2;
serTxInPtr      =     serRxOutPtr+2;
serTxOutPtr     =     serTxInPtr+2;
serRxBufUsed    =     serTxOutPtr+2;
serTxBufUsed    =     serRxBufUsed+1;
serControl      =     serTxBufUsed+1;
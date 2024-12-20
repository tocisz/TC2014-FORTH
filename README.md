# TC2014-FORTH

This Forth system is based on an old [fig-Forth derivative](https://github.com/rachel8973/RC2014-FORTH) for Zilog Z80.

I did some modifications I liked:
1. Don't mark end of word name with `+0x80`. You can use Unicode in word names if you like. It's (still) case sensitive unlike most of Forth systems.
2. All words are lowercase. I find it more aesthetically pleasing.
3. Compiled by the standard GNU Assembler.
4. Python is used as a macro generator. It can generate assembly from word definitions like `word("FIRST:first", ": ufirst @ ;s")`, which makes reading and modyfiying source code easier.
5. With Python used for generating word definitions it's possible to generate graph of dependencies between words.
6. Cleanup of hacks for defining vocabularies that were used in the Forth system on which it's based. Now `forth` is the last word and its copied to RAM, so `current` address can change witin it.
7. More standard `words` instead of `VLIST`. Added `vocs`.
8. Restore fig-Forth `create` and `<builds`.
9. Simpler `sp!` and `rp!`.
10. Linux terminal CLS.
11. Using IX for return stack pointer.
12. `sysdump` and `sysload` to dump and load system state.
13. Added `rdrop`, `cmove>` and `recurse`.

## RAM image

RAM image has base address `0x8400` and can be loaded by SCM or HLOAD enabled BASIC for RC2014.
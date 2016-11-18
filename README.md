IF THIS FILE HAS NO LINE BREAKS:  View it in a web browser. 

LoROM template
==============

This is a minimal working program for the Super Nintendo
Entertainment System using the LoROM (mode $20) mapper.

Concepts illustrated:

* internal header and init code
* setting up a static background
* loading data from multiple 32 KiB banks in a LoROM
* structure of a game loop
* automatic controller reading
* 8.8 fixed-point arithmetic
* acceleration-based character movement physics
* sprite drawing and animation, with horizontal flipping
* makefile-controlled conversion of graphics to tile data in
  both 2-bit-per-pixel and 4-bit-per-pixel formats
* booting the SPC700 audio CPU
* writing SPC700 code using 65C02 syntax using blargg's
  [SPC700 macro pack] for ca65
* basic use of SPC700 timers to control playback
* makefile-controlled compression of sampled sound to BRR format
* creating an SPC700 state file for SPC music players

Concepts not illustrated:

* checksum in internal header (yet)
* a 512-byte header for obsolete floppy-based copiers

[SPC700 macro pack]: http://forums.nesdev.com/viewtopic.php?p=121690#p121690

Setting up the build environment
--------------------------------
Building this demo requires cc65, Python, Pillow, GNU Make, and GNU
Coreutils.  For detailed instructions to set up a build environment,
see [nrom-template].

[nrom-template]: https://github.com/pinobatch/nrom-template

Organization of the program
---------------------------

### Include files

* `snes.inc`: Register definitions and useful macros for the S-CPU
* `global.inc`: S-CPU global variable and function declarations
* `spc-ca65.inc`: Macro library to produce SPC700 instructions
* `spc-65c02.inc`: Macro library to use 65C02 syntax with the SPC700

### Source code files (65816)

* `snesheader.s`: Super NES internal header
* `init.s`: PPU and CPU I/O initialization code
* `main.s`: Main program
* `bg.s`: Background graphics setup
* `player.s`: Player sprite graphics setup and movement
* `ppuclear.s`: Useful subroutines for interacting with the S-PPU
* `blarggapu.s`: Send a sound driver to the S-SMP 

### Source code files (SPC700)

* `spcheader.s`: Header for the `.spc` file; unused in `.sfc`
* `spcimage.s`: Sound driver

Each source code file is made up of subroutines that start with
`.proc` and end with `.endproc`.  See the [ca65 Users Guide] for
what these mean.

[ca65 Users Guide]: http://cc65.github.io/doc/ca65.html

The tools
---------
The `tools` folder contains a few essential command-line programs
written in Python to convert asset data (graphics, audio, etc.) into
a form usable by the Super NES.  The makefile contains instructions
to run these programs whenever the original asset data changes.

* `pilbmp2nes.py` converts bitmap images in PNG or BMP format
  into tile data usable by several classic video game consoles.
  It has several options to control the data format; use
  `pilbmp2nes.py --help` from the command prompt to see them all.
* `wav2brr.py` converts an uncompressed wave file to the BRR (bit
  rate reduction) format, a lossy audio codec based on ADPCM used
  by the S-DSP (the audio chip in the Super NES).
* `mktables.py` generates some lookup tables, such as the
  pitch table to be used by a music engine.  
* `karplus.py` generates a plucked string sound, used for the
  bass sample.

Greets
------

* [Super Nintendo Development Wiki] contributors
* Martin Korth (nocash) for [Fullsnes] doc and [NO$SNS] emulator
* Shay Green (blargg) for APU examples and SPC700 macro pack
* Jeremy Chadwick (koitsu) for more code organization tips

[Super Nintendo Development Wiki]: http://wiki.superfamicom.org/
[Fullsnes]: http://problemkaputt.de/fullsnes.htm
[NO$SNS]: http://problemkaputt.de/sns.htm

Legal
-----
The demo is distributed under the following license, based on the
GNU All-Permissive License:

> Copyright 2014-2016 Damian Yerrick
> 
> Copying and distribution of this file, with or without
> modification, are permitted in any medium without royalty provided
> the copyright notice and this notice are preserved in all source
> code copies.  This file is offered as-is, without any warranty.

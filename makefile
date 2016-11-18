#!/usr/bin/make -f
#
# Makefile for LoROM template
# Copyright 2014-2015 Damian Yerrick
#
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#

# These are used in the title of the SFC program and the zip file.
title = lorom-template
version = 0.06

# Space-separated list of asm files without .s extension
# (use a backslash to continue on the next line)
objlist = \
  snesheader init main bg player \
  ppuclear blarggapu spcimage
objlistspc = \
  spcheader spcimage

AS65 = ca65
LD65 = ld65
CFLAGS65 = 
objdir = obj/snes
srcdir = src
imgdir = tilesets

# if it's not bsnes, it's just BS
EMU = ../shared-tools/bsnes

# game-music-emu by blargg et al.
SPCPLAY = ../shared-tools/gme_player

# Calculate the current directory as Wine applications would see it.
# yep, that's 8 backslashes.  Apparently, there are 3 layers of escaping:
# one for the shell that executes sed, one for sed, and one for the shell
# that executes wine
wincwd := $(shell pwd | sed -e "s'/'\\\\\\\\'g")

# .PHONY means these targets aren't actual filenames
.PHONY: all run nocash-run spcrun dist clean

# Per Martin Korth on 2014-09-16: NO$SNS requires absolute
# paths because he screwed up and made the filename processing
# too clever.
nocash-run: $(title).sfc
	wine "C:\\Program Files\\nocash\\no\$$sns.exe" "Z:$(wincwd)\\$(title).sfc"

# When you type make without a target name, make will try
# to build the first target.  So unless you're trying to run
# NO$SNS in Wine, you should move run above nocash-run.
run: $(title).sfc
	$(EMU) $<

# Special target for just the SPC700 image
spcrun: $(title).spc
	$(SPCPLAY) $<

all: $(title).sfc $(title).spc

clean:
	-rm $(objdir)/*.o $(objdir)/*.chr $(objdir)/*.ov53 $(objdir)/*.sav $(objdir)/*.pb53 $(objdir)/*.s

dist: zip
zip: $(title)-$(version).zip
$(title)-$(version).zip: zip.in all README.md $(objdir)/index.txt
	zip -9 -u $@ -@ < $<

# Build zip.in from the list of files in the Git tree
zip.in:
	git ls-files | grep -e "^[^.]" > $@
	echo zip.in >> $@

$(objdir)/index.txt: makefile
	echo "Files produced by build tools go here. (This file's existence forces the unzip tool to create this folder.)" > $@

# Rules for ROM

objlisto = $(foreach o,$(objlist),$(objdir)/$(o).o)
objlistospc = $(foreach o,$(objlistspc),$(objdir)/$(o).o)

map.txt $(title).sfc: lorom256k.cfg $(objlisto)
	$(LD65) -o $(title).sfc -m map.txt -C $^

spcmap.txt $(title).spc: spc.cfg $(objlistospc)
	$(LD65) -o $(title).spc -m map.txt -C $^

$(objdir)/%.o: $(srcdir)/%.s $(srcdir)/snes.inc $(srcdir)/global.inc
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/%.o: $(objdir)/%.s
	$(AS65) $(CFLAGS65) $< -o $@

$(objdir)/mktables.s: tools/mktables.py
	$< > $@

# Files that depend on .incbin'd files
$(objdir)/bg.o: \
 $(objdir)/bggfx.chrgb
$(objdir)/player.o: \
 $(objdir)/swinging2.chrsfc
$(objdir)/spcimage.o: \
 $(objdir)/selnow.brr $(objdir)/karplusbassloop.brr

# Rules for CHR data

# .chrgb (CHR data for Game Boy) denotes the 2-bit tile format
# used by Game Boy and Game Boy Color, as well as Super NES
# mode 0 (all planes), mode 1 (third plane), and modes 4 and 5
# (second plane).
$(objdir)/%.chrgb: tilesets/%.png
	tools/pilbmp2nes.py --planes=0,1 $< $@

$(objdir)/%.chrsfc: tilesets/%.png
	tools/pilbmp2nes.py "--planes=0,1;2,3" $< $@

# Rules for audio
$(objdir)/%.brr: audio/%.wav
	tools/wav2brr.py $< $@
$(objdir)/%.brr: $(objdir)/%.wav
	tools/wav2brr.py $< $@
$(objdir)/%loop.brr: audio/%.wav
	tools/wav2brr.py --loop $< $@
$(objdir)/%loop.brr: $(objdir)/%.wav
	tools/wav2brr.py --loop $< $@
$(objdir)/karplusbass.wav: tools/karplus.py
	$< -o $@ -n 1024 -p 64 -r 4186 -e square:1:4 -a 30000 -g 1.0 -f .5

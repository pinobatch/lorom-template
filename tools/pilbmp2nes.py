#!/usr/bin/env python3
#
# Bitmap to multi-console CHR converter using Pillow, the
# Python Imaging Library
#
# Copyright 2014-2025 Damian Yerrick
# Copying and distribution of this file, with or without
# modification, are permitted in any medium without royalty
# provided the copyright notice and this notice are preserved.
# This file is offered as-is, without any warranty.
#
from __future__ import with_statement, print_function, unicode_literals
from PIL import Image
from time import sleep

def formatTilePlanar(tile, planemap, hflip=False, little=False):
    """Turn a tile into bitplanes.

Planemap opcodes:
10 -- bit 1 then bit 0 of each tile
0,1 -- planar interleaved by rows
0;1 -- planar interlaved by planes
0,1;2,3 -- SNES/PCE format

"""
    hflip = 7 if hflip else 0
    if (tile.size != (8, 8)):
        return None
    pixels = list(tile.getdata())
    pixelrows = [pixels[i:i + 8] for i in range(0, 64, 8)]
    if hflip:
        for row in pixelrows:
            row.reverse()
    out = bytearray()

    planemap = [[[int(c) for c in row]
                 for row in plane.split(',')]
                for plane in planemap.split(';')]
    # format: [tile-plane number][plane-within-row number][bit number]

    # we have five (!) nested loops
    # outermost: separate planes
    # within separate planes: pixel rows
    # within pixel rows: row planes
    # within row planes: pixels
    # within pixels: bits
    for plane in planemap:
        for pxrow in pixelrows:
            for rowplane in plane:
                rowbits = 1
                thisrow = bytearray()
                for px in pxrow:
                    for bitnum in rowplane:
                        rowbits = (rowbits << 1) | ((px >> bitnum) & 1)
                        if rowbits >= 0x100:
                            thisrow.append(rowbits & 0xFF)
                            rowbits = 1
                out.extend(thisrow[::-1] if little else thisrow)
    return bytes(out)

def pilbmp2chr(im, tileWidth=8, tileHeight=8,
               formatTile=lambda im: formatTilePlanar(im, "0;1")):
    """Convert a bitmap image into a list of byte strings representing tiles."""
    im.load()
    (w, h) = im.size
    outdata = []
    for mt_y in range(0, h, tileHeight):
        for mt_x in range(0, w, tileWidth):
            metatile = im.crop((mt_x, mt_y,
                                mt_x + tileWidth, mt_y + tileHeight))
            for tile_y in range(0, tileHeight, 8):
                for tile_x in range(0, tileWidth, 8):
                    tile = metatile.crop((tile_x, tile_y,
                                          tile_x + 8, tile_y + 8))
                    data = formatTile(tile)
                    outdata.append(data)
    return outdata

def parse_argv(argv):
    # optparse is still used because of how it supports mixing
    # named arguments (-i and -o) with positional arguments
    from optparse import OptionParser
    parser = OptionParser(usage="usage: %prog [options] [-i] INFILE [-o] OUTFILE")
    parser.add_option("-i", "--image", dest="infilename",
                      help="read image from INFILE", metavar="INFILE")
    parser.add_option("-o", "--output", dest="outfilename",
                      help="write CHR data to OUTFILE", metavar="OUTFILE")
    parser.add_option("-W", "--tile-width", dest="tileWidth",
                      help="set width of metatiles", metavar="HEIGHT",
                      type="int", default=8)
    parser.add_option("--packbits", dest="packbits",
                      help="use Apple PackBits RLE compression if available",
                      action="store_true", default=False)
    parser.add_option("--pb8", dest="use_pb8",
                      help="use PB8 RLE compression if available",
                      action="store_true", default=False)
    parser.add_option("-H", "--tile-height", dest="tileHeight",
                      help="set height of metatiles", metavar="HEIGHT",
                      type="int", default=8)
    parser.add_option("-1", dest="planes",
                      help="set 1bpp mode (default: 2bpp NES)",
                      action="store_const", const="0", default="0;1")
    parser.add_option("-p", "--planes", dest="planes",
                      help="set the plane map (1bpp: 0) (NES: 0;1) (GB: 0,1) (SMS:0,1,2,3) (TG16/SNES: 0,1;2,3) (MD: 3210)")
    parser.add_option("--hflip", dest="hflip",
                      help="horizontally flip all tiles (most significant pixel on right)",
                      action="store_true", default=False)
    parser.add_option("--little", dest="little",
                      help="reverse the bytes within each row-plane (needed for GBA and a few others)",
                      action="store_true", default=False)
    parser.add_option("--add", dest="addamt",
                      help="value to add to each pixel",
                      type="int", default=0)
    parser.add_option("--add0", dest="addamt0",
                      help="value to add to pixels of color 0 (if different)",
                      type="int", default=None)
    (options, args) = parser.parse_args(argv[1:])

    tileWidth = int(options.tileWidth)
    if tileWidth <= 0:
        raise ValueError("tile width '%d' must be positive" % tileWidth)

    tileHeight = int(options.tileHeight)
    if tileHeight <= 0:
        raise ValueError("tile height '%d' must be positive" % tileHeight)

    # Fill unfilled roles with positional arguments
    argsreader = iter(args)
    try:
        infilename = options.infilename
        if infilename is None:
            infilename = next(argsreader)
    except StopIteration:
        raise ValueError("not enough filenames")

    outfilename = options.outfilename
    if outfilename is None:
        try:
            outfilename = next(argsreader)
        except StopIteration:
            outfilename = '-'
    if outfilename == '-':
        import sys
        if sys.stdout.isatty():
            raise ValueError("cannot write CHR to terminal")

    addamt, addamt0 = options.addamt, options.addamt0
    if addamt0 is None: addamt0 = addamt

    return (infilename, outfilename, tileWidth, tileHeight,
            options.planes, options.hflip, options.little,
            options.packbits, options.use_pb8, addamt, addamt0)

argvTestingMode = True

def make_stdout_binary():
    """Ensure that sys.stdout is in binary mode, with no newline translation."""

    # Recipe from
    # http://code.activestate.com/recipes/65443-sending-binary-data-to-stdout-under-windows/
    # via http://stackoverflow.com/a/2374507/2738262
    if sys.platform == "win32":
        import os, msvcrt
        msvcrt.setmode(sys.stdout.fileno(), os.O_BINARY)

def main(argv=None):
    import sys
    if argv is None:
        argv = sys.argv
        if (argvTestingMode and len(argv) < 2
            and sys.stdin.isatty() and sys.stdout.isatty()):
            argv.extend(input('args:').split())
    try:
        (infilename, outfilename, tileWidth, tileHeight,
         planes, hflip, little,
         usePackBits, use_pb8, addamt, addamt0) = parse_argv(argv)
    except Exception as e:
        sys.stderr.write("%s: %s\n" % (argv[0], str(e)))
        sys.exit(1)

    im = Image.open(infilename)
    im.load()
    if len(im.getbands()) > 1:
        print("%s: %s: image must be indexed (not mode %s)"
              % (argv[0], infilename, im.mode), file=sys.stderr)
        sys.exit(1)

    # Subpalette shift
    if addamt or addamt0:
        px = bytearray(im.getdata())
        for i in range(len(px)):
            thispixel = px[i]
            px[i] = thispixel + (addamt if thispixel else addamt0)
        im.putdata(px)

    outdata = pilbmp2chr(im, tileWidth, tileHeight,
                         lambda im: formatTilePlanar(im, planes, hflip, little))
    outdata = b''.join(outdata)
    im = None
    if use_pb8:
        from pb8 import pb8
        outdata = pb8(outdata)
    elif usePackBits:
        from packbits import PackBits
        sz = len(outdata) % 0x10000
        outdata = PackBits(outdata).flush().tobytes()
        outdata = b''.join([chr(sz >> 8), chr(sz & 0xFF), outdata])

    # Write output file
    outfp = None
    try:
        if outfilename != '-':
            outfp = open(outfilename, 'wb')
        else:
            outfp = sys.stdout
            make_stdout_binary()
        outfp.write(outdata)
    finally:
        if outfp and outfilename != '-':
            outfp.close()

if __name__=='__main__':
    main()
##    main(['pilbmp2nes.py', '../tilesets/char_pinocchio.png', 'char_pinocchio.chr'])
##    main(['pilbmp2nes.py', '--packbits', '../tilesets/char_pinocchio.png', 'char_pinocchio.pkb'])

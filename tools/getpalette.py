#!/usr/bin/env python3
"""
Spits out the indexed palette of an image in some format

Copyright 2019 Damian Yerrick
Copying and distribution of this file, with or without
modification, are permitted in any medium without royalty
provided the copyright notice and this notice are preserved.
This file is offered as-is, without any warranty.
"""
import sys
import argparse
from PIL import Image

def get_bgr555(rgbtuples):
    return [
        ((b & 0xF8) << 7) | ((g & 0xF8) << 2) | ((r & 0xF8) >> 3)
        for r, g, b in rgbtuples
    ]

def format_bgr555_le(rgbtuples):
    out = bytearray()
    for word in get_bgr555(rgbtuples):
        out.append(word & 0xFF)
        out.append((word >> 8) & 0xFF)
    return bytes(out)

def format_wordsperline(words, prefix="", n=8):
    lines = []
    wordsperline = 8
    for i in range(0, len(words), wordsperline):
        lines.append("%s%s\n" % (prefix, ",".join(words[i:i + wordsperline])))
    return "".join(lines)

def format_bgr555_ca65(rgbtuples):
    words = ["$%04x" % r for r in get_bgr555(rgbtuples)]
    return format_wordsperline(words, " .word ", 8)

def format_bgr555_rgbasm(rgbtuples):
    words = ["$%04x" % r for r in get_bgr555(rgbtuples)]
    return format_wordsperline(words, " dw ", 8)

def format_hextriplet(rgbtuples):
    return "".join("#%02x%02x%02x\n" % r for r in rgbtuples)

formats = {
    "bgr555-le": format_bgr555_le,
    "bgr555-ca65": format_bgr555_ca65,
    "bgr555-rgbasm": format_bgr555_rgbasm,
    "hextriplet": format_hextriplet
}

def parse_slice(s):
    s = s.split(':', 1)
    if not all(x.isdigit() for x in s):
        raise ValueError("nondigits in slice %s" % args.slice)
    rside = int(s[-1])
    lside = int(s[0]) if len(s) >= 2 else 0
    out = (lside, rside)
    if lside > rside:
        raise ValueError("slice %d:%d not ascending" % out)
    if rside >= 256:
        raise ValueError("slice %d:%d beyond 256" % out)
    return out

def parse_argv(argv):
    p = argparse.ArgumentParser()
    formatchoices = sorted(formats)
    p.add_argument("image",
                   help="half-open interval of palette entries to keep (e.g. 3:9 for 3, 4, ..., 8)")
    p.add_argument("-f", "--format",
                   help="choose format",
                   choices=formatchoices, default="hextriplet")
    p.add_argument("-n", "--slice",
                   help="count or half-open interval of palette entries to write (e.g. 8 for 0, 1, 2, ..., 7; or 3:11 for 3, 4, 5, ..., 10)",
                   type=parse_slice, default=(0, 256))
    p.add_argument("-o", "--output",
                   help="write (default: standard output)",
                   default="-")
    return p.parse_args(argv[1:])

def main(argv=None):
    args = parse_argv(argv or sys.argv)
    im = Image.open(args.image)
    if im.mode != 'P':
        raise ValueError("%s: does not use indexed color" % args.image)
    
    palette = im.getpalette()
    palette = [tuple(palette[i:i + 3]) for i in range(0, len(palette), 3)]
    palette = palette[args.slice[0]:args.slice[1]]

    out = formats[args.format](palette)
    out_byteslike = hasattr(out, "decode")
    if args.output == '-':
        if out_byteslike:
            sys.stdout.buffer.write(out)
        else:
            sys.stdout.write(out)
    else:
        with open(args.output, "wb" if out_byteslike else "w") as outfp:
            outfp.write(out)

if __name__=='__main__':
##    main(["getpalette.py", "../tilesets/EWJ2cover.png", "-n", "48", "-f", "bgr555-ca65"])
    main()


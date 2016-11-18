.setcpu "none"
.include "spc-65c02.inc"

.macro stya addr
.local addr1
addr1 = addr
  movw addr, ya
.endmacro

TIMEREN     = $F1  ; 0-2: enable timer; 7: enable ROM in $FFC0-$FFFF
DSPADDR     = $F2
DSPDATA     = $F3
TIMERPERIOD = $FA  ; Divisors for timers (0, 1: 8 kHz base; 2: 64 kHz base)
TIMERVAL    = $FD  ; Number of times timer incremented (bits 3-0; cleared on read)

DSP_CLVOL    = $00
DSP_CRVOL    = $01
DSP_CFREQLO  = $02  ; Playback frequency in 7.8125 Hz units
DSP_CFREQHI  = $03  ; (ignored 
DSP_CSAMPNUM = $04  
DSP_CATTACK  = $05  ; 7: set; 6-4: decay rate; 3-0: attack rate
DSP_CSUSTAIN = $06  ; 7-5: sustain level; 4-0: sustain decay rate
DSP_CGAIN    = $07  ; Used only when attack is disabled

DSP_LVOL     = $0C
DSP_RVOL     = $1C
DSP_LECHOVOL = $2C
DSP_RECHOVOL = $3C
DSP_KEYON    = $4C
DSP_KEYOFF   = $5C
DSP_FLAGS    = $6C  ; 5: disable echo; 4-0: set LFSR rate
DSP_NOISECH  = $3D  ; Replace these channels with LFSR noise
DSP_ECHOCH   = $4D  ; Echo comes from these channels
DSP_SAMPDIR  = $5D  ; High byte of base address of sample table

.export sample_dir, spc_entry

.segment "SPCZEROPAGE"
; Writing to KON or KOFF twice within a 64-cycle (2 sample) window
; is unreliable.  So writes are collected into bitmask regs.
kon_ready: .res 1
koff_ready: .res 1

; We want to KOFF the notes, wait a bit for them to be faded out,
; and KON the new notes a little later.
ready_notes: .res 8
ready_insts: .res 8

.segment "SPCIMAGE"
.align 256
sample_dir:
  ; each directory entry is 4 bytes:
  ; a start address then a loop address
  .addr pulse_8, pulse_8
  .addr pulse_4, pulse_4
  .addr pulse_2, pulse_2
  .addr tri_wave, tri_wave
  .addr karplusbass, karplusbass_loop
  .addr testsample, testsample

inst_dir:


pulse_2:
  .byte $B0,$9B,$BB,$BB,$BB,$BB,$BB,$BB,$B9
  .byte $B3,$75,$55,$55,$55,$55,$55,$55,$57
pulse_4:
  .byte $B0,$9B,$BB,$BB,$B9,$75,$55,$55,$55
  .byte $B3,$55,$55,$55,$55,$55,$55,$55,$57
pulse_8:
  .byte $B0,$9B,$B9,$75,$55,$55,$55,$55,$55
  .byte $B3,$55,$55,$55,$55,$55,$55,$55,$57
tri_wave:
  ; The triangle wave is intentionally distorted slightly because the
  ; SPC700's Gaussian interpolator has a glitch with three
  ; consecutive 8<<12 samples.
  .byte $C0,$00,$11,$22,$33,$44,$55,$66,$76
  .byte $C0,$77,$66,$55,$44,$33,$22,$11,$00
  .byte $C0,$FF,$EE,$DD,$CC,$BB,$AA,$99,$89
  .byte $C3,$88,$99,$AA,$BB,$CC,$DD,$EE,$FF
karplusbass:
    .incbin "obj/snes/karplusbassloop.brr"
karplusbass_loop = * - 36  ; period 64 samples (36 bytes)
testsample:
    .incbin "obj/snes/selnow.brr"

; round(261.625 * 8 / 7.8125 * 2^(i / 12)) - 256
notefreqs_lowbyte:
  .byte  12,  28,  45,  63,  82, 102
  .byte 123, 145, 169, 195, 221, 250
one_shl_x:
  .repeat 8, I
    .byte 1 << I
  .endrepeat

  nop  ; resync debugger's disassembly
spc_entry:
  ldy #$7F
  lda #DSP_LVOL  ; master volume left
  stya DSPADDR
  lda #DSP_RVOL  ; master volume right
  stya DSPADDR
  ; Disable the APU features we're not using
  ldy #%00100000  ; mute off, echo write off, LFSR noise stop
  lda #DSP_FLAGS
  stya DSPADDR
  ldy #$00
  lda #DSP_KEYON  ; Clear key on
  stya DSPADDR
  dey
  lda #DSP_KEYOFF  ; Key off everything
  stya DSPADDR
  iny
  lda #DSP_NOISECH  ; LFSR noise on no channels
  stya DSPADDR
  lda #DSP_ECHOCH  ; Echo on no channels
  stya DSPADDR
  lda #DSP_LECHOVOL  ; Left echo volume = 0
  stya DSPADDR
  lda #DSP_RECHOVOL  ; Right echo volume = 0
  stya DSPADDR
  
  ; The DSP acts on KON and KOFF only once every two samples (1/16000
  ; seconds or 64 cycles).  A key on or key off request must remain
  ; set for at least 64 cycles, but key off must be cleared before
  ; key on can be set again.  If key on is set while key off is set,
  ; it'll immediately cut the note and possibly cause a pop.
  ldx #7
  lda #$FF
:
  sta ready_notes,x
  dex
  bpl :-

  lda #DSP_KEYOFF  ; Now clear key off request (must not happen within
  stya DSPADDR
  
  lda #DSP_SAMPDIR  ; set sample directory start address
  ldy #>sample_dir
  stya DSPADDR

  lda #80  ; 8000 Hz base / 160 = 100 Hz
  sta TIMERPERIOD
  lda #%10000001
  sta TIMEREN
  lda TIMERVAL

  ldx #1
  lda #PLING_2
  jsr ch_set_inst
  ldy #1*12+10
  jsr ch_set_pitch

  ; play the first note
  lda #DSP_KEYON
  ldy #%00000010  ; channel 1
  stya DSPADDR

  ldx #10
:
  lda TIMERVAL
  beq :-
  dex
  bne :-

  ldx #0
  lda #PLING_8
  jsr ch_set_inst
  ldy #3*12+2
  jsr ch_set_pitch

  ldx #2
  lda #BASSGUITAR
  jsr ch_set_inst
  ldy #0*12+10
  jsr ch_set_pitch

  lda #DSP_KEYON
  ldy #%00000101  ; channels 0 and 2
  stya DSPADDR


  ; make your selection now
  lda #DSP_CLVOL|$30
  ldy #127
  stya DSPADDR
  lda #DSP_CRVOL|$30
  ldy #127
  stya DSPADDR
  lda #DSP_CFREQLO|$30
  ldy #$00
  stya DSPADDR
  lda #DSP_CFREQHI|$30
  ldy #$08
  stya DSPADDR
  lda #DSP_CSAMPNUM|$30
  ldy #$05
  stya DSPADDR
  lda #DSP_CATTACK|$30
  ldy #$00
  stya DSPADDR
  lda #DSP_CGAIN|$30
  ldy #$4F
  stya DSPADDR

  ldx #90  ; 10 + 90 = 100 centiseconds
:
  lda TIMERVAL
  beq :-
  dex
  bne :-
  lda #DSP_KEYON
  ldy #%00001000  ; channel 3
  stya DSPADDR

forever:
  jmp forever


; PLAYING NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Instrument format
; $00: Left volume (bit 7-4) and right volume (bit 3-0),
;      used for balance and panning
; $01: Length of period in 16-sample units
; $02: Sample number
; $03: Attack and decay rates
; $04: Sustain threshold and rate
; $05-$07: Not used yet
.macro INST name, lvol, rvol, freqscale, samplenum, attack, decay, sustain, sustainlevel
name = (* - music_inst_table) / 8
  .assert (0 <= (lvol) && (lvol) <= 15), error, "left volume must be 0-15"
  .assert (0 <= (rvol) && (rvol) <= 15), error, "right volume must be 0-15"
  .assert (0 <= (rvol) && (rvol) <= 15), error, "right volume must be 0-15"
  .assert (1 <= (attack) && (attack) <= 31), error, "attack must be 1-31"
  .assert (attack & 1), error, "attack must be odd"
  .assert (17 <= (decay) && (decay) <= 31), error, "decay must be 1-31"
  .assert (decay & 1), error, "decay must be odd"
  .assert (0 <= (sustain) && (sustain) <= 31), error, "sustain must be 0-31"
  .assert (1 <= (sustainlevel) && (sustainlevel) <= 8), error, "sustainlevel must be 1-8"
  .byte ((lvol) << 4) | (rvol)
  .byte freqscale
  .byte samplenum
  .byte ((attack) >> 1) | (((decay) >> 1) << 4)
  .byte (sustain) | (((sustainlevel) - 1) << 5)
  .byte $00,$00,$00
.endmacro

music_inst_table:
  ;    name         lv  rv frq smp att dec sus lvl
  INST PLING_8,      3, 15,  1,  0, 27, 21, 15,  2
  INST PLING_4,     11, 11,  1,  1, 27, 21, 15,  2
  INST PLING_2,     15,  3,  1,  2, 27, 21, 15,  2
  INST BASSGUITAR,  12, 12,  1,  4, 29, 17, 15,  8

;;
; Sets the frequency of channel X to A times the frequency for note Y.
; Frequency 0 represents 2093 Hz, which when used with a 32-sample
; wave represents a C two octaves below middle C.
; If A * 2^(Y/12) >= 64, the playback frequency is unspecified.
.proc ch_set_pitch
pitchhi=0
  sta pitchhi
  txa
  xcn a
  ora #DSP_CFREQLO
  sta DSPADDR
  tya
octave_loop:
  cmp #12
  bcc octave_done
  sbc #12
  asl pitchhi
  jmp octave_loop
octave_done:
  tay
  lda notefreqs_lowbyte,y
  ldy pitchhi
  mul ya
  sta DSPDATA
  inc DSPADDR
  ;clc  ; octave_loop always ends with carry clear
  tya
  adc pitchhi
  sta DSPDATA
  rts
.endproc

;;
; Sets volume, sample ID, and envelope of channel X to instrument A.
; @param X channel number (0-7)
; @param A instrument number
; @return $02: pointer to instrument
.proc ch_set_inst
instptr = $02
  ldy #8
  mul ya
  clc
  adc #<music_inst_table
  sta instptr
  tya
  adc #>music_inst_table
  sta instptr+1
  txa
  xcn a
  sta DSPADDR
  
  ; Set left channel volume
  ldy #0
  lda (instptr),y
  pha
  ora #$0F
  ldy #127  ; FIXME: use channel volume
  mul ya
  sty DSPDATA

  ; Set right channel volume
  inc DSPADDR
  pla
  xcn a
  ora #$0F
  ldy #127  ; FIXME: use channel volume
  mul ya
  sty DSPDATA

  ; Set sample ID and envelope
  lda DSPADDR
  eor #DSP_CSAMPNUM^DSP_CRVOL
  sta DSPADDR
  ldy #2
  lda (instptr),y
  sta DSPDATA
  inc DSPADDR
  iny
  lda (instptr),y
  sta DSPDATA
  inc DSPADDR
  iny
  lda (instptr),y
  sta DSPDATA

  ; Return the frequency scale
  ldy #1
  lda (instptr),y
  rts
.endproc


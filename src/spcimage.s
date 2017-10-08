.setcpu "none"
.include "spc-65c02.inc"

.macro stya addr
.local addr1
addr1 = addr
  .assert addr <= $00FE, error, "stya works only in zero page"
  movw <addr, ya
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
DSP_FMCH     = $2D  ; Modulate these channels' frequency by the amplitude before it
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
noteInstrument: .res 8

patternptrlo: .res 8
patternptrhi: .res 8
noteRowsLeft: .res 8
musicPattern: .res 8
patternTranspose: .res 8
noteLegato: .res 8

music_tempoLo: .res 1
music_tempoHi: .res 1
tempo_counter: .res 2
conductorPos: .res 2
conductorSegnoLo: .res 1
conductorSegnoHi: .res 1
conductorWaitRows: .res 1

TIMER_HZ = 125

.segment "SPCIMAGE"
.align 256
sample_dir:
  ; each directory entry is 4 bytes:
  ; a start address then a loop address
  .addr pulse_8_brr, pulse_8_brr
  .addr pulse_4_brr, pulse_4_brr
  .addr pulse_2_brr, pulse_2_brr
  .addr triangle_brr, triangle_brr
  .addr karplusbassloop_brr, karplusbass_loop
  .addr hat_brr, hat_brr
  .addr kick_brr, kick_brr
  .addr snare_brr, snare_brr

  nop  ; resync debugger's disassembly
.align 256
spc_entry:
  jsr pently_init
  lda #0
  jsr pently_start_music

nexttick:
  :
    lda TIMERVAL
    beq :-
  jsr pently_update
  jmp nexttick

; CONDUCTOR TRACK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc pently_init
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
  sty music_tempoLo
  sty music_tempoHi
  lda #DSP_KEYON  ; Clear key on
  stya DSPADDR
  lda #DSP_FMCH   ; Clear frequency modulation
  stya DSPADDR

  dey
  lda #DSP_KEYOFF  ; Key off everything
  stya DSPADDR
  sty tempo_counter
  sty tempo_counter+1
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
  sta <ready_notes,x
  dex
  bpl :-

  lda #DSP_KEYOFF
  stya DSPADDR
  
  lda #DSP_SAMPDIR  ; set sample directory start address
  ldy #>sample_dir
  stya DSPADDR

  lda #8000/TIMER_HZ  ; S-Pently will use 125 Hz
  sta TIMERPERIOD
  lda #%10000001
  sta TIMEREN
  lda TIMERVAL
  rts
.endproc

.proc pently_start_music
  pha
  ; Start the song
  lda #<300
  ldy #>300
  stya music_tempoLo
  lda #0
  sta conductorWaitRows

  pla
  asl a
  tax
  lda pently_songs,x
  sta conductorPos
  sta conductorSegnoLo
  lda pently_songs+1,x
  sta conductorPos+1
  sta conductorSegnoHi
  ; falls through
.endproc

.proc pently_stop_all_tracks
  ldx #7
  initpatternloop:
    lda #<silentPattern
    sta <patternptrlo,x
    lda #>silentPattern
    sta <patternptrhi,x
    lda #$FF
    sta musicPattern,x
    lda #0
    sta <noteRowsLeft,x
    dex
    bpl initpatternloop
  rts
.endproc

.proc pently_update
  jsr keyon_ready_notes
  clc
  lda music_tempoLo
  adc tempo_counter
  sta tempo_counter
  lda music_tempoHi
  adc tempo_counter+1
  sta tempo_counter+1
  bcs is_next_row
    rts
  is_next_row:

  ; New row time!
  lda tempo_counter
  sec
  sbc #<(60 * TIMER_HZ)
  sta tempo_counter
  lda tempo_counter+1
  sec
  sbc #>(60 * TIMER_HZ)
  sta tempo_counter+1

  jsr play_conductor
  ldx #7
  :
    jsr play_pattern_row
    dex
    bpl :-
  jmp keyoff_ready_notes
.endproc

.proc play_conductor
  lda <conductorWaitRows
  beq doConductor
    dec <conductorWaitRows
    rts
  doConductor:

  ldy #0
  lda (<conductorPos),y
  inc <conductorPos
  bne :+
    inc <conductorPos+1
  :
  cmp #CON_SETTEMPO
  bcc @notTempoChange
    ; Removed: BPM math
    and #%00000111
    sta music_tempoHi
  
    lda (<conductorPos),y
    inc <conductorPos
    bne :+
      inc <conductorPos+1
    :
    sta music_tempoLo
    jmp doConductor
  @notTempoChange:
  cmp #CON_WAITROWS
  bcc conductorPlayPattern
  bne @notWaitRows
    jmp conductorDoWaitRows
  @notWaitRows:

  ; Removed: attack track
  ; Removed for now: note on

  @notAttackSet:

  cmp #CON_FINE
  bne @notFine
    lda #0
    sta <music_tempoHi
    sta <music_tempoLo
    ; Removed: dal segno callback
    jmp pently_stop_all_tracks
  @notFine:

  cmp #CON_SEGNO
  bne @notSegno
    lda <conductorPos
    sta <conductorSegnoLo
    lda <conductorPos+1
    sta <conductorSegnoHi
    jmp doConductor
  @notSegno:

  cmp #CON_DALSEGNO
  bne @notDalSegno
    lda <conductorSegnoLo
    sta <conductorPos
    lda <conductorSegnoHi
    sta <conductorPos+1
    ; Removed: dal segno callback
    jmp doConductor
  @notDalSegno:
  rts

conductorPlayPattern:
  and #$07
  tax

  ; Removed handling for attack track, as S-DSP doesn't really
  ; support restarting a note already in progress
  lda #0
  sta <noteLegato,x  ; start all patterns with legato off
  skipClearLegato:
  sta <noteRowsLeft,x
  lda (<conductorPos),y
  sta <musicPattern,x
  iny
  lda (<conductorPos),y
  sta <patternTranspose,x
  iny
  lda (<conductorPos),y
  sta <noteInstrument,x
  tya
  sec
skipAplusCconductor:
  adc <conductorPos
  sta <conductorPos
  bcc :+
    inc <conductorPos+1
  :
  jsr startPattern
  jmp doConductor

  ; this should be last so it can fall into skipConductor
conductorDoWaitRows:
  lda (<conductorPos),y
  inc <conductorPos
  bne :+
    inc <conductorPos+1
  :
  sta <conductorWaitRows
  rts
.endproc

; PLAYING PATTERNS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

durations: .byte 1, 2, 3, 4, 6, 8, 12, 16

.proc startPattern
  lda #0
  sta noteRowsLeft,x
  lda musicPattern,x
  cmp #255
  bcc @notSilentPattern
    lda #<silentPattern
    sta <patternptrlo,x
    lda #>silentPattern
    sta <patternptrhi,x
    rts
  @notSilentPattern:
  asl a
  tay
  bcc @isLoPattern
    lda pently_patterns+256,y
    sta <patternptrhi,x
    lda pently_patterns+257,y
    sta <patternptrhi,x
    rts
  @isLoPattern:
  lda pently_patterns,y
  sta <patternptrlo,x
  lda pently_patterns+1,y
  sta <patternptrhi,x
  rts
.endproc

.proc play_pattern_row
patternptr = $00

  lda noteRowsLeft,x
  beq notWaitingRows
    sec
    sbc #1
    sta noteRowsLeft,x
    rts
  notWaitingRows:

  lda patternptrlo,x
  sta patternptr
  lda patternptrhi,x
  sta patternptr+1

anotherPatternByte:
  ldy #0
  lda (patternptr),y
  cmp #$FF
  bcc notStartPatternOver
    jsr startPattern
    bra notWaitingRows
  notStartPatternOver:
  inc patternptr
  bne :+
    inc patternptr+1
  :

  cmp #INSTRUMENT
  bcc isNoteCmd
    ; TODO: Handle effects
    jmp anotherPatternByte
  isNoteCmd:

  pha
  and #$07
  tay
  lda durations,y
  sta noteRowsLeft,x
  pla
  lsr a
  lsr a
  lsr a
  cmp #25
  bcc isNoteOn
  beq skipNote
    ; TODO: Handle noteoff
    lda #$7F
    sta <ready_notes,x
    jmp skipNote
  isNoteOn:
  
  ldy <patternTranspose,x
  bpl noteOnNotDrum

    ; Drum mode: play a middle C with instrument X
    sta <noteInstrument,x
    lda #2*12+0
    sta <ready_notes,x
    bra skipNote
  noteOnNotDrum:
  clc
  adc <patternTranspose,x
  sta <ready_notes,x  ; Using previous instrument

skipNote:
  lda noteRowsLeft,x
  sec
  sbc #1
  sta noteRowsLeft,x
patDone:
  lda patternptr
  sta patternptrlo,x
  lda patternptr+1
  sta patternptrhi,x
  rts
.endproc

silentPattern: .byte $D7, $FF

; PLAYING NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
; Sets the frequency of channel X to A times the frequency for note Y.
; Frequency 0 represents 2093 Hz, which when used with a 32-sample
; wave represents a C two octaves below middle C.
; If A * 2^(Y/12) >= 64, the playback frequency is unspecified.
; @param X voice ID (0-7)
; @param A pitch scale (BRR blocks per period)
; @param Y semitone number
; @return A, Y, $00 trashed; X preserved
.proc ch_set_pitch
pitchhi = $00
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
; @return $02: pointer to instrument; A: instrument's pitch scale;
; Y trashed; X preserved
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

;;
; Calculate which notes are ready to be keyed off.
; Sending a key off command takes 2 samples (64 cycles), which
; starts a fade-out process that takes up to 256 samples (8 ms).
; So find which voices are ready for key off and on, and do so.
.proc keyoff_ready_notes
unready_chs = $07
  ldx #7
  loop:
    ; Bit 7 of ready_notes is 1 for no keyoff or 0 for keyoff.
    lda ready_notes,x
    asl a
    rol unready_chs
    dex
    bpl loop
  lda #DSP_KEYOFF
  sta DSPADDR
  lda unready_chs
  eor #$FF
  sta DSPDATA
  rts
.endproc

.proc keyon_ready_notes
unready_chs = $07
  ldx #7
  loop:
    ; Bit 7 of ready_notes is 1 for no keyon or 0 for keyon.
    ; $78-$7F are invalid notes used to key off without keying on.
    lda ready_notes,x
    cmp #$78
    bcs not_ready
      lda noteInstrument,x
      jsr ch_set_inst
      pha
      lda ready_notes,x
      tay
      pla
      jsr ch_set_pitch

      clc
    not_ready:
    rol unready_chs

    ; Acknowledge key on
    lda #$FF
    sta ready_notes,x
    dex
    bpl loop
  lda #DSP_KEYOFF
  ldy #0
  stya DSPADDR
  lda #DSP_KEYON
  sta DSPADDR
  lda unready_chs
  eor #$FF
  sta DSPDATA
  rts
.endproc

; Sample data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pulse_2_brr:
  .byte $B0,$9B,$BB,$BB,$BB,$BB,$BB,$BB,$B9
  .byte $B3,$75,$55,$55,$55,$55,$55,$55,$57
pulse_4_brr:
  .byte $B0,$9B,$BB,$BB,$B9,$75,$55,$55,$55
  .byte $B3,$55,$55,$55,$55,$55,$55,$55,$57
pulse_8_brr:
  .byte $B0,$9B,$B9,$75,$55,$55,$55,$55,$55
  .byte $B3,$55,$55,$55,$55,$55,$55,$55,$57
triangle_brr:
  ; The triangle wave is intentionally distorted slightly because the
  ; SPC700's Gaussian interpolator has a glitch with three
  ; consecutive 8<<12 samples.
  .byte $C0,$00,$11,$22,$33,$44,$55,$66,$76
  .byte $C0,$77,$66,$55,$44,$33,$22,$11,$00
  .byte $C0,$FF,$EE,$DD,$CC,$BB,$AA,$99,$89
  .byte $C3,$88,$99,$AA,$BB,$CC,$DD,$EE,$FF
karplusbassloop_brr:
    .incbin "obj/snes/karplusbassloop.brr"
karplusbass_loop = * - 36  ; period 64 samples (36 bytes)
hat_brr:
    .incbin "obj/snes/hat.brr"
kick_brr:
    .incbin "obj/snes/kickgen.brr"
snare_brr:
    .incbin "obj/snes/decentsnare.brr"

; $ python3
; [round(261.625 * 8 / 7.8125 * 2**(i / 12)) - 256 for i in range(12)]
notefreqs_lowbyte:
  .byte 12, 28, 45, 63, 82, 102, 123, 145, 169, 195, 221, 250
one_shl_x:
  .repeat 8, I
    .byte 1 << I
  .endrepeat

; Music data definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Instrument format
; $00: Left volume (bit 7-4) and right volume (bit 3-0),
;      used for balance and panning
; $01: Length of period in 16-sample units, or note 24 freq / 4186 Hz
; $02: Sample number
; $03: Attack and decay rates
; $04: Sustain threshold and rate
; $05-$07: Not used yet
.macro INST name, lvol, rvol, freqscale, samplenum, attack, decay, sustain, sustainlevel
name = (* - music_inst_table) / 8
.ident(.concat("PD_", .string(name))) = (* - music_inst_table)
  .assert (0 <= (lvol) && (lvol) <= 15), error, "left volume must be 0-15"
  .assert (0 <= (rvol) && (rvol) <= 15), error, "right volume must be 0-15"
  .assert (0 <= (rvol) && (rvol) <= 15), error, "right volume must be 0-15"
  .assert (1 <= (attack) && (attack) <= 31), error, "attack must be 1-31"
  .assert (attack & 1), error, "attack must be odd"
  .assert (16 <= (decay) && (decay) <= 31), error, "decay must be 17-31"
  .assert (0 <= (sustain) && (sustain) <= 31), error, "sustain must be 0-31"
  .assert (1 <= (sustainlevel) && (sustainlevel) <= 8), error, "sustainlevel must be 1-8"
  .byte ((lvol) << 4) | (rvol)
  .byte freqscale
  .byte samplenum
  .byte ((attack) >> 1) | (((decay) >> 1) << 4)
  .byte (sustain) | (((sustainlevel) - 1) << 5)
  .byte $00,$00,$00
.endmacro

CON_PLAYPAT = $00   ; next: pattern, transpose, instrument
CON_WAITROWS = $20  ; next: number of rows to wait minus 1
CON_FINE = $21      ; stop music now
CON_SEGNO = $22     ; set loop point
CON_DALSEGNO = $23  ; jump to loop point. if no point was set, jump to start of song.
CON_NOTEON = $28
CON_SETTEMPO = $30  ; low bits: bits 10-8 of tempo in rows/min; next: bits 7-0 of tempo

; Conductor macros
.macro playPat ch, patid, transpose, instrument
  .byt CON_PLAYPAT|ch, patid, transpose, instrument
.endmacro
.macro playDrumPat ch, patid
  .byt CON_PLAYPAT|ch, patid, $80, $80
.endmacro
.macro stopPat ch
  .byt CON_PLAYPAT|ch, 255, 0, 0
.endmacro
.macro waitRows n
  .byt CON_WAITROWS, (n)-1
.endmacro
.macro fine
  .byt CON_FINE
.endmacro
.macro segno
  .byt CON_SEGNO
.endmacro
.macro dalSegno
  .byt CON_DALSEGNO
.endmacro
.macro setTempo rowsPerMin
.local irpm
irpm = rowsPerMin
  .byt CON_SETTEMPO|>irpm, <irpm
.endmacro
.macro noteOn ch, notenum, instrument
  .byt CON_NOTEON|ch, notenum, instrument
.endmacro

; Pattern commands
N_C  =  0*8
N_CS =  1*8
N_D  =  2*8
N_DS =  3*8
N_E  =  4*8
N_F  =  5*8
N_FS =  6*8
N_G  =  7*8
N_GS =  8*8
N_A  =  9*8
N_AS = 10*8
N_B  = 11*8
N_DB = N_CS
N_EB = N_DS
N_GB = N_FS
N_AB = N_GS
N_BB = N_AS
N_CH  = N_C  + 12*8
N_CSH = N_CS + 12*8
N_DBH = N_DB + 12*8
N_DH  = N_D  + 12*8
N_DSH = N_DS + 12*8
N_EBH = N_EB + 12*8
N_EH  = N_E  + 12*8
N_FH  = N_F  + 12*8
N_FSH = N_FS + 12*8
N_GBH = N_GB + 12*8
N_GH  = N_G  + 12*8
N_GSH = N_GS + 12*8
N_ABH = N_AB + 12*8
N_AH  = N_A  + 12*8
N_ASH = N_AS + 12*8
N_BBH = N_BB + 12*8
N_BH  = N_B  + 12*8
N_CHH = N_CH + 12*8
N_TIE = 25*8
REST  = 26*8
INSTRUMENT = $D8
ARPEGGIO = $D9
LEGATO_OFF = $DA
LEGATO_ON = $DB
TRANSPOSE = $DC
GRACE = $DD
VIBRATO = $DE
CHVOLUME = $DF
PATEND = $FF

; The default duration is one row (a sixteenth note in the tracker).
; OR the pitch with one of these constants.
D_8  = 1
D_D8 = 2
D_4  = 3
D_D4 = 4
D_2  = 5
D_D2 = 6
D_1  = 7

; Music data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: Make a version of pentlyas.py that can output this format

music_inst_table:
  ;    name         lv  rv frq smp att dec sus lvl
  INST KICK,        13, 13,  3,  6, 31, 17,  0,  8
  INST SNARE,       10, 15,  3,  7, 31, 17,  0,  8
  INST HAT,          3,  6,  3,  5, 31, 17,  0,  8
  INST PLING_8,      4, 11,  1,  0, 31, 19, 15,  2
  INST PLING_4,      8,  8,  1,  1, 31, 19, 15,  2
  INST PLING_2,     10,  6,  1,  2, 31, 17, 15,  2
  INST BASSGUITAR,  12, 12,  1,  4, 31, 17, 15,  8
  INST BASSCLAR,    15,  3,  1,  2, 21, 21, 27,  4

pently_patterns:
  .addr PPDAT_0200_sq1, PPDAT_0200_sq2, PPDAT_0200_bass, PPDAT_0200_drums
  ; pattern 4: cleared
  .addr PPDAT_round_clear_sq1

.addr PPDAT_0300_drum, PPDAT_0300_bass1, PPDAT_0300_bass2, PPDAT_0300_sq2_1
.addr PPDAT_0300_sq1_1, PPDAT_0300_sq2_2

pently_songs:
.addr PSDAT_0200, PSDAT_round_clear, PSDAT_0300


;____________________________________________________________________
; 2am theme
; This is the famous first eight bars of the second movement of
; Beethoven's "Pathetique" done in 9/8 like ACWW 2am.

PSDAT_0200:
  setTempo 300
  playPat 0, 0, 24, 4
  playPat 1, 1, 24, 4
  playPat 2, 2, 12, 4
  waitRows 36
  playDrumPat 3, 3
  waitRows 144
  dalSegno

PPDAT_0200_sq1:
  .byt REST|D_D8, N_GS|D_D4, REST|D_D8, N_FS|D_D4
  .byt REST|D_D8, N_GS|D_D4, REST|D_D8, N_FS|D_D4
  .byt REST|D_D8, N_GS|D_D4, REST|D_D8, N_FS|D_D4
  .byt REST|D_D8, N_GS|D_D4, REST|D_D8, N_FS|D_D4
  .byt REST|D_D8, N_A|D_D4, REST|D_D8, N_B|D_D8, N_DSH|D_D8
  .byt REST|D_D8, N_GS|D_D4, REST|D_D8, N_GS|D_D4
  .byt REST|D_D8, N_GS|D_D4, REST|D_D8, N_GS|D_D4
  .byt REST|D_D8, N_A|D_D4, REST|D_D8, N_E|D_D4
  .byt REST|D_D8, N_FS|D_D4, REST|D_D8, N_D|D_D4
  .byt REST|D_D8, N_D|D_D4, REST|D_D8, N_CS|D_D4
  .byt 255

PPDAT_0200_sq2:
  .byt REST|D_D8, N_CSH|D_D4, REST|D_D8, N_B|D_D4
  .byt REST|D_D8, N_CSH|D_D4, REST|D_D8, N_B|D_D4
  .byt N_CSH|D_2, REST, N_B|D_2, REST
  .byt N_EH|D_2, N_TIE, REST|D_D4, N_DH|D_D8
  .byt N_CSH|D_4, N_TIE, N_EH|D_4, N_AH|D_D8, N_BH|D_D4
  .byt N_EH|D_2, N_TIE, REST|D_D4, N_FH|D_D8
  .byt N_FSH|D_2, REST, N_B|D_D4, N_CSH|D_8, N_DH
  .byt N_EH|D_2, REST, N_AS|D_2, REST
  .byt N_DH|D_2, REST, N_CSH|D_8, N_B|D_D8, N_A|D_D8, N_GS
  .byt N_B|D_2, REST, N_A|D_2, REST
  .byt 255

PPDAT_0200_bass:
  .byt N_A|D_2, REST, N_G|D_2, REST
  .byt N_A|D_2, REST, N_G|D_2, REST
  .byt N_A|D_2, REST, N_G|D_2, REST
  .byt N_A|D_2, REST, N_G|D_2, REST
  .byt N_FS|D_2, REST, N_B|D_2, REST
  .byt N_E|D_2, REST, N_E|D_2, REST
  .byt N_DH|D_2, REST, N_DH|D_2, REST
  .byt N_CSH|D_2, REST, N_FS|D_2, REST
  .byt N_B|D_2, REST, N_E|D_2, REST
  .byt N_A|D_2, REST, N_A|D_2, REST
  .byt 255

PPDAT_0200_drums:
  .byt PD_KICK|D_D8, PD_HAT|D_8, PD_HAT, PD_HAT|D_D8, PD_SNARE|D_D8, PD_HAT|D_D4
  .byt 255

;____________________________________________________________________
; 3am theme

PSDAT_0300:
  playDrumPat 0, 5
  playPat 3, 6, 0, BASSCLAR
  waitRows 48
  playPat 1, 8, 24, PLING_2
  playPat 2, 9, 24, PLING_2
  waitRows 96
  playPat 3, 7, 0, BASSCLAR
  playPat 1, 10, 19, PLING_2
  playPat 2, 10, 24, PLING_2
  waitRows 96
  stopPat 1
  stopPat 2
  dalSegno

PPDAT_0300_drum:
  .byt PD_KICK|D_D8, PD_HAT|D_8, PD_KICK, PD_SNARE|D_8, PD_HAT, PD_HAT|D_8, PD_HAT
  .byt PD_KICK|D_8, PD_HAT, PD_HAT|D_8, PD_KICK, PD_SNARE|D_D8, PD_HAT|D_D8
  .byt PATEND

PPDAT_0300_bass1:
  .byt N_E|D_8, REST, N_EH|D_8, N_E|D_8, REST, N_EH|D_8, REST|D_8
  .byt REST|D_D2
  .byt N_A|D_8, REST, N_AH|D_8, N_A|D_8, REST, N_AH|D_8, REST|D_8
  .byt REST|D_D2
  .byt PATEND

PPDAT_0300_bass2:
  .byt N_B|D_8, REST, N_BH|D_8, N_B|D_8, REST, N_BH|D_8, REST|D_8
  .byt REST|D_D2
  .byt N_A|D_8, REST, N_AH|D_8, N_A|D_8, REST, N_AH|D_8, REST|D_8
  .byt REST|D_D2
  .byt PATEND

PPDAT_0300_sq2_1:
  .byt N_EH|D_D2, N_DH|D_D2, N_CSH|D_D2, N_CH|D_D2
  .byt PATEND

PPDAT_0300_sq1_1:
  .byt N_CSH|D_D2, N_B|D_D2, N_A|D_D2, N_G|D_D2
  .byt PATEND

PPDAT_0300_sq2_2:
  .byt REST|D_D8, N_B|D_4, N_TIE, N_DH|D_4
  .byt N_CSH|D_4, N_TIE, N_A|D_4, N_TIE|D_D8
  .byt REST|D_D8, N_A|D_4, N_TIE, N_CH|D_4
  .byt N_B|D_4, N_TIE, N_G|D_4, N_TIE|D_D8
  .byt PATEND


;____________________________________________________________________
; round cleared theme

PSDAT_round_clear:
  setTempo 60
  playPat 0, 4, 12, PLING_2
  playPat 1, 4, 21, PLING_2
  waitRows 5
  fine

PPDAT_round_clear_sq1:
  .byt N_F, N_A, N_G, N_C|D_8
  .byt 255

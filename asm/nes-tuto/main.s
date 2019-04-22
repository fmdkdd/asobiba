;;
;; iNES header
;;

.segment "HEADER"

INES_MAPPER = 0               ; 0 = NROM
INES_MIRROR = 1               ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0               ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A        ; ID
.byte $02                       ; 16k PRG chunk count
.byte $01                       ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;;
;; CHR ROM
;;

.segment "TILES"
.incbin "background.chr"
.incbin "sprite.chr"

;;
;; vectors placed at top 6 bytes of memory area
;;

.segment "VECTORS"
.word nmi
.word reset
.word irq

;;
;; reset routine
;;

.segment "CODE"
reset:
	sei                     ; mask interrupts
	lda #0
	sta $2000               ; disable NMI
	sta $2001               ; disable rendering
	sta $4015               ; disable APU sound
	sta $4010               ; disable DMC IRQ
	lda #$40
	sta $4017               ; disable APU IRQ
	cld                     ; disable decimal mode
	ldx #$FF
	txs                     ; initialize stack

	;; wait for first vblank
	bit $2002
:
	bit $2002
	bpl :-
	;; clear all RAM to 0
	lda #0
	ldx #0
:
	sta $0000, X
	sta $0100, X
	sta $0200, X
	sta $0300, X
	sta $0400, X
	sta $0500, X
	sta $0600, X
	sta $0700, X
	inx
	bne :-
	;; wait for second vblank
:
	bit $2002
	bpl :-
	;; NES is initialized, ready to begin!

        ;; load palettes
        lda $2002               ; reset latch
	lda #$3F
	sta $2006
        lda #0
	sta $2006               ; set PPU address to $3F00
	ldx #0
:
	lda palette, X
	sta $2007
	inx
	cpx #32
	bcc :-

        ;; load hardcoded sprites data
        ldx #0
:
        lda sprites, X
        sta $0200, X
        inx
        cpx #16
        bcc :-

        ;; load background
        lda $2002               ; reset latch
        lda #$20
        sta $2006
        lda #$00
        sta $2006

        lda #6
        ldx #0
:
        sta $2007
        inx
        cpx #$ff
        bcc :-

        ;; set attributes
        lda $2002               ; reset latch
        lda #$23
        sta $2006
        lda #$c0
        sta $2006

	ldx #0
:
        lda #%11100100
        sta $2007
        inx
        cpx #$10
        bcc :-

        lda #%10000000          ; enable NMI
        sta $2000

        ;; loop
:
        jmp :-



.segment "OAM"
oam: .res 256                   ; sprite OAM data to be uploaded by DMA

.segment "CODE"
nmi:
	;; sprite OAM DMA
	ldx #0
	stx $2003
	lda #>oam
	sta $4014

        ;; count up for palette swap
        inc $01
        lda $01
        lsr a
        and #3
        sta $00

        ;; jiggle sprite 0 up and down
        lda $204
        sec
        sbc #2
        clc
        adc $00
        sta $200

        ;; read controller 1
        lda #1
        sta $4016
        lda #0
        sta $4016

        ;; player 1 - A
        lda $4016
        and #1
        beq :+
input_a:
        ;; palette swap
	lda $00
        sta $202
        clc
        adc #1
        sta $206
        clc
        adc #1
        sta $20a
        clc
        adc #1
        sta $20e
:
        ;; player 1 - B
        lda $4016
        and #1
        beq :+
input_b:
:
        ;; player 1 - Select
        lda $4016
        and #1
        beq :+
input_select:
:
        ;; player 1 - Start
        lda $4016
        and #1
        beq :+
input_start:
:
        ;; player 1 - Up
        lda $4016
        and #1
        beq :+
input_up:
        ;; move up
	lda $204
        sec
        sbc #1
        sta $200
        sta $204
        sta $208
        sta $20c
:
        ;; player 1 - Down
        lda $4016
        and #1
        beq :+
input_down:
        ;; move down
        lda $204
        clc
        adc #1
        sta $200
        sta $204
        sta $208
        sta $20c
:
        ;; player 1 - Left
        lda $4016
        and #1
        beq :+
input_left:
        ;; move left
	lda $203
        sec
        sbc #1
        sta $203
        clc
        adc #8
        sta $207
        clc
        adc #8
        sta $20b
        clc
        adc #8
        sta $20f
:
        ;; player 1 - Right
        lda $4016
        and #1
        beq :+
input_right:
	;; move right
	lda $203
        clc
        adc #1
        sta $203
        clc
        adc #8
        sta $207
        clc
        adc #8
        sta $20b
        clc
        adc #8
        sta $20f
:

        lda #%10000000 ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
        sta $2000
        lda #%00011110 ; enable sprites, enable background, no clipping on left side
        sta $2001
        lda $00
        sta $2005
        lda #0
        sta $2005

        rti

;;
;; irq
;;

.segment "CODE"
irq:
	rti

.segment "RODATA"
palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$09,$19,$29 ; bg1 green
.byte $0F,$01,$11,$21 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$18,$28,$38 ; sp0 yellow
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine

sprites:
.byte $80,$04,$00,$80
.byte $80,$04,$01,$88
.byte $80,$04,$02,$90
.byte $80,$04,$03,$98

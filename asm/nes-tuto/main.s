;
; iNES header
;

.segment "HEADER"

INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;
; CHR ROM
;

.segment "TILES"
.incbin "background.chr"
.incbin "sprite.chr"

;
; vectors placed at top 6 bytes of memory area
;

.segment "VECTORS"
.word nmi
.word reset
.word irq

;
; reset routine
;

.segment "CODE"
reset:
	sei       ; mask interrupts
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; disable APU sound
	sta $4010 ; disable DMC IRQ
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack

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



load_palettes:
        lda $2002 ; reset PPU status
	lda #$3F
	sta $2006
        lda #0
	sta $2006 ; set PPU address to $3F00
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

        lda #%10000000          ; enable NMI
        sta $2000

        lda #%00010000
        sta $2001

        ;; palette swap
        lda #0
        :
                sta $0202
                sta $0203
                adc #1
                sta $0206
	        adc #1
                sta $020a
	        adc #1
                sta $020e
                adc #1
                jmp :-



.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "CODE"
nmi:
	; sprite OAM DMA
	ldx #0
	stx $2003
	lda #>oam
	sta $4014
        rti

;
; irq
;

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

;
; end of file
;

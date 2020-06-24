	processor 6502
	include "vcs.h"
	include "macro.h"

;-----------------------------------
;Constants
;-----------------------------------
;
SCANLINES = 192
BALL_SIZE = 2

;Colors
TURQUOISE = $BA
BRONZE = $28
DARK_BLUE = $70
BROWN = $22
GREEN = $CC

;Testing
TEST_PF	= $FF

;Heart
HEART_X = 150	;Heart UI Element X Variable
HEART_Y = 20	;Heart UI Element Y Variable (Unused)

;-----------------------------------
;Variables
;-----------------------------------
	
	SEG.U VARS
	ORG $80

;Player Position
player_x	ds 1
player_y	ds 1

;Ball Position
ball_x	ds 1
ball_y	ds 1

;Draws
player_draw	ds 1
ball_draw	ds 1

;Pointers
player_pointer	ds.w 1
ball_pointer	ds.w 1
;-----------------------------------
;End of Variables
;-----------------------------------

;-----------------------------------
;Main Game Code
;-----------------------------------

	SEG CODE
	ORG $F000
	;Position Object along Screen
PosObj
	sec
	sta WSYNC
	
	;Divide by 15
Div15
	sbc #15
	bcs Div15
	
	eor #7
	asl
	asl
	asl
	asl
	
	sta RESP1,x
	sta HMOVE
	rts
	
	;Code executed upon hitting the Reset button
Reset
	CLEAN_START

	ldx #TURQUOISE
	stx COLUBK
	
	ldx #BRONZE
	stx COLUP0	
Frame
	
    ;VBLANK processing
    lda #0
    sta VBLANK

    lda #2
    sta VSYNC

	;VSYNCH
	REPEAT 3
	sta WSYNC
	REPEND
	
	lda #0
	sta VSYNC
	
	; Vertical Blank
	ldy #35
VerticalBlank	
    sta WSYNC
	dey
	bne VerticalBlank

;----------------------------------
;End Vertical Sync
;----------------------------------

;----------------------------------
;Controller Input
;----------------------------------

	;Player Input
	lda #%10000000
	bit SWCHA
	beq RIGHT_player

	lda #%01000000
	bit SWCHA
	beq LEFT_player
	
	lda #%00100000
	bit SWCHA
	beq DOWN_player
	
	lda #%00010000
	bit SWCHA
	beq UP_player
	
	jmp END_INPUT
	
RIGHT_player:
	inc player_x
    jmp END_INPUT
LEFT_player:
	dec player_x
    jmp END_INPUT
DOWN_player:
	dec player_y
    jmp END_INPUT    
UP_player:
	inc player_y
    jmp END_INPUT

END_INPUT
	;Move Player along X axis
	lda player_x
	ldx #0
	jsr PosObj
	
	lda #HEART_X
	ldx #0
	jsr PosObj
	
	;Set Wait Sync and use HMOVE to create fine positioning
	sta WSYNC
	sta HMOVE
	
;--------------------------------
;Create a two-line Kernel
;--------------------------------

	;Player Draw Data
	lda #(SCANLINES + PLAYER_HEIGHT)
	sec
	sbc player_y
	sta player_draw
	;Player Pointer
	lda #<(PLAYER_SPRITE + PLAYER_HEIGHT - 1)
	sec
	sbc player_y
	sta player_pointer
	lda #>(PLAYER_SPRITE + PLAYER_HEIGHT - 1)
	sbc #0
	sta player_pointer+1
	sta WSYNC
	
	;
	
;----------------------------------
;Start of Visible Screen (192 Scanlines)
;----------------------------------

	;Draw Player Color
	ldx #BRONZE
	stx COLUP0
	stx COLUP1
;--------------------------------
	
	ldy #9
Heart
	lda HEART_FRAME_1 -2,Y
	sta WSYNC
	
	sta GRP1
	
	dey
	bne Heart	
	
	lda #0
	sta GRP1
	
	REPEAT 183
	sta WSYNC
	REPEND
;--------------------------------
	lda #%01000010
	sta VBLANK

;---------------------------------
;End of Visible Screen
;---------------------------------

	;Overscan
	ldy #30
Overscan
	sta WSYNC
	dey
	bne Overscan

	jmp Frame
;----------------------------------
;Start of Data Tables
;----------------------------------

;Sprites
PLAYER_SPRITE
        .byte #%01100110;--
        .byte #%00100100;--
        .byte #%10011000;--
        .byte #%01111110;--
        .byte #%00011001;--
        .byte #%00100100;--
        .byte #%01011010;--
        .byte #%00111100;-
PLAYER_HEIGHT = * - PLAYER_SPRITE 	


;Heart Animation

HEART_FRAME_1
        .byte #%00011000;--
        .byte #%00100100;--
        .byte #%01000010;--
        .byte #%10000001;--
        .byte #%10000001;--
        .byte #%10000001;--
        .byte #%10011001;--
        .byte #%01100110;--
HEART_HEIGHT = * - HEART_FRAME_1	

;----------------------------------
;End of Data Storage
;----------------------------------
	ORG $FFFA
;-----------------------------------
;End Code
;-----------------------------------
    .word Reset          ; NMI
    .word Reset          ; RESET
    .word Reset          ; IRQ
	
	END

				RSRESET
ContiguousBitMap_Planes		RS.L	1
ContiguousBitMap_PlaneModulo	RS.L	1
ContiguousBitMap_RowModulo	RS.L	1
ContiguousBitMap_NumPlanes	RS.W	1
				RS.W	1
				

	SECTION	code,code

;----------------------------------------------------------------------------
; CPU-based bitmap blit routine
;
; This routine copies a rectangular region from source to destination bitmap.
; Both bitmaps should be in planar format and with the same number of planes.
; It will perform only aligned 32bit reads/writes against memory.
; Memory accesses are organized to suit copying fastmem->chipmem.
;
; Current version is a draft; it will read the entire target buffer even when
;  all bits there will get overwritten.
;
;
; Inputs:
; 	a0	source ContiguousBitMap
; 	a1	dest ContiguousBitMap
; 	d0.w	source x0
; 	d1.w	source y0
; 	d2.w	source w (must be >0)
; 	d3.w	source h (must be >0)
; 	d4.w	target x0
; 	d5.w	target y0
;
; Outputs:
;  All registers trashed.

PlanarCpuBlit
	tst.w	d2
	bmi	.skip
	move.w	ContiguousBitMap_NumPlanes(a0),d6
	move.w	d3,.numRows
	bmi	.skip
	move.w	d6,.numPlanes

	move.l	ContiguousBitMap_RowModulo(a0),d6
	and.l	#$ffff,d1
	move.l	ContiguousBitMap_RowModulo(a1),d7
	and.l	#$ffff,d5
	mulu.l	d6,d1
	mulu.l	d7,d5
	moveq	#0,d6
	moveq	#0,d7
	move.w	d0,d6
	move.w	d4,d7
	and.w	#$1f,d0
	and.w	#$1f,d4
	eor.w	d0,d6
	eor.w	d4,d7
	lsr.w	#3,d6
	lsr.w	#3,d7
	add.l	d6,d1
	add.l	d7,d5
	
	move.l	ContiguousBitMap_PlaneModulo(a0),a2
	moveq	#0,d3
	move.l	ContiguousBitMap_PlaneModulo(a1),a3
	move.l	a2,d6
	move.w	ContiguousBitMap_NumPlanes(a0),d3
	move.l	a3,d7
	mulu.l	d3,d6
	mulu.l	d3,d7
	neg.l	d6
	neg.l	d7
	add.l	ContiguousBitMap_RowModulo(a0),d6
	add.l	ContiguousBitMap_RowModulo(a1),d7
	move.l	d6,.sourceRowDelta
	move.l	d7,.targetRowDelta

	move.l	ContiguousBitMap_Planes(a0),a0
	move.l	ContiguousBitMap_Planes(a1),a1
	add.l	d1,a0
	add.l	d5,a1

	move.w	d0,d1
	move.w	d4,d5
	subq.w	#1,d2
	add.w	d2,d1
	add.w	d2,d5

; d0.w	source x0
; d1.w	source (x1-1)
; d4.w	target x0
; d5.w	target (x1-1)

	moveq	#$1f,d6
	move.w	d4,d2
	move.w	d5,d3
	lsr.w	#5,d2
	lsr.w	#5,d3
	sub.w	d2,d3
	and.w	d6,d0
	and.w	d6,d1
	and.w	d6,d4
	and.w	d6,d5

; d0.w	source x0 & $1f
; d1.w	source (x1-1) & $1f
; d3.w	number of longwords to output per row in target - 1
; d4.w	target x0 & $1f
; d5.w	target (x1-1) & $1f
; d6.w	$1f
	
	move.w	d5,d2
	moveq	#-1,d7
	eor.w	d6,d2
	moveq	#-1,d6
	lsl.l	d2,d7
	lsr.l	d4,d6

; d0.w	source x0 & $1f
; d1.w	source (x1-1) & $1f
; d3.w	number of longwords to output per row in target - 1
; d4.w	target x0 & $1f
; d5.w	target (x1-1) & $1f
; d6.l	fwm
; d7.l	lwm
	
	sub.w	d0,d4
	sub.w	d1,d5

	add.l	d5,d5
	move.w	d4,d5
	add.l	d5,d5
	swap	d5
	and.w	#3,d5

	and.w	#$1f,d4
	
; a0	source pixels
; a1	dest pixels

; d0.w	source x0 & $1f
; d3.w	number of longwords to output per row in target - 1
; d4.w	start bit delta & $1f
; d5.w	bit delta code (0..3)
;		bit 0: 1 = negative start bit delta
;		bit 1: 1 = negative end bit delta
; d6.l	fwm
; d7.l	lwm
;----------

	subq.w	#1,d3
	bmi	.oneLongwordOutput

;----------------------------------------------------------------------------

.multipleLongwordOutput
	move.l	d7,-(sp)
	move.w	d4,d7

	jmp	.multipleLongwordOutput_rotateInput_jumpTable(pc,d5.w*4)
.multipleLongwordOutput_rotateInput_jumpTable
	bra.w	.multipleLongwordOutput_rotateInput_firstLong0_lastLong01
	bra.w	.multipleLongwordOutput_rotateInput_firstLong01_lastLong01
	bra.w	.multipleLongwordOutput_rotateInput_firstLong0_lastLong0
	bra.w	.multipleLongwordOutput_rotateInput_firstLong01_lastLong0
	
	
.multipleLongwordOutput_rotateInput_firstLong0_lastLong01

	move.l	d3,-(sp)
	move.l	d6,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput
	addq.l	#4,a0
	move.l	(sp)+,d3
	beq.s	.multipleLongwordOutput_rotateInput_firstLong0_lastLong01_nMiddleLongs

.multipleLongwordOutput_rotateInput_firstLong0_lastLong01_middleLongs

	move.l	d3,-(sp)
	moveq	#-1,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput
	addq.l	#4,a0

	move.l	(sp)+,d3
	subq.w	#1,d3
	bne.s	.multipleLongwordOutput_rotateInput_firstLong0_lastLong01_middleLongs
.multipleLongwordOutput_rotateInput_firstLong0_lastLong01_nMiddleLongs
	
	move.l	(sp)+,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput

	rts

.multipleLongwordOutput_rotateInput_firstLong0_lastLong0

	move.l	d3,-(sp)
	move.l	d6,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput
	addq.l	#4,a0
	move.l	(sp)+,d3
	beq.s	.multipleLongwordOutput_rotateInput_firstLong0_lastLong0_nMiddleLongs

.multipleLongwordOutput_rotateInput_firstLong0_lastLong0_middleLongs

	move.l	d3,-(sp)
	moveq	#-1,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput
	addq.l	#4,a0

	move.l	(sp)+,d3
	subq.w	#1,d3
	bne.s	.multipleLongwordOutput_rotateInput_firstLong0_lastLong0_middleLongs
.multipleLongwordOutput_rotateInput_firstLong0_lastLong0_nMiddleLongs
	
	move.l	(sp)+,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput

	rts

.multipleLongwordOutput_rotateInput_firstLong01_lastLong0

	move.l	d3,-(sp)
	addq.l	#4,a0
	move.l	d6,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput
	addq.l	#4,a0
	move.l	(sp)+,d3
	beq.s	.multipleLongwordOutput_rotateInput_firstLong01_lastLong0_nMiddleLongs

.multipleLongwordOutput_rotateInput_firstLong01_lastLong0_middleLongs

	move.l	d3,-(sp)
	moveq	#-1,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput
	addq.l	#4,a0

	move.l	(sp)+,d3
	subq.w	#1,d3
	bne.s	.multipleLongwordOutput_rotateInput_firstLong01_lastLong0_middleLongs
.multipleLongwordOutput_rotateInput_firstLong01_lastLong0_nMiddleLongs
	
	move.l	(sp)+,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput

	rts
	
.multipleLongwordOutput_rotateInput_firstLong01_lastLong01

	move.l	d3,-(sp)
	addq.l	#4,a0
	move.l	d6,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput
	addq.l	#4,a0
	move.l	(sp)+,d3
	beq.s	.multipleLongwordOutput_rotateInput_firstLong01_lastLong01_nMiddleLongs

.multipleLongwordOutput_rotateInput_firstLong01_lastLong01_middleLongs

	move.l	d3,-(sp)
	moveq	#-1,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput
	addq.l	#4,a0

	move.l	(sp)+,d3
	subq.w	#1,d3
	bne.s	.multipleLongwordOutput_rotateInput_firstLong01_lastLong01_middleLongs
.multipleLongwordOutput_rotateInput_firstLong01_lastLong01_nMiddleLongs
	
	move.l	(sp)+,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bsr	.renderColumn_rotateInput_constantMaskOutput

	rts

;----------------------------------------------------------------------------
	
.oneLongwordOutput
	and.l	d7,d6
	move.w	d4,d7
	beq.s	.oneLongwordOutput_nonRotateInput

	jmp	.oneLongwordOutput_rotateInput_jumpTable(pc,d5.w*4)
.oneLongwordOutput_rotateInput_jumpTable

	bra.w	.oneLongwordOutput_rotateInput_longword0Input
	bra.w	.oneLongwordOutput_rotateInput_twoLongwordsInput
	illegal
	illegal
	bra.w	.oneLongwordOutput_rotateInput_longword1Input

.oneLongwordOutput_rotateInput_twoLongwordsInput
	addq.l	#4,a0
	move.l	d6,d5
	moveq	#-1,d6
	lsr.l	d7,d6
	bra.s	.renderColumn_rotateInput_constantMaskOutput

.oneLongwordOutput_rotateInput_longword0Input
	addq.l	#4,a0
	move.l	d6,d5
	moveq	#0,d6
	bra.s	.renderColumn_rotateInput_constantMaskOutput
	
.oneLongwordOutput_nonRotateInput
.oneLongwordOutput_rotateInput_longword1Input
	move.l	d6,d5
	moveq	#-1,d6
	bra.s	.renderColumn_rotateInput_constantMaskOutput

	nop
	
;----------------------------------------------------------------------------

; d5	targetmask
; d6	ror mask
; d7	ror factor
; a0	source
; a1	target
; a2	source plane-modulo
; a3	target plane-modulo

.renderColumn_rotateInput_constantMaskOutput
	move.l	d6,d4
	not.l	d4
	and.l	d5,d4
	and.l	d5,d6
	not.l	d5

	move.l	a1,-(sp)
	move.l	a0,-(sp)

	move.w	.numRows,d3
	swap	d3
	move.w	.numPlanes,d3

	move.l	(a1),d2

	move.l	-4(a0),d0
	move.l	(a0),d1

	bra.s	.maskPlaneStart

.renderColumn_rotateInput_constantMaskOutput_row
	swap	d3
	move.w	.numPlanes,d3

.renderColumn_rotateInput_constantMaskOutput_plane
	move.l	(a1),d2

	move.l	-4(a0),d0
	move.l	(a0),d1
	move.l	a4,(a5)

.maskPlaneStart
	add.l	a2,a0
	ror.l	d7,d0
	move.l	a1,a5
	ror.l	d7,d1
	add.l	a3,a1
	and.l	d4,d0
	and.l	d6,d1
	move.l	d0,a4
	and.l	d5,d2
	add.l	d1,a4
	add.l	d2,a4

	subq.w	#1,d3
	bne.s	.renderColumn_rotateInput_constantMaskOutput_plane

	add.l	.sourceRowDelta,a0
	swap	d3
	add.l	.targetRowDelta,a1
	subq.w	#1,d3
	bne.s	.renderColumn_rotateInput_constantMaskOutput_row

	move.l	a4,(a5)

	move.l	(sp)+,a0
	move.l	(sp)+,a1
	addq.l	#4,a1
	rts

.skip
	rts


	SECTION	data,data
	
	
.numRows	ds.w	1
.numPlanes 	ds.w	1

.sourceRowDelta	ds.l	1
.targetRowDelta	ds.l	1

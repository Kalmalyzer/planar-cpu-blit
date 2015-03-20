
				RSRESET
ContiguousBitMap_Planes		RS.L	1
ContiguousBitMap_PlaneModulo	RS.L	1
ContiguousBitMap_RowModulo	RS.L	1
ContiguousBitMap_NumPlanes	RS.W	1
				RS.W	1
				

	SECTION	code,code

;----------------------------------------------------------------------------
; CPU-based bitmap blit routine, with mask plane
;
; This routine copies a rectangular region from source to target bitmap.
; Both bitmaps should be in planar format and with the same number of planes.
; The mask bitmap should have 1 plane.
; The source and mask bitmaps should be of the  same dimensions.
; 32-bit align all bitmaps.
; Ensure that bitmaps are a multiple of 32 pixels wide.
; It will perform only aligned 32bit reads/writes against memory.
; Memory accesses are organized to suit copying fastmem->chipmem.
;
; Current version sucks a bit for architectures which reads entire
;  cachelines from fastmem (030+DBURST, and 040 & 060).
;
; Profiling results on 68060@50:
; Copy fast->chip, with 8bpl output...
; Mask is all empty:		processing 2.1MB/frame
; Mask contains mixed data:	outputting 43kB/frame
; Mask is all set:		outputting 64kB/frame
;
; Inputs:
; 	a0	source ContiguousBitMap
; 	a1	mask ContiguousBitMap
; 	a2	dest ContiguousBitMap
; 	d0.w	source x0
; 	d1.w	source y0
; 	d2.w	source w (must be >0)
; 	d3.w	source h (must be >0)
; 	d4.w	target x0
; 	d5.w	target y0
;
; Outputs:
;  All registers trashed.

PlanarCpuBlitMasked
		tst.w	d2
		bmi	.skip
		move.w	ContiguousBitMap_NumPlanes(a0),d6
		move.w	d3,.numRows
		bmi	.skip
		move.w	d6,.numPlanes

		move.l	ContiguousBitMap_RowModulo(a0),d6
		and.l	#$ffff,d1
		move.l	ContiguousBitMap_RowModulo(a2),d7
		and.l	#$ffff,d5
		mulu.l	d6,d1
		mulu.l	d7,d5
		move.l	d6,.sourceRowModulo
		moveq	#0,d6
		move.l	d7,.targetRowModulo
		moveq	#0,d7
		move.l	ContiguousBitMap_RowModulo(a1),a6
		move.w	d0,d6
		move.l	a6,.maskRowModulo
		move.w	d4,d7
		and.w	#$1f,d0
		and.w	#$1f,d4
		eor.w	d0,d6
		eor.w	d4,d7
		lsr.w	#3,d6
		lsr.w	#3,d7
		add.l	d6,d1
		add.l	d7,d5

		move.l	ContiguousBitMap_PlaneModulo(a0),a5
		moveq	#0,d3
		move.l	ContiguousBitMap_PlaneModulo(a2),a6
		move.l	a5,d6
		move.w	ContiguousBitMap_NumPlanes(a0),d3
		move.l	a6,d7
		mulu.l	d3,d6
		mulu.l	d3,d7
		neg.l	d6
		move.l	ContiguousBitMap_RowModulo(a0),a3
		neg.l	d7
		add.l	a3,d6
		add.l	ContiguousBitMap_RowModulo(a2),d7
		move.l	d6,.sourceRowDelta
		move.l	d7,.targetRowDelta
		move.l	a3,.maskRowDelta

		move.l	ContiguousBitMap_Planes(a0),a0
		move.l	ContiguousBitMap_Planes(a1),a1
		move.l	ContiguousBitMap_Planes(a2),a2
		add.l	d1,a0
		add.l	d1,a1
		add.l	d5,a2

		move.w	d0,d1
		move.w	d4,d5
		subq.w	#1,d2
		add.w	d2,d1
		add.w	d2,d5

;	d0.w	source x0
;	d1.w	source (x1-1)
;	d4.w	target x0
;	d5.w	target (x1-1)

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

;	d0.w	source x0 & $1f
;	d1.w	source (x1-1) & $1f
;	d3.w	number of longwords to output per row in target - 1
;	d4.w	target x0 & $1f
;	d5.w	target (x1-1) & $1f
;	d6.w	$1f
	
		move.w	d5,d2
		moveq	#-1,d7
		eor.w	d6,d2
		moveq	#-1,d6
		lsl.l	d2,d7
		lsr.l	d4,d6

;	d0.w	source x0 & $1f
;	d1.w	source (x1-1) & $1f
;	d3.w	number of longwords to output per row in target - 1
;	d4.w	target x0 & $1f
;	d5.w	target (x1-1) & $1f
;	d6.l	fwm
;	d7.l	lwm
	
		sub.w	d0,d4
		move.w	d4,d5

		and.w	#$1f,d4
		
		lea	.dummyTargetLocation,a4

;	a0	source pixels
;	a1	mask pixels
;	a2	target pixels
;	a4	dummy target location

;	d0.w	source x0 & $1f
;	d3.w	number of longwords to output per row in target - 1
;	d4.w	start bit delta & $1f
;	d5.w	start bit delta
;	d6.l	fwm
;	d7.l	lwm

		subq.w	#1,d3
		bmi.s	.oneLongwordOutput

;----------------------------------------------------------------------------

.multipleLongwordOutput
		rol.l	d4,d6
		rol.l	d4,d7
	
		move.l	d7,-(sp)

		tst.w	d5
		bpl.s	.multipleLongwordOutput_positiveShift
		addq.l	#4,a0
		addq.l	#4,a1
.multipleLongwordOutput_positiveShift

		move.l	d3,-(sp)

		move.l	d6,d0

		moveq	#-1,d7
		lsl.l	d4,d7
		move.l	d7,d6
		not.l	d6

		and.l	d0,d6
		and.l	d0,d7

		bsr.s	.renderColumn
		move.l	(sp)+,d3
		beq.s	.multipleLongwordOutput_nMiddleLongs

.multipleLongwordOutput_middleLongs

		move.l	d3,-(sp)

		moveq	#-1,d7
		lsl.l	d4,d7
		move.l	d7,d6
		not.l	d6

		bsr.s	.renderColumn

		move.l	(sp)+,d3
		subq.w	#1,d3
		bne.s	.multipleLongwordOutput_middleLongs
.multipleLongwordOutput_nMiddleLongs

		move.l	(sp)+,d0

		moveq	#-1,d7
		lsl.l	d4,d7
		move.l	d7,d6
		not.l	d6

		and.l	d0,d6
		and.l	d0,d7

		bsr.s	.renderColumn
		move.l	a3,(a4)
		rts

;----------------------------------------------------------------------------
	
.oneLongwordOutput
		and.l	d7,d6
		rol.l	d4,d6
		move.l	d6,d0

		moveq	#-1,d7
		lsl.l	d4,d7
		move.l	d7,d6
		not.l	d6

		and.l	d0,d6
		and.l	d0,d7

		tst.w	d5
		bpl.s	.oneLongwordOutput_positiveShift
		addq.l	#4,a0
		addq.l	#4,a1
.oneLongwordOutput_positiveShift
	
		bsr.s	.renderColumn
		move.l	a3,(a4)
		rts
	
;----------------------------------------------------------------------------

;	d4	ror factor
;	d6	leftLongMaskPreRotate
;	d7	rightLongMaskPreRotate
;	a0	source pixels
;	a1	mask pixels
;	a2	target pixels
;	a5	source plane-modulo
;	a6	target plane-modulo

					RSRESET
.renderColumn_leftLongMaskPreRotate	RS.L	1
.renderColumn_rightLongMaskPreRotate	RS.L	1
.renderColumn_LocalSize			RS.B	0

.renderColumn
		move.l	a2,-(sp)
		move.l	a1,-(sp)
		move.l	a0,-(sp)
		subq.l	#.renderColumn_LocalSize,sp

		move.l	d6,.renderColumn_leftLongMaskPreRotate(sp)
		move.l	d7,.renderColumn_rightLongMaskPreRotate(sp)

		move.w	.numRows,d3
.renderColumn_row
		swap	d3

		move.l	-4(a1),d6
		move.l	(a1),d7
	
		and.l	.renderColumn_leftLongMaskPreRotate(sp),d6
		and.l	.renderColumn_rightLongMaskPreRotate(sp),d7

		move.l	d6,d5
		move.w	.numPlanes,d3
		or.l	d7,d5
		beq	.renderColumn_planesFullyMaskedAway
		not.l	d5
		beq.s	.renderColumn_planesWithoutMask
		ror.l	d4,d5

.renderColumn_planeWithMask
		move.l	-4(a0),d0
		move.l	(a0),d1
		move.l	(a2),d2
		move.l	a3,(a4)

		and.l	d6,d0		; 2
		and.l	d7,d1		; 2
		or.l	d1,d0		; 2
		ror.l	d4,d0		; 6

		and.l	d5,d2		; 2
		move.l	a2,a4		; 2
		move.l	d0,a3		; 2
		add.l	d2,a3		; 2

		add.l	a5,a0		; 2
		add.l	a6,a2		; 2
		subq.w	#1,d3		; 2
		bne.s	.renderColumn_planeWithMask
					; 26 + branch

		add.l	.sourceRowDelta,a0
		add.l	.maskRowDelta,a1
		add.l	.targetRowDelta,a2

		swap	d3
		subq.w	#1,d3
		bne.s	.renderColumn_row
		
		addq.l	#.renderColumn_LocalSize,sp

		move.l	(sp)+,a0
		move.l	(sp)+,a1
		move.l	(sp)+,a2
		addq.l	#4,a0
		addq.l	#4,a1
		addq.l	#4,a2
		rts

.renderColumn_planesWithoutMask
.renderColumn_planeWithoutMask
		move.l	-4(a0),d0
		move.l	(a0),d1

		move.l	a3,(a4)

		and.l	d6,d0		; 2
		and.l	d7,d1		; 2
		or.l	d1,d0		; 2
		ror.l	d4,d0		; 6

		move.l	a2,a4		; 2
		move.l	d0,a3		; 2

		add.l	a5,a0		; 2
		add.l	a6,a2		; 2
		subq.w	#1,d3		; 2
		bne.s	.renderColumn_planeWithoutMask
					; 22 + branch

		add.l	.sourceRowDelta,a0
		add.l	.maskRowDelta,a1
		add.l	.targetRowDelta,a2

		swap	d3
		subq.w	#1,d3
		bne	.renderColumn_row
		
		addq.l	#.renderColumn_LocalSize,sp

		move.l	(sp)+,a0
		move.l	(sp)+,a1
		move.l	(sp)+,a2
		addq.l	#4,a0
		addq.l	#4,a1
		addq.l	#4,a2
		rts

.renderColumn_planesFullyMaskedAway
		add.l	.sourceRowModulo,a0
		add.l	.maskRowModulo,a1
		add.l	.targetRowModulo,a2

		swap	d3
		subq.w	#1,d3
		bne	.renderColumn_row
		
		addq.l	#.renderColumn_LocalSize,sp

		move.l	(sp)+,a0
		move.l	(sp)+,a1
		move.l	(sp)+,a2
		addq.l	#4,a0
		addq.l	#4,a1
		addq.l	#4,a2
		rts

.skip
		rts


		SECTION	data,data
	
	
.numRows	ds.w	1
.numPlanes 	ds.w	1

.sourceRowModulo	ds.l	1
.maskRowModulo		ds.l	1
.targetRowModulo	ds.l	1

.sourceRowDelta	ds.l	1
.maskRowDelta	ds.l	1
.targetRowDelta	ds.l	1

.dummyTargetLocation
		ds.l	1

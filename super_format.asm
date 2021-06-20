; roudoudou / Resistance 2021

org #9000

; CALL Format,drive
; CALL WriteSector,command,drive,side,piste,premsect,dernsect,taillesect,buffer
; CALL WriteHexa,command,drive,side,piste,premsect,dernsect,realsize,buffer
jp format_entry
jp write_sector
jp write_hexa

format_entry
cp 1 : ret nz
ld a,(ix+0) : ld (drive),a

ld hl,str_motor : call print_string

;**************************************************
; wait for system to shut down floppy drive
;**************************************************
ld b,0 : halt : djnz $-1
         halt : djnz $-1
ld hl,str_motoron : call print_string
;**************************************************
; then start it! + delay
;**************************************************
ld bc,#FA7E : ld a,1 : out (c),a
ld b,0 : halt : djnz $-1
         halt : djnz $-1

ld bc,#FB7E ; FDC I/O port

;**************************************************
; reinit values for multiple run
;**************************************************
xor a : ld (track),a : call calibrate : call calibrate ; twice for 80 tracks
ld hl,TRACK_DEFINITION
ld a,(hl) ; nbtrack
inc a
ld (track_to_write+1),a
inc hl
ld (current_definition+1),hl

push bc : ld hl,str_is_calibrated : call print_string : pop bc


;**************************************************
;**************************************************
;**************************************************
;**************************************************
                  super_format
;**************************************************
;**************************************************
;**************************************************
;**************************************************

track_to_write ld a,#12 : dec a : ld (track_to_write+1),a : jp z,ziend

push bc,de,hl : ld hl,#0109 : call #BB75 : ld hl,str_check : call print_string : ld hl,#0509 : call #BB75 : pop hl,de,bc

;**************************************************
; check ET3 | floppy inserted and not protected
;**************************************************
ld a,4 : call push_fdc ; command
xor a  : call push_fdc ; drive
call GetResult
ld hl,msgerr4 : ld a,(nbresult) : cp 1 : jp nz,exit_ko
ld hl,msgerr5
ld a,(result) ; ET3
and #C0
jp nz,exit_ko

;**************************************************
; read definition for the track
;**************************************************
current_definition ld hl,#1234

ld a,(hl) : inc hl ; track
ld (track),a
ld a,(hl) : inc hl ; side
ld (side),a
ld e,(hl) : inc hl ; sector size
ld d,(hl) : inc hl ; nbsector
ld a,e : or d : jp z,exit_ok

;**************************************************
; seek track (may calibrate if not succeed)
;**************************************************
push bc,de,hl : ld hl,str_seek : call print_string : pop hl,de,bc
push bc : ld bc,#7F10 : out (c),c : ld c,64+19 : out (c),c : pop bc ; VERT VIF
push de,hl : call seek_track : pop hl,de ; seek on (track)
push bc,de,hl : ld hl,str_format : call print_string : pop hl,de,bc

di

push bc : ld bc,#7F10 : out (c),c : ld c,64+14 : out (c),c : pop bc ; ORANGE

;**************************************************
; format command
;**************************************************
ld a,(side) : rrca : and 128 : or #4D : call push_fdc   ; FORMAT command Multi-Tete
ld a,(drive) : call push_fdc          ; drive
ld a,e   : call push_fdc          ; sector size
ld a,d   : call push_fdc          ; nbsector
ld a,(hl): call push_fdc : inc hl ; GAP

push bc : ld bc,#7F10 : out (c),c : ld c,64+12 : out (c),c : pop bc ; ROUGE

ld a,(hl): call push_fdc : inc hl ; filler

;**************************************************
; format sector execution
;**************************************************
.loopsector
ld a,(track) : call push_fdc          ; track
ld a,(side)  : call push_fdc          ; head
ld a,(hl)    : call push_fdc : inc hl ; ID
ld a,(hl)    : call push_fdc : inc hl ; sector size (used to read/write)
dec d
jr nz,.loopsector

push bc : ld bc,#7F10 : out (c),c : ld c,64+22 : out (c),c : pop bc ; VERT

ld (current_definition+1),hl

;**************************************************
; format result
;**************************************************
call GetResult
; check des erreurs
ld hl,msgerr1 : ld a,(nbresult) : cp 7 : jr nz,exit_ko
ld hl,msgerr2 : ld a,(result+0) ; ET0
and 128+64+16+8 ; => ERROR (no disk, calib faile, head unavailable)
jr nz,exit_ko
ld a,(result+0) ; ET0
and 32 ; terminated
jr nz,exit_ko
ld hl,msgerr3
ld a,(result+1) ; ET1
and 16+2 ; => ERROR (overrun or protected) => DO NOT CHECK END OF TRACK because we want to deformat some ;)
jr nz,exit_ko

push bc : ld bc,#7F10 : out (c),c : ld c,64+18 : out (c),c : pop bc ; VERT VIF

;ld a,(track) : inc a : ld (track),a => from track definition
ei
jp super_format

ziend
xor a : ld (track),a : call calibrate : call calibrate ; twice for 80 tracks
ret

;**************************************************
;**************************************************
;**************************************************
exit_ok
ld hl,str_ok
call print_string
jr motoff

exit_ko
;ld hl,str_ko
call print_string

ld a,(nbresult) : ld d,a : call printA : inc d
dec d : jr z,motoff
ld a,(result) : call printA
dec d : jr z,motoff
ld a,(result+1) : call printA
dec d : jr z,motoff
ld a,(result+2) : call printA
dec d : jr z,motoff
ld a,(result+3) : call printA
dec d : jr z,motoff
ld a,(result+4) : call printA
dec d : jr z,motoff
ld a,(result+5) : call printA
dec d : jr z,motoff
ld a,(result+6) : call printA

motoff
ei
ld bc,#FA7E : out (c),0
ret

printA
push af
rrca : rrca : rrca : rrca : and #F : call printF
pop af
and #F
call printF
ld a,' '
jp #BB5A
printF
cp 10 : jr c,.digit
add 'A'-10
jp #BB5A
.digit
add '0'
jp #BB5A

print_string ld a,(hl) : or a : ret z
call #bb5A : inc hl : jr print_string

msgerr1 defb 13,10,'there was error during format: need 7 result bytes',13,10,0
msgerr2 defb 13,10,'there was error during format: ET0 is wrong',13,10,0
msgerr3 defb 13,10,'there was error during format: ET1 is wrong',13,10,0
msgerr4 defb 13,10,'there was error during format: GetET3 need 1 byte result',13,10,0
msgerr5 defb 13,10,'there was error during format: ET3 is wrong',13,10,0

str_ok defb 13,10,'everything went OK',13,10,0
str_motor defb 'waiting before motor ON',13,10,0
str_calibrating defb 'Calibration in progress',13,10,0
str_is_calibrated defb 'Calibration OK',13,10,0
str_motoron defb 'motor ON',13,10,0
str_check defb 'CHK/       ',0
str_seek defb 'SK/',0
str_format defb 'FRMT',0


;************************************
;   FDC routines
;************************************
get_int_state
ld a,8 : call push_fdc
call GetResult
ret

calibrate
push bc : ld hl,str_calibrating : call print_string : pop bc

ld a,7 : call push_fdc
ld a,(drive) : call push_fdc
.waitseek
call get_int_state
ld a,(nbresult) : cp 2 : jr nz,.waitseek ; a successful calibration returns 2 results
ld a,(result+0) : cp 32 : jr nz,.waitseek; ET0 must tell us it's over
; then seek_track again!

seek_track
ld a,15 : call push_fdc
ld a,(drive)  : call push_fdc
ld a,(track) : call push_fdc
.waitseek
call get_int_state
ld a,(nbresult) : cp 2 : jr nz,.waitseek ; same as calibration
ld a,(result+0) : cp 32 : jr nz,.waitseek
ld a,(track) : ld e,a
ld a,(result+1) : cp e : jr nz,calibrate ; if we get a wrong track ID then calibrate!
ret

; A=value to push
push_fdc
push af
.ready
in a,(c)
jp p,.ready
and 64 : jr nz,GURUDISPLAY ; if FDC does not want our value then we are in a wrong state
pop af
inc c
out (c),a
dec c
ret

GURUDISPLAY ld bc,#7F10 : out (c),c : ld a,64+5 : out (c),a : jr $

; compact version for GetResult
GetResult
push de,hl
ld d,7 ; Max results to get
ld hl,result
.wait_ready in a,(c) : jp p,.wait_ready
and 64 : jr z,.done                ; is it a result?
inc c : in a,(c) : dec c
ld (hl),a : inc hl ; store it!
dec d
jr nz,.wait_ready
.done
ld a,7
sub d
ld (nbresult),a ; also store nbresult
pop hl,de
ret

result     defs 7
nbresult   defb 0
track      defb 0
drive      defb 0
side       defb 0
reallength defw 0

defb 'roudoudou'


;********** Amstrad 100% n°44 - routine adaptée pour gérer drive+face ******
; CALL WriteSector,command,drive,side,piste,premsect,dernsect,taillesect,buffer
write_sector: cp 8 : ret nz : di
ld a,#0f : call l809c: ld a,(ix+12): call l809c: ld a,(ix+#08): call l809c: l8024: in a,(c): jp p,l8024
ld hl,tampon: l802c: ld a,#08: call l809c: call l807d:
ld hl,tampon: bit 5,(hl): jr z,l802c:ld h,(ix+#01): ld l,(ix+#00):
ld a,(ix+10): rrca : and 128 : or (ix+14): call l809c: ld a,(ix+12): call l809c: ld a,(ix+#08): call l809c: ld a,#00: call l809c
ld a,(ix+#06): call l809c: ld a,(ix+#02): call l809c: ld a,(ix+#04): call l809c: ld a,#2a: call l809c: ld a,#ff: call l809c
jr write_data.ready
write_data: inc c: inc b : outi : dec c : .ready: in a,(c): jp p,.ready: and #20: jr nz,write_data
ld hl,tampon: call l807d: ei: ret: tampon defs 10 : defb 'roudoudou'
l807d: in a,(c): cp #c0: jr c,l807d: inc c: in a,(c): ld (hl),a: dec c: inc hl: ld a,#05
l808b: dec a: jr nz,l808b: in a,(c): and #10: jr nz,l807d: ld a,(tampon+1): and #04: ret nz: scf: ret
l809c: ld bc,#fb7e: push af: l80a0: in a,(c): add a: jr nc,l80a0: add a: jr nc,l80aa: pop af: ret
l80aa: pop af: inc c: out (c),a: dec c: ret ;;;;;;; INUTILE ld a,#05: l80b1: nop: dec a: jr nz,l80b1: ret


;********** Amstrad 100% n°44 - routine adaptée pour gérer drive+face et secteur taille 6 ******
; CALL WriteHexagone,command,drive,side,piste,premsect,dernsect,realsize,buffer
write_hexa: cp 8 : ret nz : di
ld a,#0f : call .pushfdc: ld a,(ix+12): call .pushfdc: ld a,(ix+#08): call .pushfdc: .miniready: in a,(c): jp p,.miniready
ld hl,tampon: .getint: ld a,#08: call .pushfdc: call .result:
ld hl,tampon: bit 5,(hl): jr z,.getint:ld h,(ix+#01): ld l,(ix+#00):
ld a,(ix+10): rrca : and 128 : or (ix+14): call .pushfdc: ld a,(ix+12): call .pushfdc: ld a,(ix+#08): call .pushfdc: ld a,#00: call .pushfdc
ld a,(ix+6): call .pushfdc: ld a,6: call .pushfdc: ld a,(ix+4): call .pushfdc: ld a,#2a: call .pushfdc
; before command start
ld de,(ix+2) ; realsize
dec de
inc d
inc e

ld a,#ff: call .pushfdc
jr .ready

.write_data: inc c: inc b : outi : dec c

dec e : jr nz,.ready
dec d : jr z,.stop

.ready: in a,(c): jp p,.ready: and #20: jr nz,.write_data

.stop ld bc,#FA7E : out (c),0
ei : xor a : ld b,a : halt : djnz $-1
ld bc,#FA7E : inc a : out (c),a ; and start!
halt : djnz $-1
halt : djnz $-1
ld bc,#FB7E

ld hl,tampon: call .result

;********** check ID *************
ld a,#4A : call .pushfdc     ; GETID
ld a,(ix+12) : call .pushfdc ; DRIVE
.waitid in a,(c) : jp p,.waitid

call GetResult
; nbresult=7
; result ==  192+32 == 32   ;ET0
; result+1 & 4 == 0 ?
; result+5 == ID
; result+6 == 6
; CPCEmuPower => 0 success 48 failed

ld a,(nbresult) : cp 7 : jr nz,hexa_failed
ld a,(result+0) : and 8 : jr nz,hexa_failed     ; wrong format
ld a,(result+1) : and 1 : jr nz,hexa_failed     ; ID not found
ld a,(result+5) : cp (ix+4) : jr nz,hexa_failed ; ID requested
ld a,(result+6) : cp 6 : jr nz,hexa_failed      ; sector size

ret


.result: in a,(c): cp 192: jr c,.result: inc c: in a,(c): ld (hl),a: dec c: inc hl
ld a,5 : .smallpause : dec a: jr nz,.smallpause ; cradooooooooo
in a,(c): and 16: jr nz,.result: ld a,(tampon+1): and 4: ret nz: scf: ret

.pushfdc: ld bc,#fb7e: push af
.pushready : in a,(c): add a: jr nc,.pushready: add a: jr nc,.sendio: pop af: ret
.sendio: pop af: inc c: out (c),a: dec c: ret

hexa_failed
ld hl,str_hexf : call print_string
ei
ret


str_hexf defb 7,' Failed',7,' to write',7,' sector',7,13,10,0



TRACK_DEFINITION
include 'export_definition.asm'



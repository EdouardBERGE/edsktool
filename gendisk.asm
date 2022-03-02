DISK_IN_OPEN   equ #BC77
DISK_IN_CLOSE  equ #BC7A
DISK_IN_DIRECT equ #BC83

disk_buffer = #4000-2048
implante=#8000
org implante
; get param
cp 1 : ret nz : ld a,(ix+0) : ld (drive),a : xor a : ld (motor_state),a

ld (ForceExit+1),sp
ld a,10 : ld (maxfatal),a

ld a,2 : call #BC0E ; mode 2

ld hl,str_motor : call PrintStr
call MotorON
ld hl,str_amsdos: call PrintStr
call AmsdosTiming
ld hl,str_reset : call PrintStr
call ResetPack

ld hl,str_check : call PrintSTR
call CheckET3

ld hl,str_calibrate : call PrintSTR
ld a,(drive)
;call CalibrateDrive
;call CheckET3
call GetSpeed

;*********************************
            MainLoop
;*********************************
call LoadPack : jp nc,ExitOK
call MotorON

ld hl,#4000 : ld (track_definition+1),hl

;*** extract informations for pack ***
track_definition ld hl,#1234

ld a,(hl) : inc hl : ld (track),a
ld a,(hl) : inc hl : ld (side),a
ld e,(hl) : inc hl : ld a,e : ld (sectorsize),a
ld d,(hl) : inc hl : ld a,d : ld (nbsector),a
or e : jp z,MainLoop ; end of pack, read next pack!
ld a,(hl) : inc hl : ld (gap),a
ld a,(hl) : inc hl : ld (filler),a
push de
ld (track_definition+1),hl

call CheckET3
ld a,(drive) : ld d,a : ld a,(track) : ld e,a : call SeekTrack

ld hl,str_format : call PrintSTR : ld a,(track) : call dispA : ld hl,str_side : call PrintSTR : ld a,(side) : call dispA : call CRLF

call CheckET3 : call Format
call CheckET3 : call GetSpeed
ld a,(sectorsize) : cp 6 : jp z,track_definition ; unformated track, rien à écrire ensuite


WriteSectorList
call CheckET3
ld hl,(track_definition+1)
ld a,(hl) : inc hl : ld (track),a
ld a,(hl) : inc hl : ld (side),a

ld (track_definition+1),hl
ld a,(side) : cp #FF : jp z,track_definition

ld a,(hl) : inc hl : ld (idsector),a
ld a,(hl) : inc hl : ld (sectorsize),a
ld a,(hl) : inc hl : ld (writecommand),a

ld e,(hl) : inc hl : ld d,(hl) : inc hl ; position delay
ld (sectordelay),de
ld e,(hl) : inc hl : ld d,(hl) : inc hl ; sector size
ld (sectordatasize),de

	push hl,de
	ld hl,str_writesector : call PrintSTR
	ld a,(writecommand) : cp #45 : ld hl,str_std : call z,PrintSTR
	ld a,(writecommand) : cp #49 : ld hl,str_dam : call z,PrintSTR
	ld a,':' : call #BB5A

	ld a,(idsector) : call dispHexa : ld a,':' : call #BB5A
	ld a,(sectorsize) : call dispA : ld a,':' : call #BB5A
	pop hl : call dispHL
	pop hl
ld a,(sectorsize) : cp 6 : jr z,.hexa
call WriteSector
jp WriteSectorList
.hexa
call WriteHexaSector
jp WriteSectorList


; format data
drive      defb 0
track      defb 0
side       defb 0
sectorsize defb 0
nbsector   defb 0
gap        defb 0
filler     defb 0
garbageId  defb 0

writecommand defb 0
idsector     defb 0
sectordatasize defw 0
sectordelay defw 0

;**************
    ExitOK
;**************
ld hl,str_exitok : .display call PrintSTR
   ExitError equ ExitOK.display

ld a,(nbresult)
or a : jr z,ForceExit
ld hl,result
.disp_result
push af
ld a,(hl) : call DispHexa :inc hl : ld a,' ' : call #BB5A
pop af
dec a
jr nz,.disp_result

;**************
   ForceExit
;**************
ld sp,#1234
call MotorOFF
ret







;*************************************************************************
;*************************************************************************
;                             routines FDC
;*************************************************************************
;*************************************************************************

WriteHexaSector
di
ld (.hlretry+1),hl ; backup in case of retry
.hlretry ld hl,#1234
ld bc,#FB7E
ld a,(writecommand) : call sendFDCcommand
ld a,(drive)        : call sendFDCparam
ld a,(track)        : call sendFDCparam
ld a,(side)         : call sendFDCparam
ld a,(idsector)     : call sendFDCparam
ld a,(sectorsize)   : call sendFDCparam
ld a,(idsector)     : call sendFDCparam
ld a,(gap)          : call sendFDCparam

; before command start
ld de,.ready : push de
ld de,(sectordatasize) ; realsize
dec de
inc d
inc e

ld a,#FF      : jp sendFDCfast ; retour en .ready

.write_data: inc c: inc b : outi : dec c
dec e : jr nz,.ready
dec d : jr z,.stop
.ready: in a,(c): jp p,.ready: and #20: jr nz,.write_data

.stop ld b,#FA : out (c),0 ; #FA7E
ld b,#FB

.ready2 in a,(c) : jp p,.ready2 : and #20 : jr z,.termine
inc c : out (c),0 : dec c : jr .ready2
.termine
ld (track_definition+1),hl
ei
ld bc,#FA7E : ld a,1 : out (c),a
ld hl,str_hexa_check : call PrintSTR
halt : djnz $-1

ld bc,#FB7E
call GetResult

;********** check ID *************
ld a,#4A : call sendFDCcommand     ; GETID
ld a,(drive) : call sendFDCparam ; DRIVE
.waitid in a,(c) : jp p,.waitid
call GetResult
; nbresult=7
; result ==  192+32 == 32   ;ET0
; result+1 & 4 == 0 ?
; result+5 == ID
; result+6 == 6
; CPCEmuPower => 0 success 48 failed

ld a,(nbresult) : cp 7 : jr nz,.hexa_failed
ld a,(result+0) : and 8 : jr nz,.hexa_failed     ; wrong format
ld a,(result+1) : and 1 : jr nz,.hexa_failed     ; ID not found
ld a,(result+5) : ld hl,idsector : cp (hl) : jr nz,.hexa_failed ; ID requested
ld a,(result+6) : cp 6 : jr nz,.hexa_failed      ; sector size
ld hl,str_verified : jp PrintSTR
.hexa_failed
ld hl,str_hexa_failed : jp PrintSTR



WriteSector
di
ld (.hlretry+1),hl ; backup in case of retry
.hlretry ld hl,#1234
ld bc,#FB7E

ld a,10 : call sendFDCcommand ; getID single density mode
ld a,(drive)        : call sendFDCparam
call GetResult
ld de,(sectordelay) : inc de
.positioncontrol dec de : ld a,6 : dec a : jr nz,$-1 : ld a,e : or d : jr nz,.positioncontrol ; 9 + 3 + 5 x 4

ld a,(writecommand) : call sendFDCcommand
ld a,(drive)        : call sendFDCparam
ld a,(track)        : call sendFDCparam
ld a,(side)         : call sendFDCparam
ld a,(idsector)     : call sendFDCparam
ld a,(sectorsize)   : call sendFDCparam
ld a,(idsector)     : call sendFDCparam
ld a,(gap)          : call sendFDCparam
ld e,#20
ld d,(hl)
ld a,(sectordatasize)      : call sendFDCfast  ; useless
.ready : in a,(c) : jp p,.ready : and e : jr z,.write_end
inc c : out (c),d : inc hl : ld d,(hl) : dec c : jr .ready

.write_end
ld (track_definition+1),hl
call GetResult
ei

ld a,(nbresult) : cp 7 : jp nz,.retry
ld a,(result+0) : and 64+128 : cp 64 : jp nz,.retry ; status special pour RW
ld a,(result+0) : and 8+16 : jp nz,.retry
ld a,(result+1) : and 1+2+4+16 : jp nz,.retry ; wrong format / cannot find ID / timeout
ld a,(result+2) : and 1 : jp nz,.retry ; wrong format

ld hl,(.hlretry+1) : ld de,(sectordatasize) : add hl,de : ld de,(track_definition+1)
ld a,h : cp d : jp nz,.retry
ld a,l : cp e : jp nz,.retry
;ret

ld a,(sectorsize) : or a : jp nz,CheckSector ; no check on zero size sector...
ld hl,str_notverified : jp PrintSTR

.retry
ld hl,str_err_write : call PrintSTR
ld a,(result) : call dispA : ld a,' ' : call #BB5A
ld a,(result+1) : call dispA : ld a,' ' : call #BB5A
ld a,(result+2) : call dispA : ld a,' ' : call #BB5A : ld a,'p' : call #BB5A
ld a,(result+3) : call dispA : ld a,' ' : call #BB5A
ld a,(result+5) : call dispHexa : ld a,' ' : call #BB5A
ld a,(result+6) : call dispA : call CRLF
ld a,(maxfatal) : dec a : ld (maxfatal),a : jp z,ForceExit
jp .hlretry


CheckSector
di
ld hl,#3000
ld de,#3001
ld bc,#0999
ld (hl),0
ldir
;
ld bc,#FB7E

ld a,10 : call sendFDCcommand ; getID single density mode
ld a,(drive)        : call sendFDCparam
call GetResult
ld de,(sectordelay) : inc de
.positioncontrol dec de : ld a,6 : dec a : jr nz,$-1 : ld a,e : or d : jr nz,.positioncontrol ; 9 + 3 + 5 x 4

; read command according to write command
ld a,(writecommand) 
cp #45 : jr nz,.dam : ld a,#46
.dam cp #49 : jr nz,.std : ld a,#4C
.std call sendFDCcommand
ld a,(drive)        : call sendFDCparam
ld a,(track)        : call sendFDCparam
ld a,(side)         : call sendFDCparam
ld a,(idsector)     : call sendFDCparam
ld a,(sectorsize)   : call sendFDCparam
ld a,(idsector)     : call sendFDCparam
ld a,(gap)          : call sendFDCparam
ld e,#20
ld hl,#3000
ld a,(sectordatasize)   : call sendFDCfast  ; useless
.ready : in a,(c) : jp p,.ready : and e : jr z,.write_end
inc c : in a,(c) : ld (hl),a : inc hl : dec c : jr .ready

.write_end
call GetResult
ei

ld a,(nbresult) : cp 7 : jp nz,.retry
ld a,(result+0) : and 64+128 : cp 64 : jp nz,.retry ; status special pour RW
ld a,(result+0) : and 8+16 : jp nz,.retry
ld a,(result+1) : and 1+2+4+16 : jp nz,.retry ; wrong format / cannot find ID / timeout
ld a,(result+2) : and 1 : jp nz,.retry ; wrong format

ld hl,(WriteSector.hlretry+1) : ld de,#3000 : ld bc,(sectordatasize)
.compare
ld a,(de) : cp (hl) : jr nz,.retry
inc de : inc hl : dec bc : ld a,b : or c : jr nz,.compare

ld hl,str_verified : call PrintSTR
ret

.retry
ld hl,str_err_check : call PrintSTR
ld a,(result) : call dispA : ld a,' ' : call #BB5A
ld a,(result+1) : call dispA : ld a,' ' : call #BB5A
ld a,(result+2) : call dispA : ld a,' ' : call #BB5A : ld a,'p' : call #BB5A
ld a,(result+3) : call dispA : ld a,' ' : call #BB5A
ld a,(result+5) : call dispHexa : ld a,' ' : call #BB5A
ld a,(result+6) : call dispA : call CRLF
ld a,(maxfatal) : dec a : ld (maxfatal),a : jp z,ForceExit
jp CheckSector




Format
di
ld bc,#FB7E
ld a,(side) : rrca : and 128 : or #4D : call sendFDCcommand ; Format command multi-tête
ld a,(drive)      : call sendFDCparam
ld a,(sectorsize) : call sendFDCparam
ld a,(nbsector)   : call sendFDCparam
ld a,(gap)        : call sendFDCparam

ld hl,(track_definition+1)
ld a,(nbsector) : ld d,a
ld a,(filler)     : call sendFDCfast

.loopsector
ld a,(track) : call sendFDCfast          ; track
ld a,(side)  : call sendFDCfast          ; head
ld a,(hl)    : call sendFDCfast : inc hl ; ID
ld a,(hl)    : call sendFDCfast : inc hl ; sector size (used to read/write)
dec d
jr nz,.loopsector
ld a,(hl) : ld (garbageId),a : inc hl
ld (track_definition+1),hl
call GetResult
ei
ld hl,str_err_format1
ld a,(nbresult) : cp 7 : jp nz,ExitError
ld hl,str_err_format2
ld a,(result+0) : and 128+64+16+8 : jp nz,ExitError ; no disk, calib faile, head unavailable
ld hl,str_err_format3
ld a,(result+0) : and 32 : jp nz,ExitError ; terminated
ld hl,str_err_format4
ld a,(result+1) : and 16+2 : jp nz,ExitError ; overrun or protected => DO NOT CHECK END OF TRACK because we want to deformat some ;)
ret


AmsdosTiming
ld a,#03 : call sendFDCcommand ; commande de réglage du lecteur
ld a,#A1 : call sendFDCparam   ; step rate 6ms de l'AMSDOS / head unload à 16ms
ld a,#03 : call sendFDCparam   ; head load time 6ms
ret

UnblockFDC
in a,(c)
call p,.forceIO ; tant qu'il n'a vraiment rien à nous dire, on tape dans le port I/O
.ready
inc hl
ld a,h : or l : jr z,UnblockFDC
in a,(c) : jp p,.ready
bit 6,a     ; 0:FDC prêt à recevoir 1:FDC prêt à envoyer
jr z,.et3   ; FDC dispo à écouter et en attente, on fait un ET3 pour terminer
inc c
in a,(c)    ; dépiler une donnée du port I/O
dec c
jr .ready
.forceIO inc c : out (c),0 : dec c : ret ; demande de version de la puce
.et3
ld a,4 : call sendFDCparam ; command
ld a,(drive)  : call sendFDCparam ; drive
call GetResult
ret


sendFDCcommand
push hl,bc,af
ld bc,#FB7E   ; statut port
ld hl,0       ; init timeout counter
.waitready
inc hl
ld a,h : or l : call z,unblockFDC     ; au bout d'une seconde on débloque
in a,(c) : jp p,.waitready            ; tant que bit 7 nul, on attend
bit 6,a : call nz,unblockFDC          ; si bit 6 alors le FDC veut renvoyer quelque chose
inc c                                 ; sinon tout est ok pour envoyer au FDC
pop af
out (c),a
pop bc,hl
ret

sendFDCparam
push bc
ld bc,#FB7E   ; statut port
.waitready in 0,(c) : jp p,.waitready ; tant que le FDC est occupé, on attend
inc c         ; I/O port
out (c),a
pop bc
ret

sendFDCfast
.waitready in 0,(c) : jp p,.waitready ; tant que le FDC est occupé, on attend
inc c         ; I/O port
out (c),a
dec c
ret

GetResult
push de,hl
ld bc,#FB7E
ld d,7       ; max de résultats à récupérer
ld hl,result ; tableau des résultats
.wait_ready in a,(c) : jp p,.wait_ready
and 64 : jr z,.GetDone      ; est-ce un résultat?
inc c : ini : inc b : dec c ; oui, on le stocke
dec d
jr nz,.wait_ready
.GetDone
ld a,7          ; nombre de résultats récupérés
sub d           ; est égal à 7 moins restant à faire
ld (nbresult),a ; on enregistre ce nombre
pop hl,de
ret

nbresult defb 0
result   defs 7

; A=lecteur a calibrer
CalibrateDrive
push de
ld d,a ; sauvegarde du lecteur dans D
jr calinext
caliretry
;push af,bc,de,hl
;ld a,(result) : call dispHexa : ld a,'.':call #BB5A
;pop hl,de,bc,af
calinext
ld a,7:call sendFDCcommand ; calibration
ld a,d:call sendFDCparam   ; lecteur
calires
ei : halt : halt
ld a,8:call sendFDCcommand ; sense int state, pas d'autre paramètre!
call GetResult
ld a,(nbresult) : cp 2 : jr nz,calires ; 2 résultats ou rien
ld a,(result) : and #F8                ; on garde uniquement les bits d'état de ET0
                                       ; sinon ça ne fonctionnera 
cp 32 : jr nz,caliretry                ; si problème on recommence
pop de
push af,bc,de,hl
ld hl,str_calib_ok : call printSTR
pop hl,de,bc,af
ret

; D=lecteur
; E=piste
SeekTrack
ld a,15:call sendFDCcommand ; déplacement piste
ld a,d :call sendFDCparam   ; lecteur
ld a,e :call sendFDCparam   ; piste
WaitSeek
ld a,8 :call sendFDCcommand
call GetResult
ld a,(nbresult) : cp 2 : jr nz,WaitSeek ; 2 résultats ou rien, comme la calibration
ld a,(result) : and #F8                 ; on ne conserve que les bits d'état de ET0
cp 32 : jr nz,WaitSeek                  ; est-ce que l'instruction est terminée?
ld a,(result+1) : cp e : ret z          ; on est sur la piste on s'en va
ld a,d : call CalibrateDrive            ; sinon on recalibre
jr SeekTrack                            ; et on recommence

;*********************************
            MotorON
;*********************************
ld a,(#BE5F) ; AMSDOS considère son moteur allumé?
or a : jr z,.next
ld a,1 : ld (#BE69),a ; ticker au minimum et on attend
halt
jr MotorON
.next
ld bc,#FA7E : ld a,1 : out (c),a   ; on démarre dans tous les cas
ld a,(motor_state) : or a : ret nz ; déjà allumé
push bc
ld bc,#FA7E : ld a,1 : out (c),a   ; on démarre
ld (motor_state),a
ld bc,0                            ; on attend environ 1s
.wait push bc : pop bc : djnz .wait : dec c : jr nz,.wait
pop bc
ret

MotorOFF
push bc
ld bc,#FA7E : xor a : out (c),a
ld (motor_state),a
pop bc
ret

motor_state defb 0

;**************************************************
                     CheckET3
;**************************************************
ld bc,#FB7E
ld a,4 : call sendFDCcommand ; command
ld a,(drive)  : call sendFDCparam ; drive
call GetResult

ld a,(nbresult) : cp 1 : jr z,.next
ld hl,str_err_et3state : call PrintSTR : jr CheckET3

.next
ld a,(result) ; ET3
and #C0 ; inserted and not protected
ret z

ld hl,str_err_et3_insertprotect : call PrintSTR
ei
ld b,0 : halt : djnz $-1
ld hl,maxfatal : dec (hl)
jr nz,CheckET3
jp ForceExit

maxfatal defb 0


;*********************************
            GetSpeed
;*********************************
push hl
di
ld bc,#FB7E
ld a,10 : call sendFDCcommand    ; GetID
ld a,(drive) : call sendFDCparam ; drive
call GetResult ; 200 nops

ld a,10 : call sendFDCcommand    ; GetID en séquence = 200 nops
ld a,(drive) : call sendFDCparam ; drive

; ENVIRON 400 nops écoulés au MINIMUM soit 12.5 octets quasi négligeable...
ld hl,13
.ready inc hl : nop 23 : in a,(c) : jp p,.ready ; 32
srl hl
push hl : push hl : push hl
call GetResult
; 5 tours/seconde c'est 200.000 nops ou 6250 x 32 nops

ld hl,str_tracklen   : call PrintSTR
pop hl               : call DispHL
ld hl,str_trackbytes : call PrintSTR

pop de : ld hl,SpeedHisto : ld a,(track) : add a : add l : ld l,a : ld a,h : adc 0 : ld h,a : ld (hl),e : inc hl : ld (hl),d ; historise les vitesses de rotation

pop hl
ld hl,#90 : add hl,de
ld a,h : cp #18 : jr z,.speedok
         cp #19 : jr z,.speedok
ld hl,str_err_speed : jp ExitError

.speedok
pop hl
ret

;*********************************
           ResetPack
;*********************************
ld hl,pack_number
ld b,4
.loop ld (hl),'0' : inc hl : djnz .loop
ret


str_rsx_sd   str 'SD'
str_rsx_disc str 'DISC'
;*********************************
            LoadPack
;*********************************

ld hl,str_rsx_sd : call #BCD4 : jr nc,.start
xor a : call #001B
.start
ld hl,str_loadpack : call PrintStr
ld hl,pack_filename
ld b,pack_end-pack_filename
ld de,disk_buffer
call DISK_IN_OPEN
ret nc
ld hl,#4000
call DISK_IN_DIRECT
call DISK_IN_CLOSE

ld hl,str_rsx_disc : call #BCD4 : jr nc,.close
xor a : call #001B
.close
;*** increment counter in filename ***
ld hl,pack_number+3
.reloop ld a,(hl) : inc a : cp '9'+1 : jr nz,.incok
ld (hl),'0' : dec hl : jr .reloop
.incok
ld (hl),a
scf
ret

pack_filename defb 'PACK'
pack_number   defb '0000.DAT'
pack_end


;*********************************
            PrintSTR
;*********************************
ld a,(hl) : or a : ret z
call #BB5A : inc hl : jr PrintSTR

CRLF ld a,13 : call #BB5A : ld a,10 : call #BB5A : ret

DispHL: ld d,'0'
	ld	bc,-10000
	call	Num1
	ld	bc,-1000
	call	Num1
	ld	bc,-100
	call	Num1
	ld	c,-10
	call	Num1
	ld	c,-1
	ld d,1 ; 0 is zero
Num1:	ld	a,'0'-1
Num2:	inc	a
	add	hl,bc
	jr	c,Num2
	sbc	hl,bc
	cp d
	ret z ; skip trailing zero
	call #BB5A
        ld d,1 ; end skip zero
	ret 

DispA : ld h,0 : ld l,a : jp dispHL

DispHexa push af : push af
ld a,'#' : call #BB5A
pop af
rrca : rrca : rrca : rrca : and 15 : cp 10 : jr c,.digit
add 'A'-'0'-10
.digit add '0'
call #BB5A
pop af : and 15 : cp 10 : jr c,.digit2 : add 'A'-'0'-10
.digit2 add '0'
jp #BB5A

str_loadpack  defb '- Loading data pack -',13,10,0
str_amsdos    defb 'Configure FDC with regular Amsdos timing',13,10,0
str_reset     defb 'Reinit engine',13,10,0
str_motor     defb 'Motor ON',13,10,0
str_check     defb 'Drive check',13,10,0
str_calibrate defb 'Drive calibration',13,10,0
str_calib_ok  defb 'Calibration OK',13,10,0
str_exitok    defb 'Floppy writed',13,10,0
str_format    defb 'Format track: ',0
str_side      defb ' side: ',0
str_writesector defb 'Write ',0
str_verified    defb ' Verified',13,10,0
str_notverified    defb ' Wont be verified',13,10,0
str_std defb 'STD ',0
str_dam defb 'DAM ',0
str_hexa_check   defb ' Check Hexa',0
str_hexa_failed  defb ' Failed',13,10,0
str_tracklen     defb 'Output track len: ',0
str_trackbytes   defb ' bytes',13,10,0

str_err_et3state          defb 'Error with ET3 state (retrying)',13,10,0
str_err_et3_insertprotect defb 'Insert an unprotected floppy   ',13,10,0
str_err_write             defb ' !Write Err: ',0
str_err_check             defb ' !Check Err: ',0
str_err_format1           defb 'Fatal error during format (nbrez!=7)',13,10,0
str_err_format2           defb 'Fatal error during format (nodisk,calib,head out of order)     ',13,10,0
str_err_format3           defb 'Fatal error during format (not terminated)',13,10,0
str_err_format4           defb 'Fatal error during format (overrun or protected)',13,10,0
str_err_speed             defb 'Fatal error: tracklength must be between 6000 and 6500 bytes',13,10,0

SpeedHisto defs 42*2

print {hex}$

save"gendisk.bin",implante,$-implante,AMSDOS




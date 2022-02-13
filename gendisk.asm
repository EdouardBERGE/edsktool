DISK_IN_OPEN   equ #BC77
DISK_IN_CLOSE  equ #BC7A
DISK_IN_DIRECT equ #BC83

disk_buffer = #4000-2048

org #8000
; get param
cp 1 : ret nz : ld a,(ix+0) : ld (drive),a

ld (ForceExit+1),sp
ld a,10 : ld (maxfatal),a

call MotorON
call AmsdosTiming
call ResetPack

ld hl,str_check : call PrintSTR
call CheckET3

ld hl,str_calibrate : call PrintSTR
ld a,(drive)
call CalibrateDrive

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

ld hl,str_format : call PrintSTR : ld a,(track) : call dispA : call CRLF

call CheckET3
call Format

ld a,(sectorsize) : cp 6 : jp z,track_definition ; unformated track, rien à écrire ensuite
ld a,(nbsector) : ld (sectorcounter+1),a

WriteSectorList
call CheckET3
ld hl,(track_definition+1)
ld a,(hl) : inc hl : ld (track),a
ld a,(hl) : inc hl : ld (side),a
ld a,(hl) : inc hl : ld (idsector),a
ld a,(hl) : inc hl : ld (sectorsize),a
ld a,(hl) : inc hl : ld (writecommand),a

ld e,(hl) : inc hl : ld d,(hl) : inc hl ; sector size
ld (sectordatasize),de

	push hl,de
	ld hl,str_writesector : call PrintSTR
	ld a,(writecommand) : cp #45 : ld hl,str_std : call z,PrintSTR
	ld a,(writecommand) : cp #49 : ld hl,str_dam : call z,PrintSTR
	ld a,':' : call #BB5A

	ld a,(idsector) : call dispHexa : ld a,':' : call #BB5A
	ld a,(sectorsize) : call dispA : ld a,':' : call #BB5A
	pop hl : call dispHL : call CRLF
	pop hl
call WriteSector

sectorcounter ld a,#12 : dec a : ld (sectorcounter+1),a : jr nz,WriteSectorList

jp track_definition



; format data
drive      defb 0
track      defb 0
side       defb 0
sectorsize defb 0
nbsector   defb 0
gap        defb 0
filler     defb 0

writecommand defb 0
idsector     defb 0
sectordatasize defw 0

;**************
    ExitOK
;**************
ld hl,str_exitok : .display call PrintSTR
   ExitError equ ExitOK.display
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
WriteSector
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
ld e,#20
ld d,(hl)
ld a,#FF            : call sendFDCfast  ; useless
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
ret

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
ld (track_definition+1),hl
call GetResult
ei
ld hl,str_err_format
ld a,(nbresult) : cp 7 : jp nz,ExitError
ld a,(result+0) : and 128+64+16+8 : jp nz,ExitError ; no disk, calib faile, head unavailable
ld a,(result+0) : and 32 : jp nz,ExitError ; terminated
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
ret z       ; FDC dispo à écouter et en attente, on s'en va
inc c
in a,(c)    ; dépiler une donnée du port I/O
dec c
jr .ready

.forceIO inc c : out (c),0 : dec c : ret ; demande de version de la puce

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
caliretry
ld a,7:call sendFDCcommand ; calibration
ld a,d:call sendFDCparam   ; lecteur
calires
ld a,8:call sendFDCcommand ; sense int state, pas d'autre paramètre!
call GetResult
ld a,(nbresult) : cp 2 : jr nz,calires ; 2 résultats ou rien
ld a,(result) : and #F8                ; on garde uniquement les bits d'état de ET0
                                       ; sinon ça ne fonctionnera 
cp 32 : jr nz,caliretry                ; si problème on recommence
pop de
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
ld hl,str_motor : call PrintStr

ld a,(#BE5F) ; AMSDOS considère son moteur allumé?
or a : jr z,.next
ld a,1 : ld (#BE69),a ; ticker au minimum et on attend
jr MotorON
.next
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
           ResetPack
;*********************************
ld hl,pack_number
ld b,4
.loop ld (hl),'0' : inc hl : djnz .loop
ret

;*********************************
            LoadPack
;*********************************
ld hl,str_loadpack : call PrintStr
ld hl,pack_filename
ld b,pack_end-pack_filename
ld de,disk_buffer
call DISK_IN_OPEN
ret nc
ld hl,#4000
call DISK_IN_DIRECT
call DISK_IN_CLOSE
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

str_loadpack  defb 'Loading data pack',13,10,0
str_motor     defb 'Motor ON',13,10,0
str_check     defb 'Drive check',13,10,0
str_calibrate defb 'Drive calibration',13,10,0
str_exitok    defb 'Floppy writed',13,10,0
str_format    defb 'Format track ',0
str_writesector defb 'Write ',0
str_std defb 'std ',0
str_dam defb 'dam ',0

str_err_et3state          defb 'Error with ET3 state (retrying)',13,10,0
str_err_et3_insertprotect defb 'Insert an unprotected floppy   ',13,10,0
str_err_format            defb 'Fatal error during format      ',13,10,0
str_err_write             defb 'WriteError: ',0




save"gendisk.bin",#8000,$-#8000,AMSDOS



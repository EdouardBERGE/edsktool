#include<stdlib.h>
#include<string.h>
#include<stdio.h>

#ifndef _MSC_VER
#include<unistd.h>
#define KNORMAL  "\x1B[0m"
#define KERROR   "\x1B[31m"
#define KAYGREEN "\x1B[32m"
#define KWARNING "\x1B[33m"
#define KBLUE    "\x1B[34m"
#define KVERBOSE "\x1B[36m"
#define KIO      "\x1B[97m"
#else
#define KNORMAL  ""
#define KERROR   "Error:"
#define KAYGREEN ""
#define KWARNING "Warning:"
#define KBLUE    ""
#define KVERBOSE ""
#define KIO      "IO:"
#endif

#define ABORT_ERROR -1

/**************************************************
          e d s k    m a n a g e m e n t
**************************************************/
struct s_edsk_sector {
unsigned char track;
unsigned char side;
unsigned char id;
unsigned char size;
unsigned char st1;
unsigned char st2;
unsigned short int length;
unsigned char *data;
int fakegap;
};

struct s_edsk_track  {
int track,side; // easy display
int unformated;
int sectornumber;
int headersize;
/* information purpose */
int sectorsize;
int gap3;
int filler;
int datarate;
int recordingmode;
struct s_edsk_sector *sector;
};

struct s_edsk {
int tracknumber;
int sidenumber;
struct s_edsk_track *track;
};

enum e_putfile_order {
	ORDER_ID=0,
	ORDER_PHYSICAL=1
};

struct s_edsk *NewEDSK(char *format) {
	struct s_edsk *edsk;
	int i,t,s;

	edsk=malloc(sizeof(struct s_edsk));
	memset(edsk,0,sizeof(struct s_edsk));
	if (!format) {
		// empty EDSK
		return edsk;
	}

	if (strcmp(format,"DATA")==0 || strcmp(format,"VENDOR")==0) {
		edsk->tracknumber=42;
		edsk->sidenumber=1;
		edsk->track=malloc(sizeof(struct s_edsk_track)*edsk->tracknumber*edsk->sidenumber);
		memset(edsk->track,0,sizeof(struct s_edsk_track)*edsk->tracknumber*edsk->sidenumber);
		for (t=0;t<=39;t++) {
			edsk->track[t].track=t;
			edsk->track[t].side=0;
			edsk->track[t].sectornumber=9;
			edsk->track[t].sectorsize=2;
			edsk->track[t].gap3=0x50;
			edsk->track[t].filler=0xE5;
			edsk->track[t].sector=malloc(edsk->track[t].sectornumber*sizeof(struct s_edsk_sector));
			for (s=0;s<9;s++) {
				edsk->track[t].sector[s].track=t;
				edsk->track[t].sector[s].side=0;
				if (strcmp(format,"DATA")==0) edsk->track[t].sector[s].id=0xC1+s; else
				if (strcmp(format,"VENDOR")==0) edsk->track[t].sector[s].id=0x41+s;
				edsk->track[t].sector[s].size=2;
				edsk->track[t].sector[s].st1=0;
				edsk->track[t].sector[s].st2=0;
				edsk->track[t].sector[s].length=512;
				edsk->track[t].sector[s].data=malloc(edsk->track[t].sector[s].length);
				for (i=0;i<edsk->track[t].sector[s].length;i++) edsk->track[t].sector[s].data[i]=edsk->track[t].filler;
			}
		}
		for (t=40;t<=41;t++) {
			edsk->track[t].unformated=1;
		}
	}

	return edsk;
}

void MAPTrack(struct s_edsk_track *track) {
	int rlen,gaplen,i,curlen;
	int s,weak,gap,tracklen=0;

	if (track->unformated) {
		printf("S%dT%02d : Unformated\n",track->side,track->track);
	} else {
		printf("S%dT%02d G%02XF%02XS%02d: ",track->side,track->track,track->gap3,track->filler,track->sectornumber);

		if (track->gap3<32) gaplen=0; else
		if (track->gap3<96) gaplen=1; else
		if (track->gap3<160) gaplen=2; else gaplen=3;

		printf("||"); // track info
			      //
		tracklen=146+track->sectornumber*(track->gap3+62);
			      //
		for (s=0;s<track->sectornumber;s++) {
			switch (gaplen) {
				case 3:printf("|");
				case 2:printf("|");
				case 1:printf("|");
				default:break;
			}
			printf("#"); // header

			switch (track->sector[s].size) {
				case 0:curlen=128;break;
				case 1:curlen=256;break;
				case 2:curlen=512;break;
				case 3:curlen=1024;break;
				case 4:curlen=2048;break;
				case 5:curlen=4096;break;
				case 6:curlen=6144;break;
				default:curlen=0;break;
			}
			if (curlen>track->sector[s].length) {
				curlen=track->sector[s].length;
			}
			tracklen+=curlen;

			rlen=(curlen+31)/64;
			rlen-=2;

			if (rlen<0) rlen=0;
			printf("%02X",track->sector[s].id);
			if (rlen>3) {
				if (curlen==track->sector[s].length) {
					printf(".s%d.",track->sector[s].size);
				} else if (curlen*3<=track->sector[s].length) {
					printf("."KWARNING"W"KNORMAL"%d.",track->sector[s].size);
					weak=1;
				} else {
					printf("."KWARNING"G"KNORMAL"%d.",track->sector[s].size);
					gap=1;
				}
				rlen-=4;
			}
			for (i=0;i<rlen;i++) printf(".");
			//printf("%s#%02X (S%d/L%d/%02X/%02X)",s>0?" | ":"",track->sector[s].id,track->sector[s].size,track->sector[s].length,track->sector[s].st1,track->sector[s].st2);
		}
		if (weak && gap) printf(" gap+weak"); else
		if (weak) printf(" weak"); else
		if (gap) printf(" gap");
		if (tracklen>6250 && tracklen<6500) printf("bigTrack! (%d bytes)",tracklen);
		if (tracklen>=6500) printf("Impossible Track! (%d bytes)",tracklen);
		printf("\n");
		weak=gap=0;
	}
	
}
void MAPEDSK(struct s_edsk *edsk) {
	int s,t;

	for (s=0;s<edsk->sidenumber;s++) {
		for (t=0;t<edsk->tracknumber;t++) {
			MAPTrack(&edsk->track[t*edsk->sidenumber+s]);
		}
	}
}
void ExploreTrack(struct s_edsk_track *track) {
	int s;

	if (track->unformated) {
		printf("S%dT%02d : Unformated [%04X]\n",track->side,track->track,track->headersize);
	} else {
		printf("S%dT%02d G%02XF%02XS%02d: ",track->side,track->track,track->gap3,track->filler,track->sectornumber);
		for (s=0;s<track->sectornumber;s++) {
			printf("%s#%02X/T%d/S%d (S%d/L%d/%02X/%02X)",s>0?" ":"",track->sector[s].id,track->sector[s].track,track->sector[s].side,track->sector[s].size,track->sector[s].length,track->sector[s].st1,track->sector[s].st2);
		}
		printf(" [%04X]\n",track->headersize);
	}
	
}
void ExploreEDSK(struct s_edsk *edsk) {
	int s,t;

	for (s=0;s<edsk->sidenumber;s++) {
		for (t=0;t<edsk->tracknumber;t++) {
			ExploreTrack(&edsk->track[t*edsk->sidenumber+s]);
		}
	}
}

struct s_edsk *EDSK_load(char *edskfilename)
{
	#undef FUNC
	#define FUNC "EDSK_load"

	unsigned char header[256];
	unsigned char *data;
	int tracknumber,sidenumber,tracksize,disksize;
	int i,b,s,t,face,curtrack,sectornumber,sectorsize,sectorid,reallength,gap3,filler,ST1,ST2;
	int currenttrackposition=0,currentsectorposition,tmpcurrentsectorposition;
	int curblock=0,curoffset=0;
	int special,is_data,is_vendor,spelocal,ctrlsize;
	FILE *f;
	struct s_edsk *edsk;

	edsk=NewEDSK(NULL);

	f=fopen(edskfilename,"rb");
	if (!f) {
		printf(KERROR"Cannot read EDSK header of [%s]!\n",edskfilename);
		exit(ABORT_ERROR);
	}

	if (fread((char*)&header,1,0x100,f)!=0x100) {
		printf(KERROR"Cannot read EDSK header of [%s]!\n",edskfilename);
		exit(ABORT_ERROR);
	}
	if (strncmp((char *)header,"EXTENDED",8)==0) {
		printf(KIO"opening EDSK [%s] / creator: %-14.14s\n"KNORMAL,edskfilename,header+34);
		tracknumber=header[34+14];
		sidenumber=header[34+14+1];

		// not in EDSK tracksize=header[34+14+1+1]+header[34+14+1+1+1]*256;
		printf("tracks: %d  side:%d\n",tracknumber,sidenumber);

		if (sidenumber<1 || sidenumber>2) {
			printf(KERROR"[%s] EDSK format is not supported in update mode (ntrack=%d nside=%d)\n",edskfilename,tracknumber,sidenumber);
			exit(ABORT_ERROR);
		}

		edsk->tracknumber=tracknumber;
		edsk->sidenumber=sidenumber;
		edsk->track=malloc(sizeof(struct s_edsk_track)*tracknumber*sidenumber);
		memset(edsk->track,0,sizeof(struct s_edsk_track)*tracknumber*sidenumber);

		for (i=disksize=0;i<tracknumber*sidenumber;i++) {
			disksize+=header[0x34+i]*256;
			edsk->track[i].headersize=header[0x34+i]*256;
		}

		printf("total track size: %dkb\n",disksize/1024);

		data=malloc(disksize);
		memset(data,0,disksize);
		if ((ctrlsize=fread((char *)data,1,disksize,f))!=disksize) {
			printf(KERROR"Cannot read EDSK tracks! expecting %d bytes but read %d bytes\n"KNORMAL,disksize,ctrlsize);
			// This is not a fatal Error anymore to allow further analysis
		}

		for (t=0;t<tracknumber;t++)
		for (face=0;face<sidenumber;face++) {
			int track_sectorsize;

			curtrack=t*sidenumber+face;
			i=currenttrackposition;
			currentsectorposition=i+0x100;

			special=0;
			edsk->track[curtrack].track=t;
			edsk->track[curtrack].side=face;

			if (!header[0x34+curtrack]) {
				edsk->track[curtrack].unformated=1;
			} else {
				currenttrackposition+=header[0x34+curtrack]*256;
				if (currenttrackposition>ctrlsize) {
					printf(KERROR"Track %d side %d is declared as %04X bytes long but there is only %04X remaining\n"KNORMAL,t,face,header[0x34+curtrack]*256,ctrlsize-(currenttrackposition-header[0x34+curtrack]*256));
				}

				if (strncmp((char *)data+i,"Track-Info\r\n",12)) {
					printf(KERROR"Invalid track information block side %d track %d => Header offset=%d\n",face,t,header[0x34+curtrack]*256);
					exit(ABORT_ERROR);
				}
				sectornumber=data[i+21];
				track_sectorsize=data[i+20];
				gap3=data[i+22];
				filler=data[i+23];

				// track info
				edsk->track[curtrack].sectornumber=sectornumber;
				edsk->track[curtrack].sectorsize=track_sectorsize;
				edsk->track[curtrack].gap3=gap3;
				edsk->track[curtrack].filler=filler;
				edsk->track[curtrack].datarate=data[i+18];
				edsk->track[curtrack].recordingmode=data[i+19];
				// sector structs
				edsk->track[curtrack].sector=malloc(sizeof(struct s_edsk_sector)*sectornumber);
				memset(edsk->track[curtrack].sector,0,sizeof(struct s_edsk_sector)*sectornumber);

				if (track_sectorsize!=2 || sectornumber!=9) {
					special=1;
				}

				//printf("G%02X F%02X NBS=%02d : ",gap3,filler,sectornumber);

				is_data=is_vendor=0;
				for (s=0;s<sectornumber;s++) {

					sectorid=data[i+24+8*s+2];
					sectorsize=data[i+24+8*s+3];
					// ST1 & ST2 indicates wrong checksum, ...
					ST1=data[i+24+8*s+4];
					ST2=data[i+24+8*s+5];
					reallength=data[i+24+8*s+6]+data[i+24+8*s+7]*256; /* real length stored */

					edsk->track[curtrack].sector[s].track=data[i+24+8*s+0];
					edsk->track[curtrack].sector[s].side=data[i+24+8*s+1];
					edsk->track[curtrack].sector[s].id=sectorid;
					edsk->track[curtrack].sector[s].size=sectorsize;
					edsk->track[curtrack].sector[s].st1=ST1;
					edsk->track[curtrack].sector[s].st2=ST2;
					edsk->track[curtrack].sector[s].length=reallength;
					edsk->track[curtrack].sector[s].data=malloc(reallength);

					if (currentsectorposition+reallength>ctrlsize) {
						printf(KERROR"Invalid side %d track %d => sector data of ID %02X outside EDSK!\n",face,t,sectorid);
						exit(ABORT_ERROR);
					} else {
						memcpy(edsk->track[curtrack].sector[s].data,&data[currentsectorposition],reallength);
					}
					currentsectorposition+=reallength;
				}
			}
		}
        } else if (strncmp(header,"MV - CPC",8)==0) {
                printf("opening legacy DSK [%s] / creator: %-14.14s\n",edskfilename,header+34);

                tracknumber=header[34+14];
                sidenumber=header[34+14+1];
                edsk->tracknumber=tracknumber;
                edsk->sidenumber=sidenumber;
                tracksize=header[34+14+1+1]+header[34+14+1+1+1]*256;

                printf("tracks: %d sides: %d ",edsk->tracknumber,edsk->sidenumber);
                printf("track size: %dkb disk size: %dkb\n",tracksize/1024,tracksize*edsk->tracknumber*edsk->sidenumber/1024);

                data=malloc(tracksize*edsk->tracknumber*edsk->sidenumber);
                if (fread(data,1,tracksize*edsk->tracknumber*edsk->sidenumber,f)!=tracksize*edsk->tracknumber*edsk->sidenumber) {
                        printf("Cannot read DSK tracks!");
                        return NULL;
                }

                edsk->track=malloc(sizeof(struct s_edsk_track)*tracknumber*sidenumber);
                memset(edsk->track,0,sizeof(struct s_edsk_track)*tracknumber*sidenumber);

                for (t=0;t<edsk->tracknumber;t++) {
                        for (face=0;face<edsk->sidenumber;face++) {
                                int maxsectorsize;
                                curtrack=t*edsk->sidenumber+face;

                                i=(t*edsk->sidenumber+face)*tracksize;
                                if (strncmp(data+i,"Track-Info\r\n",12)) {
                                        printf("Invalid track information block side %d track %d",face,t);
                                        return NULL;
                                }
                                edsk->track[curtrack].sectornumber=sectornumber=data[i+21];
                                edsk->track[curtrack].sectorsize=data[i+20];
                                edsk->track[curtrack].gap3=data[i+22];
                                edsk->track[curtrack].filler=data[i+23];
                                edsk->track[curtrack].datarate=0;
                                edsk->track[curtrack].recordingmode=0;
                                edsk->track[curtrack].track=curtrack; // easy display
                                edsk->track[curtrack].side=face; // easy display
                                reallength=0;
                                for (s=0;s<edsk->track[curtrack].sectornumber;s++) {
                                        switch (data[i+24+8*s+3]) {
                                                default:
                                                case 0:if (reallength<128) reallength=128;break;
                                                case 1:if (reallength<256) reallength=256;break;
                                                case 2:if (reallength<512) reallength=512;break;
                                                case 3:if (reallength<1024) reallength=1024;break;
                                                case 4:if (reallength<2048) reallength=2048;break;
                                                case 5:if (reallength<4096) reallength=4096;break;
                                                case 6:if (reallength<0x1800) reallength=0x1800;break;
                                        }
                                }
                                maxsectorsize=reallength;
                               edsk->track[curtrack].sector=malloc(sizeof(struct s_edsk_sector)*edsk->track[curtrack].sectornumber);
                                memset(edsk->track[curtrack].sector,0,sizeof(struct s_edsk_sector)*sectornumber);
                                for (s=0;s<edsk->track[curtrack].sectornumber;s++) {
                                        edsk->track[curtrack].sector[s].track=data[i+24+8*s];
                                        edsk->track[curtrack].sector[s].side=data[i+24+8*s+1];
                                        edsk->track[curtrack].sector[s].id=data[i+24+8*s+2];
                                        edsk->track[curtrack].sector[s].size=data[i+24+8*s+3];
                                        edsk->track[curtrack].sector[s].st1=data[i+24+8*s+4];
                                        edsk->track[curtrack].sector[s].st2=data[i+24+8*s+5];

                                        switch (edsk->track[curtrack].sector[s].size) {
                                                default:
                                                case 0:reallength=128;break;
                                                case 1:reallength=256;break;
                                                case 2:reallength=512;break;
                                                case 3:reallength=1024;break;
                                                case 4:reallength=2048;break;
                                                case 5:reallength=4096;break;
                                                case 6:reallength=0x1800;break;
                                        }
                                        edsk->track[curtrack].sector[s].length=reallength;
                                        edsk->track[curtrack].sector[s].data=malloc(reallength);

                                        memcpy(edsk->track[curtrack].sector[s].data,&data[i+0x100+s*maxsectorsize],reallength);
                                }
                        }
                }

	} else {
		printf(KERROR"file [%s] is not a valid EDSK floppy image\n",edskfilename);
		exit(ABORT_ERROR);
	}
	fclose(f);
	return edsk;
}


void EDSK_write_file(struct s_edsk *edsk, char *output_filename)
{
	unsigned char header[256]={0};
	unsigned char trackblock[256]={0};
	unsigned char headertag[25];
	int tracksize,curtrack;
	int idblock,blockoffset;
	int i,t,s,face;
	FILE *f;
	
	if (!edsk) return;

	unlink(output_filename);
	f=fopen(output_filename,"wb");
	if (!f) {
		printf(KERROR"Cannot open [%s] for writing EDSK\n"KNORMAL,output_filename);
		exit(ABORT_ERROR);
	}

	
	/* �criture header */
	strcpy((char *)header,"EXTENDED CPC DSK File\r\nDisk-Info\r\n");
	sprintf(headertag,"%-9.9s","edskt");
	strcpy((char *)header+0x22,headertag);
	header[0x30]=edsk->tracknumber;
	header[0x31]=edsk->sidenumber;

	for (t=0;t<edsk->tracknumber;t++)
	for (face=0;face<edsk->sidenumber;face++) {
		curtrack=t*edsk->sidenumber+face;
		if (edsk->track[curtrack].unformated) {
			tracksize=0;
		} else {
			tracksize=256;
			for (s=0;s<edsk->track[curtrack].sectornumber;s++) {
				tracksize+=edsk->track[curtrack].sector[s].length;
			}
			if (tracksize&0xFF) tracksize+=256; // adjust high byte value
		}
		header[0x34+curtrack]=tracksize>>8;
	}

	fwrite((char *)header,1,256,f);
	
	/* �criture des pistes */
	for (t=0;t<edsk->tracknumber;t++)
	for (face=0;face<edsk->sidenumber;face++) {
		curtrack=t*edsk->sidenumber+face;

		if (edsk->track[curtrack].unformated) continue; // no physical information for unformated track

		strcpy((char *)trackblock,"Track-Info\r\n");
		trackblock[0x10]=t;
		trackblock[0x11]=face;
		trackblock[0x12]=edsk->track[curtrack].datarate;
		trackblock[0x13]=edsk->track[curtrack].recordingmode;
		trackblock[0x14]=edsk->track[curtrack].sectorsize;
		trackblock[0x15]=edsk->track[curtrack].sectornumber;
		trackblock[0x16]=edsk->track[curtrack].gap3;
		trackblock[0x17]=edsk->track[curtrack].filler;

		for (s=0;s<edsk->track[curtrack].sectornumber;s++) {
			trackblock[0x18+s*8+0]=edsk->track[curtrack].sector[s].track;
			trackblock[0x18+s*8+1]=edsk->track[curtrack].sector[s].side;
			trackblock[0x18+s*8+2]=edsk->track[curtrack].sector[s].id;
			trackblock[0x18+s*8+3]=edsk->track[curtrack].sector[s].size;
			trackblock[0x18+s*8+4]=edsk->track[curtrack].sector[s].st1;
			trackblock[0x18+s*8+5]=edsk->track[curtrack].sector[s].st2;
			trackblock[0x18+s*8+6]=edsk->track[curtrack].sector[s].length&0xFF;
			trackblock[0x18+s*8+7]=(edsk->track[curtrack].sector[s].length>>8)&0xFF;
		}
		fwrite((char *)trackblock,1,256,f);

		tracksize=0;
		for (s=0;s<edsk->track[curtrack].sectornumber;s++) {
			fwrite((char *)edsk->track[curtrack].sector[s].data,1,edsk->track[curtrack].sector[s].length,f);
			tracksize+=edsk->track[curtrack].sector[s].length;
		}
		// filler
		if (tracksize&0xFF) {
			char filler[256]={0};
			tracksize=((tracksize+256)&0xFF00)-tracksize;
			fwrite(filler,1,tracksize,f);
		}
	}
	printf(KIO"Write edsk file %s\n",output_filename);
	fclose(f);
}


void Usage() {
	printf("EDSK options:\n");
	printf("-create DATA|VENDOR      create new edsk in data or vendor format\n");
	printf("-map                     kind of graphical map of EDSK\n");
	printf("-explore                 explore EDSK (full informations)\n");
	printf("-clean                   clean sector extra-bytes\n");
	printf("-merge                   merge two EDSK\n");
	printf("-from1                   side to use from 1st floppy when merging\n");
	printf("-from2                   side to use from 2nd floppy when merging\n");
	printf("-export                  export DSK info for edskwrite tool\n");
	printf("-usegap                  try to use GAP informations for export\n");
	printf("-freegap                 remove GAP informations\n");
	printf("-setextragap <size>      set maximum bytes of fake-GAP\n");
	printf("-forcegap                do not fix GAP when track is too long\n");
	printf("-forcefiller             do not optimize Filler value\n");
	printf("-o <filename>            set output filename for EDSK\n");
	printf("DATA options:\n");
	printf("-dump    <[side:]track:sector>                      dump sector\n");
	printf("-get     <[side:]track:sector> <file>               get sector data\n");
	printf("-put     <[side:]track:sector> <file> [<offset>]    put sector data\n");
	printf("-putfile <[side:]track:sector> <file> <PHYSICAL|ID> put file following physical/logical order\n");
	printf("-rename  <[side:]track:sector> <newid>              rename\n");
	printf("FORMAT MODIFICATION options:\n");
	printf("-droptrack <side:track[+next]>                      drop track(s)\n");
	printf("-drop <side:track:sector[+next]>                    drop sector at specified position\n");
	printf("-add  <side:track:sector[+next]> <id> <size>        add sector at position\n");
	printf("-trackgap    <side:track[+next]> <gap3>             set GAP for track\n");
	printf("-trackfiller <side:track[+next]> <filler>           set filler byte for track format\n");
	printf("-sectormax   <side:track[+next]>  <size>            set maximum bytes for all sectors\n");
	printf("-sectorcut   <side:track:sector[+next]>  <size>     set maximum bytes for one sector\n");
	printf("-hexasize    <size>                                 set real size for hexagone sector operations\n");
	printf("-tracklen    <size>                                 set real size for track operations & controls\n");
	printf("\n");
	printf("for sectorID value, you can use $ or 0x for an hexadecimal value\n");
	exit(ABORT_ERROR);
}
void ExtendedHelp() {
}

void GetValue(char *param, int *zeval) {
	char *ptr;

	if (!zeval || !param) {
		printf(KERROR"GetValue INTERNAL ERROR => Fix this!\n"KNORMAL);
		exit(ABORT_ERROR);
	}

	switch (param[0]) {
		case '$':
			*zeval=strtol(param+1,&ptr,16);
			break;
		case '0':
			switch (param[1]) {
				case 'x': *zeval=strtol(param+2,&ptr,16); break;
				default: *zeval=atoi(param); break;
			}
			break;
		default:
			*zeval=atoi(param);
			break;
	}
}
void GetTrack(char *param,int *side,int *track) {
	char *sep;

	*side=0;
	sep=strchr(param,':');
	if (sep) {
		GetValue(param,side);
		sep++;
		GetValue(sep,track);
	} else {
		GetValue(param,track);
	}

	if (*side<0 || *side>1) {
		printf(KERROR"Side must be 0 or 1\n"KNORMAL);
		exit(ABORT_ERROR);
	}
	if (*track<0 || *track>82) {
		printf(KERROR"Track must be from 0 to 81\n"KNORMAL);
		exit(ABORT_ERROR);
	}
}
void GetSector(char *param,int *side,int *track,int *sector) {
	char *sep;

	sep=strchr(param,':');
	if (sep) {
		GetValue(param,side);
		sep++;
		if (strchr(sep,':')) {
			// 3 params
			GetValue(sep,track);
			sep=strchr(sep,':')+1;
		} else {
			*track=*side;
			*side=0;
		}
		GetValue(sep,sector);
		if (*side<0 || *side>1) {
			printf(KERROR"Side must be 0 or 1\n"KNORMAL);
			exit(ABORT_ERROR);
		}
		if (*track<0 || *track>82) {
			printf(KERROR"Track must be from 0 to 81\n"KNORMAL);
			exit(ABORT_ERROR);
		}
	} else {
		printf(KERROR"Sector information must be like side:track:sector or track:sector\n"KNORMAL);
		exit(ABORT_ERROR);
	}
}

void main(int argc, char **argv) {
	int must_read=0,must_write=0;
	char *edsk_filename=NULL;
	char *edsk_filename2=NULL;
	char *output_edsk_filename=NULL;
	struct s_edsk *edsk=NULL,*edsk2=NULL;
	int i,j,k,rcpt,explore=0,merge=0,create=0,mapedsk=0,putdata=0,getdata=0,putfile=0,dumpdata=0,extragap=0,faketrigger=0,cleanextra=0,rename=0;
	char *format=NULL;
	char *infile=NULL,*outfile=NULL;
	char *datain=NULL;
	char *sep;
	int infile_offset=0,infile_size,hexasize=0x1800,tracklen=6250,usegap=0,freegap=0,side1=0,side2=0,newid=-1;
	int drop=0,add=0,droptrack=0,trackgap=0,trackfiller=0,repetition=0,export=0,fixgap=1,refix=0,fixfiller=1,sectorcut=0,sectormax=0;
	int side=0,track=0,sector=0,sectorid,sectorsize,curtrack,gap3,filler,putfile_order,sectornewsize;
	FILE *f;

	printf("edsktool build %s / roudoudou from Resistance\n\n",__DATE__);

	/*********************************************************************
	 * parameters parsing
	*********************************************************************/
	for (i=1;i<argc;i++) {
		if (argv[i][0]=='-') {
			if (strcmp(argv[i],"-h")==0 || strcmp(argv[i],"-help")==0) {
				Usage();
			} else if (strcmp(argv[i],"-putfile")==0) {
				if (i+2<argc) {
					must_write=1;
					putfile=1;
					GetSector(argv[++i],&side,&track,&sector);
					infile=argv[++i];
					// optionnal offset parameter must not be zero
					putfile_order=ORDER_ID;
					if (i+1<argc && argv[i+1][0]!='-') {
						i++;
						if (strcmp(argv[i],"PHYSICAL")==0) {
							putfile_order=ORDER_PHYSICAL;
						} else if (strcmp(argv[i],"ID")==0) {
							putfile_order=ORDER_ID;
						} else {
							printf(KERROR"-putfile option last parameter must be PHYSICAL or ID!\n"KNORMAL);
							exit(ABORT_ERROR);
						}
					}
				} else {
					printf(KERROR"-putfile option needs a position and a filename (+optionnal order PHYSICAL or ID, default is ID) to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-put")==0) {
				if (i+2<argc) {
					must_write=1;
					putdata=1;
					GetSector(argv[++i],&side,&track,&sector);
					infile=argv[++i];
					// optionnal offset parameter must not be zero
					if (i+1<argc && argv[i+1][0]!='-') { // <=== refaire la condition pour gerer le negatif
						infile_offset=atoi(argv[i+1]);
						if (infile_offset!=0) i++;
					}
				} else {
					printf(KERROR"-put option needs a position and a filename (+optionnal offset) to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-rename")==0) {
				if (i+2<argc) {
					rename=1;
					GetSector(argv[++i],&side,&track,&sector);printf("rename s%d t%d sn%d\n",side,track,sector);
					GetValue(argv[++i],&newid);printf("newid #%02X\n",newid);
				} else {
					printf(KERROR"-rename option needs a position and a sectorID in order to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-dump")==0) {
				if (i+1<argc) {
					dumpdata=1;
					GetSector(argv[++i],&side,&track,&sector);
				} else {
					printf(KERROR"-dump option needs a position to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-get")==0) {
				if (i+2<argc) {
					getdata=1;
					GetSector(argv[++i],&side,&track,&sector);
					outfile=argv[++i];
				} else {
					printf(KERROR"-get option needs a position and a filename to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-add")==0) {
				if (i+3<argc) {
					must_write=1;
					add=1;
					GetSector(argv[++i],&side,&track,&sector);
					if ((sep=strchr(argv[i],'+'))) {
						repetition=atoi(sep);
					}
					GetValue(argv[++i],&sectorid);
					GetValue(argv[++i],&sectorsize);
				} else {
					printf(KERROR"-add option needs a position, sectorID, sector size to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-trackgap")==0) {
				if (i+2<argc) {
					must_write=1;
					trackgap=1;
					GetTrack(argv[++i],&side,&track);
					if ((sep=strchr(argv[i],'+'))) {
						repetition=atoi(sep);
					}
					GetValue(argv[++i],&gap3);
				}
			} else if (strcmp(argv[i],"-sectormax")==0) {
				if (i+2<argc) {
					must_write=1;
					sectormax=1;
					GetTrack(argv[++i],&side,&track);
					if ((sep=strchr(argv[i],'+'))) {
						repetition=atoi(sep);
					}
					GetValue(argv[++i],&sectornewsize);
				} else {
					printf(KERROR"-trackfiller option needs a track + maxsize to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-trackfiller")==0) {
				if (i+2<argc) {
					must_write=1;
					trackfiller=1;
					GetTrack(argv[++i],&side,&track);
					if ((sep=strchr(argv[i],'+'))) {
						repetition=atoi(sep);
					}
					GetValue(argv[++i],&filler);
				} else {
					printf(KERROR"-trackfiller option needs a track + filler to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-droptrack")==0) {
				if (i+1<argc) {
					must_write=1;
					droptrack=1;
					GetTrack(argv[++i],&side,&track);
					if ((sep=strchr(argv[i],'+'))) {
						repetition=atoi(sep);
					}
				}
			} else if (strcmp(argv[i],"-sectorcut")==0) {
				if (i+2<argc) {
					must_write=1;
					sectorcut=1;
					GetSector(argv[++i],&side,&track,&sector);
					if ((sep=strchr(argv[i],'+'))) {
						repetition=atoi(sep);
					}
					GetValue(argv[++i],&sectornewsize);
				} else {
					printf(KERROR"-sectorcut option needs a track and a sector index +size to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-drop")==0) {
				if (i+1<argc) {
					must_write=1;
					drop=1;
					GetSector(argv[++i],&side,&track,&sector);
					if ((sep=strchr(argv[i],'+'))) {
						repetition=atoi(sep);
					}
				} else {
					printf(KERROR"-drop option needs a track and a sector index to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-map")==0) {
				mapedsk=1;
			} else if (strcmp(argv[i],"-explore")==0) {
				explore=1;
			} else if (strcmp(argv[i],"-export")==0) {
				export=1;
			} else if (strcmp(argv[i],"-forcefiller")==0) {
				fixfiller=0;
			} else if (strcmp(argv[i],"-forcegap")==0) {
				fixgap=0;
			} else if (strcmp(argv[i],"-usegap")==0) {
				usegap=1;
			} else if (strcmp(argv[i],"-freegap")==0) {
				freegap=1;
			} else if (strcmp(argv[i],"-from1")==0) {
				if (i+1<argc) {
					i++;
					side1=atoi(argv[i]);
					if (side1!=0 && side1!=1) {
						printf(KERROR"-from option needs a side (0 or 1) to run properly!\n"KNORMAL);
						exit(ABORT_ERROR);
					}
				} else {
					printf(KERROR"-from option needs a side (0 or 1) to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-from2")==0) {
				if (i+1<argc) {
					i++;
					side2=atoi(argv[i]);
					if (side2!=0 && side2!=1) {
						printf(KERROR"-from option needs a side (0 or 1) to run properly!\n"KNORMAL);
						exit(ABORT_ERROR);
					}
				} else {
					printf(KERROR"-from option needs a side (0 or 1) to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-clean")==0) {
				cleanextra=1;
				must_read=1;
			} else if (strcmp(argv[i],"-merge")==0) {
				must_read=1;
				must_write=1;
				merge=1;
			} else if (strcmp(argv[i],"-setextragap")==0) {
				if (i+1<argc) {
					i++;
					extragap=atoi(argv[i]);
					if (extragap<1) extragap=0;
				} else {
					printf(KERROR"-extragap option needs a size to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-hexasize")==0) {
				if (i+1<argc) {
					i++;
					hexasize=atoi(argv[i]);
				} else {
					printf(KERROR"-hexasize option needs a size to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-tracklen")==0) {
				if (i+1<argc) {
					i++;
					tracklen=atoi(argv[i]);
				} else {
					printf(KERROR"-tracklen option needs a size to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-o")==0) {
				if (i+1<argc) {
					i++;
					output_edsk_filename=argv[i];
					must_write=1;
				} else {
					printf(KERROR"-o option needs a filename to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			} else if (strcmp(argv[i],"-create")==0) {
				if (i+1<argc) {
					i++;
					format=argv[i];
					create=1;
					must_write=1;
				} else {
					printf(KERROR"-create option needs a format to run properly!\n"KNORMAL);
					exit(ABORT_ERROR);
				}
			}
		} else {
			if (!edsk_filename) {
				edsk_filename=argv[i];
				must_read=1;
			} else if (!edsk_filename2) {
				edsk_filename2=argv[i];
			} else {
				printf(KERROR"EDSK filenames already set!\n"KNORMAL);
				exit(ABORT_ERROR);
			}
		}
	}

	/*********************************************************************
	 * option check
	*********************************************************************/
	if (merge && !edsk_filename2) {
		printf(KERROR"cannot merge without two EDSK filenames\n"KNORMAL);
		exit(ABORT_ERROR);
	}
	if (add && (sectorsize<0 || sectorsize>6)) {
		printf(KERROR"Invalid sector size (must be 0 to 6)\n"KNORMAL);
		exit(ABORT_ERROR);
	}
	if (putfile_order!=ORDER_ID && (sector<-1 || sector>31)) {
		printf(KERROR"Sector must be from -1 to 31\n"KNORMAL);
		exit(ABORT_ERROR);
	}

	/*********************************************************************
	 * EDSK creation
	*********************************************************************/
	if (create) {
		if (format) {
			edsk=NewEDSK(format);
		} else {
			printf(KERROR"Unknown format for EDSK creation\n"KNORMAL);
			exit(ABORT_ERROR);
		}
	}
	/*********************************************************************
	 * EDSK read
	*********************************************************************/
	if (must_read) {
		if (edsk_filename) {
			edsk=EDSK_load(edsk_filename);
		} else {
			printf(KERROR"cannot read without an EDSK filename\n"KNORMAL);
			exit(ABORT_ERROR);
		}
		// twin EDSK for fusion
		if (merge && edsk_filename2) {
			edsk2=EDSK_load(edsk_filename2);
		}
	}
	/*********************************************************************
	 * datafile read
	*********************************************************************/
	if (infile) {
		int tmpsize;
		f=fopen(infile,"rb");
		if (!f) {
			printf(KERROR"Cannot open [%s] in read mode\n"KNORMAL,infile);
			exit(ABORT_ERROR);
		}
		fseek(f,0,SEEK_END);
		tmpsize=ftell(f);

		if (infile_offset<0) {
			infile_offset=tmpsize+infile_offset;
			if (infile_offset<0) {
				printf(KERROR"Cannot seek that offset for this file\n"KNORMAL);
				exit(ABORT_ERROR);
			}
		}
		infile_size=tmpsize-infile_offset;
		fseek(f,infile_offset,SEEK_SET);
		datain=malloc(infile_size);
		fread(datain,1,infile_size,f);
		printf(KIO"read %d byte%s of [%s] at offset [%d]\n"KNORMAL,infile_size,infile_size>1?"s":"",infile,infile_offset);
		fclose(f);
	}

	/*********************************************************************
	 * EDSK processing
	*********************************************************************/
	if (cleanextra) {
		int sectorlen=0;
		for (k=0;k<edsk->tracknumber;k++) {
			for (j=0;j<edsk->sidenumber;j++) {
				curtrack=k*edsk->sidenumber+j;
				if (!edsk->track[curtrack].unformated) {
					for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
						switch (edsk->track[curtrack].sector[i].size) {
							case 0:sectorlen=128;break;
							case 1:sectorlen=256;break;
							case 2:sectorlen=512;break;
							case 3:sectorlen=1024;break;
							case 4:sectorlen=2048;break;
							case 5:sectorlen=4096;break;
							default:sectorlen=0;break;
						}
						if (edsk->track[curtrack].sector[i].length<3*sectorlen && edsk->track[curtrack].sector[i].length>sectorlen) {
							edsk->track[curtrack].sector[i].length=sectorlen; // remove extra info
						}
					}
				}
			}
		}
	}
	if (freegap) {
		int sectorlen=0;
		for (k=0;k<edsk->tracknumber;k++) {
			for (j=0;j<edsk->sidenumber;j++) {
				curtrack=k*edsk->sidenumber+j;
				if (!edsk->track[curtrack].unformated) {
					for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
						switch (edsk->track[curtrack].sector[i].size) {
							case 0:sectorlen=80;break;
							case 1:sectorlen=256;break;
							case 2:sectorlen=512;break;
							case 3:sectorlen=1024;break;
							case 4:sectorlen=2048;break;
							case 5:sectorlen=4096;break;
							default:sectorlen=0;break;
						}
						if (edsk->track[curtrack].sector[i].length>sectorlen) edsk->track[curtrack].sector[i].length=sectorlen;
					}
				}
			}
		}
	}
	if (putfile+sectormax+sectorcut+trackfiller+trackgap+droptrack+drop+add+getdata+putdata+dumpdata>1) {
		printf(KERROR"Do not use multiple track/sector command at a time\n"KNORMAL);
		exit(ABORT_ERROR);
	}
	//if (trackfiller || trackgap || droptrack || drop || map || explore || add || getdata || putdata)
	if (!edsk) {
		printf(KERROR"no EDSK were loaded\n"KNORMAL);
		exit(ABORT_ERROR);
	}
	if (track+repetition>=edsk->tracknumber) {
		printf(KERROR"track (+repeat) cannot exceed EDSK max track\n"KNORMAL);
		exit(ABORT_ERROR);
	}
	if (side>=edsk->sidenumber) {
		printf(KERROR"This EDSK is not double-sided\n"KNORMAL);
		exit(ABORT_ERROR);
	}
	if (rename) {
		curtrack=track*edsk->sidenumber+side;
		if (sector>=0 || sector<edsk->track[curtrack].sectornumber && newid>=0 && newid<256) {
			edsk->track[curtrack].sector[sector].id=newid;
		} else {
			printf(KERROR"Cannot rename sector (invalid position) curtrack=%d sector=%d or invalid id!\n"KNORMAL,curtrack,sector);
			exit(ABORT_ERROR);
		}
	}
	if (dumpdata) {
		curtrack=track*edsk->sidenumber+side;
		if (sector>=0 || sector<edsk->track[curtrack].sectornumber) {
			printf("; dump side %d track %d ID=#%02X (order:%d)\n",side,track,edsk->track[curtrack].sector[sector].id,sector);
			for (i=rcpt=0;i<edsk->track[curtrack].sector[sector].length;i++) {
				if (rcpt==0) printf("defb #%02X",edsk->track[curtrack].sector[sector].data[i]);
				else printf(",#%02X",edsk->track[curtrack].sector[sector].data[i]);
				if (rcpt==15) {
					printf(" ; ");
					for (j=i-15;j<=i;j++) {
						unsigned char c;
						c=edsk->track[curtrack].sector[sector].data[j];
						if (c>31 && c<128) printf("%c",c); else printf(".");
					}
					printf("\n");
					rcpt=0;
				} else rcpt++;
			}
			if (rcpt) printf("\n");
		} else {
			printf(KERROR"Cannot dump sector (invalid position) curtrack=%d sector=%d\n"KNORMAL,curtrack,sector);
			exit(ABORT_ERROR);
		}
	}
	if (putfile) {
		curtrack=track*edsk->sidenumber+side;
		if (putfile_order==ORDER_ID) {
			for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
				if (edsk->track[curtrack].sector[i].id==sector) {
					sector=i;
					break;
				}
			}
			if (i==edsk->track[curtrack].sectornumber) {
				printf(KERROR"sector #%02X not found on track %d\n"KNORMAL,sector,track);
				exit(-1);
			}
		}
		if (sector>=0 && sector<edsk->track[curtrack].sectornumber) {
			int lasttrack=-1;
			int curpos=0;
			int nextid;

			// write sectors/tracks until the end of the file
			while (curpos<infile_size) {
				if (edsk->track[curtrack].sector[sector].length<=infile_size-curpos) {
					// plain sector
					if (track!=lasttrack) printf("\nwrite track %02d sector #%02X",track,edsk->track[curtrack].sector[sector].id); else printf(" #%02X",edsk->track[curtrack].sector[sector].id);
					lasttrack=track;

					for (i=0;i<edsk->track[curtrack].sector[sector].length;i++) {
						edsk->track[curtrack].sector[sector].data[i]=datain[curpos+i];
					}
					curpos+=edsk->track[curtrack].sector[sector].length;
				} else {
					// partial sector for last part of the file
					if (track!=lasttrack) printf("\nwrite track %02d sector #%02X (partial)",track,edsk->track[curtrack].sector[sector].id); else printf(" #%02X (partial)",edsk->track[curtrack].sector[sector].id);
					lasttrack=track;

					for (i=0;i<infile_size-curpos;i++) {
						edsk->track[curtrack].sector[sector].data[i]=datain[curpos+i];
					}
					curpos=infile_size;
				}
				// next sector
				if (curpos<infile_size) {
					switch (putfile_order) {
						case ORDER_ID:
							nextid=edsk->track[curtrack].sector[sector].id+1;
							for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
								if (edsk->track[curtrack].sector[i].id==nextid) {
									sector=i;
									break;
								}
							}
							// if not found then pick lower ID of next track
							if (i==edsk->track[curtrack].sectornumber) {
								// skip unformated track
								do {
									track++;
									if (track>=edsk->tracknumber) {
										printf(KERROR"not enough space on EDSK\n"KNORMAL);
										exit(-1);
									}
									curtrack=track*edsk->sidenumber+side;
								} while (edsk->track[curtrack].sectornumber==0);
								// find lower ID
								nextid=0;
								for (i=1;i<edsk->track[curtrack].sectornumber;i++) {
									if (edsk->track[curtrack].sector[i].id<edsk->track[curtrack].sector[nextid].id) nextid=i;
								}
								sector=nextid;
							}
							break;

						case ORDER_PHYSICAL:
							sector++;
							while (sector>=edsk->track[curtrack].sectornumber) { // pasbon
								sector=0;
								// skip unformated track
								do {
									track++;
									if (track>=edsk->tracknumber) {
										printf(KERROR"not enough space on EDSK\n"KNORMAL);
										exit(-1);
									}
									curtrack=track*edsk->sidenumber+side;
								} while (edsk->track[curtrack].sectornumber==0);
							}
							break;
						default:printf(KERROR"Internal error (putfile_order is not set)\n"KNORMAL);exit(-1);
					}
				}
			}
			printf("\n");
		} else {
			printf(KERROR"Cannot put that sector (invalid position)\n"KNORMAL);
			exit(ABORT_ERROR);
		}
	}
	if (putdata) {
		curtrack=track*edsk->sidenumber+side;
		if (sector>=0 && sector<edsk->track[curtrack].sectornumber) {
			if (infile_size<edsk->track[curtrack].sector[sector].length) {
				printf(KWARNING"not enough imported data to fill the entire sector [%d]<=[%d]\n"KNORMAL,edsk->track[curtrack].sector[sector].length,infile_size);
				for (i=0;i<infile_size;i++) {
					edsk->track[curtrack].sector[sector].data[i]=datain[i];
				}
			} else {
				for (i=0;i<edsk->track[curtrack].sector[sector].length;i++) {
					edsk->track[curtrack].sector[sector].data[i]=datain[i];
				}
			}
		} else {
			printf(KERROR"Cannot put that sector (invalid position)\n"KNORMAL);
			exit(ABORT_ERROR);
		}
	}
	if (add) {
		repetition++;
		while (repetition) {
			curtrack=track*edsk->sidenumber+side;
			if (edsk->track[curtrack].sectornumber>31) {
				printf(KERROR"Maximum number of sector per track is 32 (eDSK specifications)\n"KNORMAL);
				exit(ABORT_ERROR);
			}
			if (edsk->track[curtrack].sectornumber==0 || (sector>=-1 && sector<edsk->track[curtrack].sectornumber)) {
				edsk->track[curtrack].sector=realloc(edsk->track[curtrack].sector,(edsk->track[curtrack].sectornumber+1)*sizeof(struct s_edsk_sector));
				// simplification for first sector of the track
				if (!edsk->track[curtrack].sectornumber) {
					sector=-1;
					if (repetition==1) {
						printf(KIO"Adding sector on empty track\n");
					}
				}

				// enforce sector position
				if (sector>=edsk->track[curtrack].sectornumber) {
					printf(KERROR"Cannot insert sector after position %d while there is only %d sector(s)\n"KNORMAL,sector,edsk->track[curtrack].sectornumber);
					exit(ABORT_ERROR);
				}

				// shift sectors
				sector++;
				
				//for (i=sector;i<edsk->track[curtrack].sectornumber;i++) {
				if (edsk->track[curtrack].sectornumber)
				for (i=edsk->track[curtrack].sectornumber-1;i>=sector;i--) {
					edsk->track[curtrack].sector[i+1]=edsk->track[curtrack].sector[i];
				}
				edsk->track[curtrack].sectornumber++;
				// create new sector
				edsk->track[curtrack].sector[sector].track=track;
				edsk->track[curtrack].sector[sector].side=side;
				edsk->track[curtrack].sector[sector].id=sectorid;
				edsk->track[curtrack].sector[sector].size=sectorsize;
				edsk->track[curtrack].sector[sector].st1=0;
				edsk->track[curtrack].sector[sector].st2=0;
				switch (sectorsize) {
					default:printf(KERROR"Invalid sector size (%d)\n"KNORMAL,sectorsize);
						exit(ABORT_ERROR);
					case 0:edsk->track[curtrack].sector[sector].length=128;break;
					case 1:edsk->track[curtrack].sector[sector].length=256;break;
					case 2:edsk->track[curtrack].sector[sector].length=512;break;
					case 3:edsk->track[curtrack].sector[sector].length=1024;break;
					case 4:edsk->track[curtrack].sector[sector].length=2048;break;
					case 5:edsk->track[curtrack].sector[sector].length=4096;break;
					case 6:edsk->track[curtrack].sector[sector].length=hexasize;break;
				}
				edsk->track[curtrack].sector[sector].data=malloc(edsk->track[curtrack].sector[sector].length);

				// if it's the first one
				if (edsk->track[curtrack].sectornumber==1) {
					edsk->track[curtrack].unformated=0;
					edsk->track[curtrack].sectorsize=sectorsize;
					// standard values
					edsk->track[curtrack].gap3=0x50;
					edsk->track[curtrack].filler=0xE5;
				}

				// format sector
				for (i=0;i<edsk->track[curtrack].sector[sector].length;i++) {
					edsk->track[curtrack].sector[sector].data[i]=edsk->track[curtrack].filler;
				}
				sector--; // for multiple pass
			} else {
				printf(KERROR"Cannot add sector (invalid position) s=%d sn=%d\n"KNORMAL,sector,edsk->track[curtrack].sectornumber);
				exit(ABORT_ERROR);
			}
			track++;
			repetition--;
		}
	}
	if (drop) {
		repetition++;
		while (repetition) {
			curtrack=track*edsk->sidenumber+side;
			if (sector>=0 && sector<edsk->track[curtrack].sectornumber) {
				edsk->track[curtrack].sectornumber--;
				for (i=sector;i<edsk->track[curtrack].sectornumber;i++) {
					edsk->track[curtrack].sector[i]=edsk->track[curtrack].sector[i+1];
				}
			} else {
				printf(KERROR"Cannot drop sector (invalid position)\n"KNORMAL);
				exit(ABORT_ERROR);
			}
			track++;
			repetition--;
		}
	}
	if (droptrack) {
		repetition++;
		while (repetition) {
			curtrack=track*edsk->sidenumber+side;
			if (edsk->track[curtrack].sectornumber) {
				edsk->track[curtrack].sectornumber=0;
				free(edsk->track[curtrack].sector);
				edsk->track[curtrack].sector=NULL;
				edsk->track[curtrack].unformated=1;
			} else {
				printf(KWARNING"Track %d:%d is already unformated\n"KNORMAL,side,track);
			}
			track++;
			repetition--;
		}
	}
	if (sectorcut) {
		repetition++;
		while (repetition) {
			curtrack=track*edsk->sidenumber+side;
			if (sector>=0 && sector<edsk->track[curtrack].sectornumber) {
				if (edsk->track[curtrack].sector[sector].length>sectornewsize) {
					edsk->track[curtrack].sector[sector].length=sectornewsize;
				} else {
					printf("no resize on track %d\n",track);
				}
			} else {
				printf(KERROR"Cannot drop sector (invalid position)\n"KNORMAL);
				exit(ABORT_ERROR);
			}
			track++;
			repetition--;
		}
	}
	if (sectormax) {
		repetition++;
		while (repetition) {
			curtrack=track*edsk->sidenumber+side;
			for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
				if (edsk->track[curtrack].sector[i].length>sectornewsize) {
					edsk->track[curtrack].sector[i].length=sectornewsize;
				}
			}
			edsk->track[curtrack].filler=filler;
			track++;
			repetition--;
		}
	}
	if (trackfiller) {
		repetition++;
		while (repetition) {
			curtrack=track*edsk->sidenumber+side;
			edsk->track[curtrack].filler=filler;
			track++;
			repetition--;
		}
	}
	if (trackgap) {
		repetition++;
		while (repetition) {
			curtrack=track*edsk->sidenumber+side;
			edsk->track[curtrack].gap3=gap3;
			track++;
			repetition--;
		}
	}
	if (mapedsk) {
		MAPEDSK(edsk);
	}
	if (explore) {
		ExploreEDSK(edsk);
	}
	if (merge) {
		struct s_edsk_track *newtracks;
		int maxtrack;

		if (side1 && edsk->sidenumber<2) {
			printf(KERROR"first EDSK does not have 2 sides...\n"KNORMAL);
			exit(ABORT_ERROR);
		}
		if (side2 && edsk2->sidenumber<2) {
			printf(KERROR"second EDSK does not have 2 sides...\n"KNORMAL);
			exit(ABORT_ERROR);
		}

		// merged DSK will get the maximum track number
		if (edsk->tracknumber>edsk2->tracknumber) maxtrack=edsk->tracknumber; else maxtrack=edsk2->tracknumber;

		newtracks=malloc(sizeof(struct s_edsk_track)*maxtrack*2);
		memset(newtracks,0,sizeof(struct s_edsk_track)*maxtrack*2);

		// copie des faces
		for (i=0;i<edsk->tracknumber;i++) newtracks[i*2]=edsk->track[i*edsk->sidenumber+side1];
		for (i=0;i<edsk2->tracknumber;i++) newtracks[i*2+1]=edsk2->track[i*edsk2->sidenumber+side2];

		// new tracks are unformated
		for (i=edsk->tracknumber;i<maxtrack;i++) {
			newtracks[i*2].unformated=1;
		}
		for (i=edsk2->tracknumber;i<maxtrack;i++) {
			newtracks[i*2+1].unformated=1;
		}

		edsk->track=newtracks; // porky, memory leak powah :)
		edsk->tracknumber=maxtrack;
		edsk->sidenumber=2;
		printf("EDSK merged\n");
	}
	if (export) {
		int trackpacknumber=0;
		int posdelay[32];
		FILE *exp;
		int trackcpt=0;
		int sectorlen=0;
		int realidx;

		exp=fopen("export_definition.asm","wb");

		fprintf(exp,";****************** EDSK EXPORT FORMAT DEFINITION **************\n");
		for (track=0;track<edsk->tracknumber;track++) {
			for (side=0;side<edsk->sidenumber;side++) {
				curtrack=track*edsk->sidenumber+side;
				fprintf(exp,";*** track %02d side %d***\n",track,side);
				if (edsk->track[curtrack].unformated) {
					fprintf(exp,"defb %d,%d,6,1,#50,#50,#66,6 : defb #FF,#FF ; unformated track (no garbage ID)\n",track,side);
				} else {
					int garbageID,garbageID2,idlist[256];
					int minimalsize;
					int requestedsector=0,verifsector=0;
					memset(idlist,0,sizeof(idlist));
					memset(posdelay,0,sizeof(posdelay));
					// default size if the first size
					minimalsize=edsk->track[curtrack].sector[0].size;
					for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
						idlist[edsk->track[curtrack].sector[i].id]=1;
						if (edsk->track[curtrack].sector[i].size<minimalsize) {
							if (i<-edsk->track[curtrack].sectornumber) // do bother about last sector size
								minimalsize=edsk->track[curtrack].sector[i].size;
						}
						if (edsk->track[curtrack].sector[i].size>5) {
							minimalsize=5;
						}
					}
					for (i=255;i>=0;i--) if (!idlist[i]) {garbageID=i;break;}
					for (i=garbageID-1;i>=0;i--) if (!idlist[i]) {garbageID2=i;break;}

					// overload filler byte + warning
					switch (minimalsize) {
						case 0:sectorlen=80;break;
						case 1:sectorlen=256;break;
						case 2:sectorlen=512;break;
						case 3:sectorlen=1024;break;
						case 4:sectorlen=2048;break;
						case 5:sectorlen=4096;break;
						default:sectorlen=0;break;
					}
					if (sectorlen && fixfiller) {
						int bestfiller;
						memset(idlist,0,sizeof(idlist));
						for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
							if (edsk->track[curtrack].sector[i].size==minimalsize) {
								for (j=1;j<sectorlen;j++) {
									if (edsk->track[curtrack].sector[i].data[0]!=edsk->track[curtrack].sector[i].data[j]) break;
								}
								// filler detected
								if (j==sectorlen) {
									idlist[edsk->track[curtrack].sector[i].data[0]]++;
								}

							}
						}
						bestfiller=edsk->track[curtrack].filler;
						for (i=0;i<256;i++) {
							if (idlist[i]>idlist[bestfiller]) {
								bestfiller=i;
							}
						}
						if (idlist[bestfiller] && bestfiller!=edsk->track[curtrack].filler) {
							edsk->track[curtrack].filler=bestfiller;
							printf(KWARNING"Filler Track was changed to #%02X for side %d / track %d\n"KNORMAL,bestfiller,side,track);
						}
					}

					// check for GAP extra
					if (usegap) {
						int gaplen,gapflag=0,maxgap=0;
						int freeID=256;
						int newgap=0;

						for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
							switch (edsk->track[curtrack].sector[i].size) {
								case 0:gaplen=80;break;
								case 1:gaplen=256;break;
								case 2:gaplen=512;break;
								case 3:gaplen=1024;break;
								case 4:gaplen=2048;break;
								case 5:gaplen=4096;break;
								default:gaplen=edsk->track[curtrack].sector[i].length;break;
							}
							if (gaplen<edsk->track[curtrack].sector[i].length) {
								gaplen=edsk->track[curtrack].sector[i].length-gaplen;
								if (gaplen>maxgap) maxgap=gaplen;
								if (extragap && extragap<maxgap) maxgap=extragap;

								if (!gapflag) {
									printf(KWARNING"Extra GAP detected of %d byte(s) side %d track %d\n"KNORMAL,gaplen,side,track);
									gapflag=1;
								}
								// do we need to change format?
								if (edsk->track[curtrack].sector[i].size==minimalsize) {
									minimalsize--;
								}
								// insert fake GAP sector
								j=edsk->track[curtrack].sectornumber;
								edsk->track[curtrack].sectornumber++;
								edsk->track[curtrack].sector=realloc(edsk->track[curtrack].sector,sizeof(struct s_edsk_sector)*edsk->track[curtrack].sectornumber);
								while (j>i) {
									edsk->track[curtrack].sector[j]=edsk->track[curtrack].sector[j-1];
									j--;
								}

								edsk->track[curtrack].sector[i+1].size--;
								// find free ID
								do {
									freeID--;
									for (j=0;j<edsk->track[curtrack].sectornumber;j++) {
										if (edsk->track[curtrack].sector[j].id==freeID) break;
									}
								} while (j!=edsk->track[curtrack].sectornumber);
								edsk->track[curtrack].sector[i+1].id=freeID;
								switch (edsk->track[curtrack].sector[i+1].size) {
									case 0:sectorlen=80;break;
									case 1:sectorlen=256;break;
									case 2:sectorlen=512;break;
									case 3:sectorlen=1024;break;
									case 4:sectorlen=2048;break;
									case 5:sectorlen=4096;break;
									default:break;
								}
								edsk->track[curtrack].sector[i+1].fakegap=1;
								edsk->track[curtrack].sector[i+1].length=sectorlen;
								edsk->track[curtrack].sector[i+1].data=malloc(edsk->track[curtrack].sector[i+1].length);
								// last check
								if (maxgap>sectorlen) maxgap=sectorlen;
								// fill extra sector position
								memcpy(edsk->track[curtrack].sector[i+1].data,edsk->track[curtrack].sector[i].data+edsk->track[curtrack].sector[i].length-sectorlen,sectorlen);

								// compute new GAP
								newgap=sectorlen-60-(sectorlen-maxgap)-2;

								// A FINIR => check global sur les fakegap... => conserver le fakeoffset
								edsk->track[curtrack].gap3=newgap;
								i++;

								faketrigger=1;
							}
						}

						for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
							idlist[edsk->track[curtrack].sector[i].id]=1;
						}
						for (i=255;i>=0;i--) if (!idlist[i]) {garbageID=i;break;}
						for (i=garbageID-1;i>=0;i--) if (!idlist[i]) {garbageID2=i;break;}
					}

					for (i=0;i<edsk->track[curtrack].sectornumber-1;i++) {
						switch (edsk->track[curtrack].sector[i].size-minimalsize) {
							case 0:
								requestedsector++;
								break;
							case 1:
								if (i+1<edsk->track[curtrack].sectornumber && edsk->track[curtrack].sector[i+1].fakegap) requestedsector+=1; else requestedsector+=2;
								break;
							case 2:
								requestedsector+=4;
								break;
							case 3:
								requestedsector+=8;
								break;
							case 4:
								requestedsector+=16;
								break;
						}
					}
					// last sector does not need erased entries
					requestedsector++;

					// max length control
					if (fixgap) {
						int tmptracklen;
						int wasfixed=0;
						switch (minimalsize) {
							case 0:sectorlen=128;break; // ??? @@TOCHECK
							case 1:sectorlen=256;break;
							case 2:sectorlen=512;break;
							case 3:sectorlen=1024;break;
							case 4:sectorlen=2048;break;
							case 5:sectorlen=4096;break;
							default:
							case 6:sectorlen=0;break; // we will do what we can ^_^ aka No Size Control!
						}

						while (1) {
							tmptracklen=146+requestedsector*(edsk->track[curtrack].gap3+sectorlen+62);
							if (tmptracklen>tracklen) {
								if (faketrigger) {
									printf(KERROR"GAP cannot be fixed on fake gap track, track is too big! %d>%d\n"KNORMAL,tmptracklen,tracklen);
									exit(ABORT_ERROR);
								}
								edsk->track[curtrack].gap3--;
								if (edsk->track[curtrack].gap3<16) {
									break;
								}
								wasfixed=1;
							} else break;
						}
						if (wasfixed) {
							switch (refix) {
								case 0:case 1:case 2:
									printf(KWARNING"GAP Track was fixed for side %d / track %d\n"KNORMAL,side,track);break;
								case 3:printf(KWARNING"[...]\n"KNORMAL);break;
								default:break;
							}
							refix++;
						}

					}

					fprintf(exp,"defb %d,%d,%d,%d,%d,#%02X ; track real definition\n",track,side,minimalsize,requestedsector,edsk->track[curtrack].gap3,edsk->track[curtrack].filler);
					fprintf(exp,"defb ");
					realidx=0;
					for (i=0;i<edsk->track[curtrack].sectornumber;i++) {
						if (i) fprintf(exp,",");
						fprintf(exp,"%d,%d,#%02X,%d",edsk->track[curtrack].sector[i].track,edsk->track[curtrack].sector[i].side,edsk->track[curtrack].sector[i].id,edsk->track[curtrack].sector[i].size);
						verifsector++;

						posdelay[i]=realidx*(62+sectorlen+edsk->track[curtrack].gap3);
						realidx++;
				
						if (i+1<edsk->track[curtrack].sectornumber) // not for the last sector	

							// a paufiner rapport � la diff�rence minimalsize size > 1
						if (i+1<edsk->track[curtrack].sectornumber && !edsk->track[curtrack].sector[i+1].fakegap) {
							switch (edsk->track[curtrack].sector[i].size-minimalsize) {
								default:break;
								case 1:	fprintf(exp,",/* erased */ 0,0,%d,0",garbageID);verifsector++;realidx++;break;
								case 2:	fprintf(exp,",/* next 3 erased */ 0,0,%d,0,0,0,%d,0,0,0,%d,0",garbageID,garbageID,garbageID);verifsector+=3;realidx+=3;break;
								case 3:	fprintf(exp,",/* next 7 erased */ 0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0",garbageID,garbageID,garbageID,garbageID,garbageID,garbageID,garbageID);verifsector+=7;realidx+=7;break;
								case 4:	fprintf(exp,",/* next 15 erased */ 0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0",garbageID,garbageID,garbageID,garbageID,garbageID,garbageID,garbageID);
									fprintf(exp,",0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0,0,0,%d,0",garbageID,garbageID,garbageID,garbageID,garbageID,garbageID,garbageID,garbageID);verifsector+=15;realidx+=15;break;
							}
						} else {
							switch (edsk->track[curtrack].sector[i].size-minimalsize) {
								default:
									printf(KERROR"Track %02d integrity error! => UNSUPPORTED\n"KNORMAL,track);
									exit(ABORT_ERROR);
									break;
								case 1:break;
							}
						}
					}
					if (verifsector!=requestedsector) {
						printf(KERROR"Track %02d integrity error!\n"KNORMAL,track);
						exit(ABORT_ERROR);
					}
					fprintf(exp," ; sector definition\n");
					fprintf(exp,"defb #%02X ; garbageID\n",garbageID2);

					// sector data
					for (k=0;k<edsk->track[curtrack].sectornumber;k++) {
						int zel=0;
						int rcpt;

						if (!edsk->track[curtrack].sector[k].fakegap) continue;

						i=k;
						switch (edsk->track[curtrack].sector[i].size) {
							case 0:zel=80;break;
							case 1:zel=256;break;
							case 2:zel=512;break;
							case 3:zel=1024;break;
							case 4:zel=2048;break;
							case 5:zel=4096;break;
							case 6:zel=edsk->track[curtrack].sector[i].length;break;
						}
						if (zel!=80 && zel<edsk->track[curtrack].sector[i].length) {
							printf(KERROR"Track %02d integrity error!\n"KNORMAL,track);
							exit(ABORT_ERROR);
						}
						
						for (j=0;j<zel;j++) {
							if (edsk->track[curtrack].sector[i].data[j]!=edsk->track[curtrack].filler) break;
						}
						if (j!=zel) {
							fprintf(exp,"defb %d,%d,#%02X,%d,#%02X\n",edsk->track[curtrack].sector[i].track,edsk->track[curtrack].sector[i].side,edsk->track[curtrack].sector[i].id,edsk->track[curtrack].sector[i].size,
									edsk->track[curtrack].sector[i].st2&0x40?0x49:0x45); // DAM
							fprintf(exp,"defw %d ; pos delay\n",posdelay[i]);
							fprintf(exp,"defw %d ; real size\n",zel);

							for (j=rcpt=0;j<zel;j++) {
								if (rcpt==0) fprintf(exp,"defb #%02X",edsk->track[curtrack].sector[i].data[j]); else fprintf(exp,",#%02X",edsk->track[curtrack].sector[i].data[j]);
								if (rcpt==31) {fprintf(exp,"\n");rcpt=0;} else rcpt++;
							}
							if (rcpt!=0) fprintf(exp,"\n");
						} else {
							fprintf(exp,"; sector #%02X skipped as it is empty (format filler)\n",edsk->track[curtrack].sector[i].id);
						}
					}
					for (k=0;k<edsk->track[curtrack].sectornumber;k++) {
						int zel=0;
						int rcpt;

						if (edsk->track[curtrack].sector[k].fakegap) continue;

						i=k;
						switch (edsk->track[curtrack].sector[i].size) {
							case 0:zel=80;break;
							case 1:zel=256;break;
							case 2:zel=512;break;
							case 3:zel=1024;break;
							case 4:zel=2048;break;
							case 5:zel=4096;break;
							case 6:zel=edsk->track[curtrack].sector[i].length;break;
						}
#if 0
						if (zel!=80 && zel<edsk->track[curtrack].sector[i].length && !extragap) {
							printf(KERROR"Track %02d integrity error! sector #%02X size=%d\n"KNORMAL,track,edsk->track[curtrack].sector[i].id,edsk->track[curtrack].sector[i].size);
							exit(ABORT_ERROR);
						}
#endif
						
						for (j=0;j<zel;j++) {
							if (edsk->track[curtrack].sector[i].data[j]!=edsk->track[curtrack].filler) break;
						}
						if (j!=zel || (i+1<edsk->track[curtrack].sectornumber && edsk->track[curtrack].sector[i+1].fakegap)) {
							fprintf(exp,"defb %d,%d,#%02X,%d,#%02X\n",edsk->track[curtrack].sector[i].track,edsk->track[curtrack].sector[i].side,edsk->track[curtrack].sector[i].id,edsk->track[curtrack].sector[i].size,
									edsk->track[curtrack].sector[i].st2&0x40?0x49:0x45); // DAM
							fprintf(exp,"defw %d ; pos delay\n",posdelay[i]);
							fprintf(exp,"defw %d ; real size\n",zel);

							for (j=rcpt=0;j<zel;j++) {
								if (rcpt==0) fprintf(exp,"defb #%02X",edsk->track[curtrack].sector[i].data[j]); else fprintf(exp,",#%02X",edsk->track[curtrack].sector[i].data[j]);
								if (rcpt==31) {fprintf(exp,"\n");rcpt=0;} else rcpt++;
							}
							if (rcpt!=0) fprintf(exp,"\n");
						} else {
							fprintf(exp,"; sector #%02X skipped as it is empty (format filler)\n",edsk->track[curtrack].sector[i].id);
						}
					}





					fprintf(exp,"defb #FF,#FF ; end of sector data\n");
				}
				trackcpt++;
				if (trackcpt>=2) {
					trackcpt=0;
					fprintf(exp,"defs 8,0 ; end of pack\n");
					fprintf(exp,"save'PACK%04d.DAT',0,$,AMSDOS\n\n\n",trackpacknumber++);
					fprintf(exp,"bank\n");
				}
			}
		}

		if (trackcpt) {
			fprintf(exp,"defs 8,0 ; end of pack\n");
			fprintf(exp,"save'PACK%04d.DAT',0,$,AMSDOS\n\n\n",trackpacknumber++);
			fprintf(exp,"bank\n");
		}

		fclose(exp);
	}

	/*********************************************************************
	 * EDSK write
	*********************************************************************/
	if (must_write) {
		if (output_edsk_filename) {
			EDSK_write_file(edsk, output_edsk_filename);
		} else {
			printf(KERROR"Cannot write EDSK as there is no output filename\n"KNORMAL);
			exit(ABORT_ERROR);
		}
	}
	/*********************************************************************
	 * datafile write
	*********************************************************************/
	if (getdata) {
		unlink(outfile);
		curtrack=track*edsk->sidenumber+side;
		if (sector>=0 || sector<edsk->track[curtrack].sectornumber) {
			f=fopen(outfile,"wb");
			if (!f) {
				printf(KERROR"Cannot open [%s] for data output\n"KNORMAL,outfile);
				exit(ABORT_ERROR);
			}
			fwrite(edsk->track[curtrack].sector[sector].data,1,edsk->track[curtrack].sector[sector].length,f);
		} else {
			printf(KERROR"Cannot extract sector (invalid position) curtrack=%d sector=%d\n"KNORMAL,curtrack,sector);
			exit(ABORT_ERROR);
		}
		fclose(f);
		printf(KIO"write [%s] size=[%d]\n"KNORMAL,outfile,edsk->track[curtrack].sector[sector].length);
	}

	if (datain) free(datain);
}



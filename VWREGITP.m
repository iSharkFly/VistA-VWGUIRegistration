VWREGITP	;Portland,OR/Jim Bell,et al - Client Registration Utility
	;2.0;For WorldVistA;**LOCAL**;;;Build 4; Copyright 2015 ad infinitum et ultra    *
	;Gets data for existing clients/patients
	;*****************************************************************
	;* Licensed under GNU 2.0 or greater - see license.txt file      *
	;* Program/application is for the management of input templates  *
	;* owned by the user (DUZ).				         *
	;* REMINDER: All template fields pertain only to the Patient File*
	;*  (#2)!							 *
	;*****************************************************************
	Q  ;No fall through - jeb
	;
GPD(RESULT,DATA)	;Get patient data
	; ********************************* 8888***
	; * DATA_____TEMPLATE(IEN)^FIELDSET^DFN   *
	; * TEMPLATE__The name(IEN) of a          *
	; *            stored template            *
	; * FIELDSET_Adhoc fields in a string     *
	; *          as ".01;3;5;.131", etc       *
	; * DFN______IEN of patient file(#2)      *
	; * NOTE: TEMPLATE takes precedence       *
	; *       over FIELDSET                   *
	; *****************************************
	;
	K RESULT,AR
	N VAR,TNUM,FSET,F,DFN
	I '$L(DATA) S RESULT(0)="No information relayed. Please try again" Q
	I $P(DATA,"^",3)="" S RESULT(0)="Patient info not relayed. Please try again" Q
	S VAR="TNUM^ADHOC^DFN" F I=1:1:3 S @$P(VAR,"^",I)=$P(DATA,"^",I)
	S TNUM=$S(TNUM["(":+$P(TNUM,"(",2),1:TNUM)
	S DFN=+$P($P(DATA,"^",3),"(",2)
	S FSET=$S(TNUM:^DIE(TNUM,"DR",1,2),'TNUM&($L(ADHOC)):ADHOC,1:"")
	D GETS^DIQ(2,DFN_",","**","N","AR")
	F I=1:1:$L(FSET,";") D
	. Q:'$L($P(FSET,";",I))
	. S F=+$P(FSET,";",I)
	. S RESULT($$INR^VWREGIT)=F_"^"_$G(AR(2,DFN_",",F))
	Q
GPDM(RESULT,DATA)	;
	; ****************************************************************
	; * DATA____Parent Text^Parent field #^PATIENT IEN^TEMPLATE(IEN) *
	; ****************************************************************
	N F,SUBD,DFN,PIEN,X,RIND,FILE,X,Y,TNUM
	K MX,MAR,RESULT,AR
	S DFN=+$P(DATA,"^",3)
	S F=+$P(DATA,"^",2)
	S TNUM=+$P($P(DATA,"^",4),"(",2)
	D GETS^DIQ(2,DFN_",",F_"*;","E","AR")
	S SUBD=+$P(^DD(2,F,0),"^",2) D:+SUBD  ;Multiple field values
	. S MX="AR("_SUBD_")" F  S MX=$Q(@MX) Q:MX=""!(+$P(MX,"(",2)'=SUBD)  D:$P(MX,",",$L(MX,",")-1)'=.01
	.. S FILE=SUBD,PIEN=$P(MX,",",2,$L(MX,",")-1),PIEN=$TR(PIEN,"""","")
	.. K MAR,IMAR
	.. D GETS^DIQ(FILE,PIEN,"**","E","MAR")
	.. D GETS^DIQ(FILE,PIEN,"**","I","IMAR")
	.. S X=$Q(@"MAR"),Y=$Q(@"IMAR")
	.. I @X'=@Y S @X=@X_"("_@Y_")"
	.. S X="MAR"  ;,RIND=$$INR^VWREGIT
	.. S RIND=$$INR^VWREGIT,RESULT(RIND)="" F  S X=$Q(@X) Q:X=""  S RESULT(RIND)=RESULT(RIND)_@X_"^"
	K AR,MAR,IMAR,MX
	Q

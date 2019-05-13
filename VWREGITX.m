VWREGITX	;Portland,OR/Jim Bell, et al - World VistA GUI Pat Reg Utility
	;;2.0;WORLD VISTA;**LOCAL **;;Build 26
	;*****************************************************************
	;* Licensed under GNU 2.0 or greater - see license.txt file      *
	;* Program/application is for the management of input templates  *
	;* owned by the user (DUZ).				         *
	;* REMINDER: All template fields pertain only to the Patient File*
	;*  (#2)!							 *
	;*****************************************************************
	;No fall thru
	Q
	;
1	;CallerID = HRN; value is at $P($P(CALLERID,":",2),"^")
	S HRN=$P($P(CALLERID,":",2),"^")
	S HRN=$$HRN(HRN)
	I HRN="" S RESULT(0)="The Health Record Number (HRN) does not exist in this database"_$C(13,10)_"Please use NAME, DOB, or PHONE#."
	Q
	;
2	;CallerID = NAME; in ^2@+CALLERID
	K AR,ARR
	N HRN,PHONE,DOB,N
	S NAME=$P(CALLERID,"^",+CALLERID)
	S XNAME=NAME F  S XNAME=$O(^DPT("B",XNAME)) Q:XNAME'[NAME  D
	. S N=0 F  S N=$O(^DPT("B",XNAME,N)) Q:'+N  S AR($O(AR(" "),-1)+1)=N
	I $O(AR(" "),-1)=1 D  Q
	. S DFN=AR(1)
	. S HRN=$$HRN(DFN),HRN=$S($L(HRN):HRN,'$L(HRN):"ID-"_$P($G(^DPT(DFN,.36)),"^",3),1:"------------")
	. S DOB=$P(^DPT(DFN,0),"^",3),DOB=$E(DOB,4,5)_"/"_$E(DOB,6,7)_"/"_$E(DOB,2,3)
	. S PHONE=$P($G(^DPT(DFN,.131)),"^"),PHONE=$S($L(PHONE):PHONE,1:"<No entry>")
	. S RESULT($$INR)=HRN_"^"_$P(^DPT(DFN,0),"^")_"("_DFN_")^"_DOB_"^"_PHONE
	S N=0 F  S N=$O(AR(N)) Q:'+N  S ARR($P(^DPT(+AR(N),0),"^"),N)=+AR(N)
	S X="ARR" F  S X=$Q(@X) Q:X=""  S DFN=@X D
	. S HRN=$$HRN(DFN),HRN=$S($L(HRN):HRN,'$L(HRN):"ID-"_$P($G(^DPT(DFN,.36)),"^",3),1:"------------")
	. S DOB=$P(^DPT(DFN,0),"^",3),DOB=$E(DOB,4,5)_"/"_$E(DOB,6,7)_"/"_$E(DOB,2,3)
	. S PHONE=$P($G(^DPT(DFN,.131)),"^"),PHONE=$S($L(PHONE):PHONE,1:"<No entry>")
	. S RESULT($$INR)=HRN_"^"_$P(^DPT(DFN,0),"^")_"("_DFN_")^"_DOB_"^"_PHONE
	K AR,ARR
	Q
	;
3	;CallerID = DOB; in ^3@CALLERID
	S X=$P(CALLERID,"^",+CALLERID)
	K %DT,Y,AR
	N HRN,PHONE,N
	D ^%DT
	S N=0 F  S N=$O(^DPT("ADOB",Y,N)) Q:'+N  S AR($O(AR(" "),-1)+1)=N_"^"_Y
	I $O(AR(" "),-1)=1 D  Q  ;Only one find
	. K RESULT
	. S DFN=+AR(1)
	. S HRN=$$HRN(DFN)
	. I '$L(HRN) S HRN="ID-"_$P($G(^DPT(DFN,.36)),"^",3)
	. I '$L(HRN) S HRN="------------"
	       . S PHONE=$P($G(^DPT(DFN,.131)),"^"),PHONE=$S($L(PHONE)>0:PHONE,1:"<No entry>")
	. S RESULT($$INR)=HRN_"^"_$P(^DPT(DFN,0),"^")_"("_DFN_")"_"^"_$P(CALLERID,"^",+CALLERID)_"^"_PHONE
	K ARR S N=0 F  S N=$O(AR(N)) Q:'+N  S ARR($P(^DPT(+AR(N),0),"^"),N)=+AR(N)
	S X="ARR" F  S X=$Q(@X) Q:X=""  S DFN=@X D
	. S HRN=$$HRN(DFN)
	. I '$L(HRN) S HRN=$P($G(^DPT(DFN,.36)),"^",3)_"(ID)"
	. I '$L(HRN)!(HRN="(ID)") S HRN="------------"
	. S PHONE=$P($G(^DPT(DFN,.131)),"^"),PHONE=$S($L(PHONE)>0:PHONE,1:"<No entry>")
	. S RESULT($$INR)=HRN_"^"_$P(^DPT(DFN,0),"^")_"("_DFN_")"_"^"_$P(CALLERID,"^",+CALLERID)_"^"_PHONE
	K ARR,AR
	Q
	;
4	;CallerID = PHONE; IN ^4@+CALLERID
	S CALLERID=$TR(CALLERID,"- ()","")
	Q
	;
5	;CallerID = space-bar; IN ^2@+CALLERID
	S X=$P(CALLERID,"^",+CALLERID)
	S DFN=$G(^DISV(DUZ,"^DPT("))
	I 'DFN S RESULT(0)="Patient-Client not found" Q
	S AR(1)=DFN G 2+6  ;Direct call
	Q
	;
DE(RESULT,DATA)	;Forced hard error
	;W "
	Q
	;
HRN(IEN)	;Health Record #s from IHS PATIENT
	N N,HRNIEN,I
	S HRNIEN=""
	Q:'$D(^AUPNPAT(IEN)) HRNIEN
	S N=0 F I=1:1 S N=$O(^AUPNPAT(IEN,41,N)) Q:'+N  S HRNIEN=HRNIEN_$P($G(^AUPNPAT(IEN,41,N,0)),"^",2)_"|"
	I $E(HRNIEN,$L(HRNIEN))="|" S HRNIEN=$E(HRNIEN,1,$L(HRNIEN)-1)
	Q HRNIEN
	;
ALIST(RESULT,ALPHA,CALLERID)	;Alpha request from client
	;*****************************************************
	;* ALPHA_____Letter to look up                       *
	;* CALLERID__PIECE#:HRN^NAME(IEN)^DOB^PHONE look up  *
	;* RETURN____HRN^NAME^DOB^PHONE(Field .131 in File 2)*
	;*****************************************************	
	I '$L(ALPHA),'+CALLERID S RESULT(0)="No Alphabetical letter or HRN,Name,DOB,Phone selection..." Q
	S CALLERID=$$UP^XLFSTR(CALLERID)  ;Upcase EVERYTHING
	I +CALLERID G @+CALLERID
	N X,I,ANAME,HRN,ADOB,APHONE,Y
	K RESULT,AR,ARR
	S X="^DPT(""B"""_","_""""_ALPHA_""")"
	F I=1:1 S X=$Q(@X) Q:$S($L(ALPHA)>1:$P(X,"""",4)'[ALPHA,1:$E($P(X,"""",4))'=ALPHA)  S AR(I)=+$P(X,",",$L(X,","))
	S ARN=0 F  S ARN=$O(AR(ARN)) Q:'+ARN  D
	. S HRN=$$HRN(ARN)
	. S:'$L(HRN) HRN="---            "
	. F JJ=$L(HRN):1:15 S HRN=HRN_" "
	. S ANAME=$P(^DPT(AR(ARN),0),"^")
	. S Y=$P(^(0),"^",3)_$S($G(^DPT(AR(ARN),540000)):^(540000),1:"")
	. D DD^%DT S ADOB=Y
	. S APHONE=$P($G(^DPT(AR(ARN),.13)),"^")
	. S ARR(ANAME,ARN)=HRN_"^"_ANAME_"("_AR(ARN)_")^"_ADOB_"^"_APHONE
	S X="ARR" F  S X=$Q(@X) Q:X=""  S RESULT($$INR)=@X
	Q
	;
PLID(IEN)	;Primary Long ID, used with or in absence of HRN.
	Q $P($G(^DPT(IEN,.36)),"^",3)
	;
INR()	Q $O(RESULT(" "),-1)+1
	;
	
FIXNAME	;
	N N,X,Y,XIEN,NLENGTH,I
	S NLENGTH=0,X="AR" F  S X=$Q(@X) Q:X=""  D
	. S Y=@X,N=$P(Y,"(")_"("_+$P(Y,"(",2)_")",STR=$P(Y,")",2)
	. S NLENGTH=$S($L(N)>NLENGTH:$L(N),1:NLENGTH)
	. F I=NLENGTH:-1:$L(N) S N=N_" "
	. S Y=N_" "_STR
	. S @X=Y
	Q
GPL(RESULT,IDDATA)	;Partial patient lists
	;***********************************************
	;* IDDATA_____Contains Start^Stop alpha chars  *
	;* RESULT_____Return of results                *
	;***********************************************
	K RESULT,AR
	N N,DFN,SSN,DOB,START,STOP,NAME,XDOB ;; ,NL
	;;Get user's last patient ID
	S DFN=$G(^DISV(DUZ,"^DPT(")) D:DFN
	. S NAME=$P(^DPT(DFN,0),"^")
	. ;S SSN=$P(^(0),"^",9)
	. S HRN="HRN: "_$$HRN(DFN)  ;Health record number
	. S PLID="ID: "_$$PLID(DFN)  ;Primary Long ID
	. S DOB=$P(^(0),"^",3)
	. S XDOB=$E(DOB,4,5)_"/"_$E(DOB,6,7)_"/"_$S($E(DOB)<3:19,1:20)_$E(DOB,2,3)
	. S AR(0)=NAME_"("_DFN_")"_" "_XDOB_" "_$S($L($P(HRN,": ",2)):HRN,$L($P(PLID,": ",2)):PLID,1:"<NO ID ON FILE>")
	S START=$P(IDDATA,"^")
	S STOP=$P(IDDATA,"^",2)
	S STOP=STOP_"z"
	S STOP=$E($O(^DPT("B",STOP)))
	S STOP=$S('$L(STOP):$P(IDDATA,"^",2)_"z",1:STOP)
	S NL=0
	S N=START F  S N=$O(^DPT("B",N)) Q:N=""!($E(N)=STOP)  D
	. S DFN=$O(^(N,0))
	. S NAME=$P(^DPT(DFN,0),"^")_"("_DFN_")"
	. ;S SSN=$P(^(0),"^",9),SSN=$S('$L(SSN):"     ????",1:SSN)
	. S HRN="HRN: "_$$HRN(DFN)
	       . S PLID="ID: "_$$PLID(DFN)  ;Primary Long ID
	. S DOB=$P(^DPT(DFN,0),"^",3)
	. S XDOB=$E(DOB,4,5)_"/"_$E(DOB,6,7)_"/"_$S($E(DOB)<3:19,1:20)_$E(DOB,2,3)
	. ;W !,$J(DFN,5)," ",$J($E(NAME,1,12),12)," ",$J(SSN,10)," ",XDOB
	. S AR(NAME,DFN)=NAME_" "_XDOB_" "_$S($L($P(HRN,": ",2)):HRN,$L($P(PLID,": ",2)):PLID,1:"<NO ID ON FILE>")
	. S (DFN,NAME,SSN,DOB,XDOB)=""
	D FIXNAME
	S X="AR" F  S X=$Q(@X) Q:X=""  S RESULT($$INR)=@X
	K AR
	Q
	;
REJECT(FIELD,IEN,SUBDIC)	;Reject Asterisked,Amis,Computed fields,VA specific fields
	;This subroutine left in for possible future use
	I $L(IEN),$D(^DIZ(64850001,IEN)) Q 1  ;VA specific data field
	I FIELD["COMPONENT" Q 1  ;Pain in the butt!
	I FIELD["(VA)"!(FIELD["(CIVIL)") Q 1  ;VA fields
	I FIELD["AMIS",FIELD["SEGMENT" Q 1
	I FIELD["ELIG VERIF" Q 1
	I FIELD["ENCOUNTER CONVERSION" Q 1
	I FIELD["PROGRAMMERS U" Q 1
	I FIELD["WHO " Q 1
	I FIELD["SC AT"!(FIELD["SC%") Q 1
	I $E(FIELD)="*" Q 1  ;field marked for deletion
	I FIELD["10-10" Q 1
	I $L(IEN),$E($P($G(^DD(2,IEN,0)),"^",2))="C" Q 1  ;computed field
	I $L($G(SUBDIC)),$E($P($G(^DD(SUBDIC,IEN,0)),"^",2))="C" Q 1 ;computet in sub-dic
	Q 0  ;Passed
	;
LF(RESULT,FTYPE)	;List of assumed civilian type fields from 
	;                Patient file(#2)
	;*******************************************************************
	;*The author (me) arbitarily selected fields from the patient file *
	;* that he (me) considers to be usable by civilian VistA/CPRS users*
	;* the field count is 284 out of the 700+ fields available in the  *
	;* full patient DD. File is located at ^DIZ(64850002,              *
	;*******************************************************************
	;
	S FTYPE=$TR(FTYPE,"*&^%$#@!:;>?/., ","")  ;TMenuItem inclusions/jeb
	;S:$L(FTYPE) FTYPE=$P(^DIZ(64850003,+$P(FTYPE,"(",2),0),"^")
	S FTYPE=$$UP^XLFSTR(FTYPE)
	K RESULT
	N N,X,FIELD,FLDNO,FGRP,M,MX,MF,MFNO,MFGP,MN
	G FG:$L(FTYPE)
	; Add patient file fields
	S N=0 F  S N=$O(^DIZ(64850002,N)) Q:'+N  D
	. S X=^(N,0)
	. S FIELD=$P(X,"^")
	. S FLDNO=$P(X,"^",2)
	. S FGRP=$P(X,"^",3)
	. S RESULT($$INR)=FIELD_"("_FLDNO_")"_":"_FGRP
	. I $O(^DIZ(64850002,N,"M",0)) D
	.. S M=0 F  S M=$O(^DIZ(64850002,N,"M",M)) Q:'+M  D
	... S MX=^(M,0)
	... S MF=$P(MX,"^")
	... S MFNO=$P(MX,"^",2)
	... S MFGP=$P(MX,"^",3)
	... S RESULT($$INR)=" SF "_MF_"("_MFNO_")"_":"_MFGP
	S X="RESULT" F I=1:1 S X=$Q(@X) Q:X=""
	S RESULT(0)="Field count: "_(I-1)
	Q
	;
FG	;Fields by GROUP
	Q:'$L(FTYPE)
	K RESULT,AHF N LABEL,F,N,I
	S N=$S(+$P(FTYPE,"(",2):+$P(FTYPE,"(",2),1:$O(^DIZ(64850003,"B",FTYPE,0)))
	I 'N S RESULT($$INR)="Group not found." G FGX
	S F=0 F I=1:1  S F=$O(^DIZ(64850003,N,"F","B",F)) Q:'+F  S RESULT($$INR)=$P(^DD(2,F,0),"^")_"("_F_")"
FGX	Q
	;
FGNA(RESULT,KIND)	;Fields by sort designator
	;**********************************
	;* KIND                           *
	;*    G____Group,Field            *
	;*    N____Field#                 *
	;*    A____Alphabetical (Default) *   
	;* RESULT__Returned array         *
	;**********************************
	K RESULT
	I KIND="G" D  G FGNAX
	. K AR
	. S N=0 F  S N=$O(^DIZ(64850002,N)) Q:'+N  S X=^(N,0) D
	.. S GRP=$P(X,"^",3)
	.. S FN=$P(X,"^",2)
	.. S FIELD=$P(X,"^")
	.. S AR(GRP,FN)=FIELD_"("_FN_")"
	.. I $O(^DIZ(64850002,N,"M",0)) D
	... S MN=0 F  S MN=$O(^DIZ(64850002,N,"M",MN)) Q:'+MN  D
	.... S MX=^(MN,0)
	.... S MFN=+$P(MX,"^",2)
	.... S MFLD=$P(MX,"^")
	.... S AR(GRP,FN,MFN)="  SF  "_$P(MX,"^")_"("_$P(MX,"^",2)_")"
	. S G="" F  S G=$O(AR(G)) Q:G=""  S RESULT($$INR)="--- "_G_" ---" D
	.. S FN=0 F  S FN=$O(AR(G,FN)) Q:'+FN  S X=AR(G,FN),RESULT($$INR)=$P(X,"^") I $O(AR(G,FN,0)) S SFN=0 F  S SFN=$O(AR(G,FN,SFN)) Q:'+SFN  S RESULT($$INR)=AR(G,FN,SFN)
	I KIND="N" D  G FGNAX
	. K AR,RESULT
	. S N=0 F  S N=$O(^DIZ(64850002,N)) Q:'+N  S X=^(N,0) D
	.. S GRP=$P(X,"^",3)
	.. S FN=$P(X,"^",2)
	.. S FIELD=$P(X,"^")
	.. S AR(FN)=FIELD_"("_FN_")"
	.. I $O(^DIZ(64850002,N,"M",0)) D
	       ... S MN=0 F  S MN=$O(^DIZ(64850002,N,"M",MN)) Q:'+MN  D
	       .... S MX=^(MN,0)
	       .... S MFN=+$P(MX,"^",2)
	       .... S MFLD=$P(MX,"^")
	       .... S AR(FN,MFN)="  SF  "_$P(MX,"^")_"("_$P(MX,"^",2)_")"
	. S X="AR" F  S X=$Q(@X) Q:X=""  S RESULT($$INR)=@X
	;Kind = alphabetical
	S N=0 F  S N=$O(^DIZ(64850002,N)) Q:'+N  S X=^(N,0) D
	. S AR($P(X,"^"))=$P(X,"^")_"("_$P(X,"^",2)_")"
	. I $O(^DIZ(64850002,N,"M",0)) D
	       .. S MN=0 F  S MN=$O(^DIZ(64850002,N,"M",MN)) Q:'+MN  D
	       ... S MX=^(MN,0)
	       ... S MFN=+$P(MX,"^",2)
	       ... S MFLD=$P(MX,"^")
	... S AR($P(X,"^"),MFLD)="  SF  "_$P(MX,"^")_"("_$P(MX,"^",2)_")"
	S X="AR" F  S X=$Q(@X) Q:X=""  S RESULT($$INR)=@X
FGNAX	;K AR
	Q
	;
RETGRP(RESULT)	;Return Group IDs
	K RESULT
	S N=0 F  S N=$O(^DIZ(64850003,N)) Q:'+N  S RESULT($$INR)=$P(^(N,0),"^",2)_"("_N_")"
	Q
	;
AHF(RESULT,AHF)	;Ad hoc field selection "Finished" pressed/jeb
	;*****************************************************
	;* AFH ARRAY:                                        *
	;*   AHF(0)____DFN                                   *
	;*   AHF ARRAY_FIELD(NO) OR FIELD(NO;SUB-DIC)        *
	;*****************************************************
	;W "  ;the END
	K ^DIZ("AHF") M ^DIZ("AHF")=AHF
	K RESULT
	N FIELD,FNO,DFNDR
	S DFNDR=""
	S DFN=+AHF(0) K AHF(0)
	S X="AHF" F  S X=$Q(@X) Q:X=""  S Y=@X D
	. S FIELD=$P(Y,"(")
	. S FNO=+$P(Y,"(",2)
	. D GFA(FNO)
	. S RESULT($$INR)=FIELD_"^"_FNO_"^^"_FHELP_"^"_FPSC_"^"_$$MF(FNO)
	. S DFNDR=DFNDR_FNO_";"
	I DFN D
	. K AR N N,Y,F
	. D GETS^DIQ(2,DFN_",",DFNDR,"E","AR","ERR")
	. S X="AR" F  S X=$Q(@X) Q:X=""  D
	.. S Y=@X
	.. S F=+$P(X,",",$L(X,",")-1)
	.. S N=0 F  S N=$O(RESULT(N)) Q:'+N  I $P(RESULT(N),"^",2)=F S $P(RESULT(N),"^",3)=Y
	;ToDo: write fill in for the multiple fields
	K FHELP,FPSC
	Q
	;
GFA(FNO)	;Get field attributes at piece3 and help
	S (FHELP,FPSC)=""
	S FHELP=$G(^DD(2,FNO,3))
	I FNO'=27.02 S N=0 F  S N=$O(^DD(2,FNO,21,N)) Q:'+N  S FHELP=FHELP_^(N,0)
	S FHELP=$TR(FHELP,"'","`")
	S FPSC=$P(^DD(2,FNO,0),"^",3)
	Q
	;
MF(X)	;Check for multiple field
	;*****************************************************
	;* Reminder: This data set is Patient file only (#2) *
	;* MYESNO____=1 is a parent                          *
	;*           =0 is a primary field                   *
	;*****************************************************
	;
	S MYESNO=$S(+$P(^DD(2,X,0),"^",2):1,1:0)
	Q MYESNO
	;
	

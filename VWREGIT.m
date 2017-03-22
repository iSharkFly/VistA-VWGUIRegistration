VWREGIT	;Portland,OR/Jim Bell, et al Patient Registration Utility August 2015
	;;2.0;B/FProductions,LLC,WORLD VISTA;**LOCAL**;;Build 2
	;*******************************************************************
	;* VW Registration is designed for patient specific fields as      *
	;* defined in Fileman Input Templates or ad hoc field selection.   *
	;* Copyright Martius/MMXV ad infinitum (GNU License: See GPLv3.txt)*
	;*******************************************************************
	;;NO FALL THROUGH - JEB
	Q
	;
TFM(XF)	;TemplateField Management
	;***********************************************
	;* Check primary field entries for "parentage" *
	;* Add an "*" to gain all sub-fields of the    *
	;* parent                                      *
	;* REMEMBER: All fields pertain to file 2 only *
	;***********************************************
	N I,N,FIELD
	K FARRAY
	I '$L(XF),'$G(TNUM) Q ""
	I '$L(XF),+$G(TNUM) S XF=^DIE(TNUM,"DR",1,2)
	F I=1:1:$L(XF,";") S:$L($P(XF,";",I)) FARRAY(I)=+$P(XF,";",I)
	S N=0 F I=1:1 S N=$O(FARRAY(N)) Q:'+N  D
	. S FIELD=FARRAY(N)
	. I +$P(^DD(2,FIELD,0),"^",2) S FIELD=FIELD_"*",FMARRAY(FIELD)=$P(^(0),"^",4) K FARRAY(N)
	S XF="",N=0 F  S N=$O(FARRAY(N)) Q:'+N  S XF=XF_FARRAY(N)_";"
	Q XF
	;
CHECK()	;
	Q ""
	;
INR()	Q $O(RESULT(" "),-1)+1
	;
EN(RESULT)	   ;Template name and ID labels
	;Get the input template list
	;housekeeping
	S DTIME=99999
	;ZSY "chmod 777 "_$ZD_"regparam/*.txt" ;Moved to post-install
	;end housekeeping
	;
	K AR,RESULT
	N N,HD,FILE,LOC,P4,P5,%ZISHF,%ZISHO,DEFST,XTMP,X
	S RESULT(0)=1
	S DEFST="";
	;S DEFTMP=$O(^DIE("B","FAU_EDU",0)) ;For Florida College only
	S RESULT(0)=$$CONTROL^VWREGITU()
	S RESULT(1)="-1^No templates found"
	S DEFST=$$GET^XPAR("ALL","VW REG DEFAULT STATE")
	S DEFTMP=$$GET^XPAR("ALL","VW REG RDNPT")
	S HD=$$GET^XPAR("ALL","VW GUI REG TEMPLATE DIRECTORY")
	I '$L(HD) K RESULT D  Q
	. S RESULT($$INR)="-1^NO HOME DIRECTORY - refer to IT support, if necessary."
	. S RESULT($$INR)="No home directory has been supplied which indicates"
	. S RESULT($$INR)="the VWREG installation is incomplete. See the Help manual"
	. S RESULT($$INR)="for installation and Enter/Editing parameter values."
	. S RESULT($$INR)="Thank you,"
	. S RESULT($$INR)="      The Management."
	S FILE="regit.txt"
	S P4=1
	S P5=""
	S X=$$FTG^%ZISH(HD,FILE,$NA(AR(1)),P4,P5)
	D:+RESULT(0)
	. S $P(RESULT(0),"^",2)=$G(HD)
	. S $P(RESULT(0),"^",3)=$S(DEFST:$P(^DIC(5,DEFST,0),"^")_"("_DEFST_")",1:"")
	. S $P(RESULT(0),"^",4)=$S(+DEFTMP:$P(^DIE(DEFTMP,0),"^")_"("_DEFTMP_")",1:DEFTMP)
	. S $P(RESULT(0),"^",5)=DUZ
	I $O(AR(0)) S RESULT(1)="[TEMPLATES]"
	S N=0 F  S N=$O(AR(N)) Q:'+N  D
	. Q:$E(AR(N))="*"
	. Q:'+$P(AR(N),"(",2)
	. Q:$P($G(^DIE(+$P(AR(N),"(",2),0)),"^",4)'=2  ;must be pat file
	. S RESULT($$INR)=AR(N)
	S RESULT($$INR)="[ID]"
	;S N=0 F  S N=$O(^DIZ(64850003,N)) Q:'+N  S RESULT($$INR)=$P(^(N,0),"^",2)_"("_$P(^(0),"^")_")"
	;S N=0 F  S N=$O(RESULT(N)) Q:'+N  K:RESULT(N)="" RESULT(N)
	I '$O(RESULT(0)) S RESULT(1)="-1^No PATIENT FILE templates found"
	K AR
	Q
	;
NPT(RESULT,TNAME)	;
	; *************************************************
	; * Incoming: DFN^TEMPLATE NAME(IEN)              *
	; * Process : Get template fields plus any help   *
	; *           If +TNAME (a DFN), get DFN data for *
	; *           the template fields (Put data in    *
	; *           $P(RESULT(N),"^",3))                *
	; * Return  : RESULT(N), etc                      *
	; *************************************************
	;W "  ;Intentional bust for debugging
	N N,TNUM,FIELDS,F,FNAME,FVALUE,FHELP,FPSC,FNUM
	S TNUM=+$P(TNAME,"(",2),DFN=+TNAME
	I 'TNUM S RESULT(0)="0^new patient Template not found" Q
	S TNAME=$P($P(TNAME,"^",2),"(")
	S TNAME=$TR(TNAME,"$&*","")  ;Clean out TMENU chars
	I TNAME="GENERIC INS. FRM [WorldVistA]" G GIF
	S FIELDS=$G(^DIE(TNUM,"DR",1,2))
	I '$L(FIELDS) Q
	K RESULT S (FNUM,FCAP)=""
	F I=1:1:$L(FIELDS,";")-1 D
	. S F=$P(FIELDS,";",I)
	. Q:'$D(^DD(2,F))  ;Not existing in this patient file
	. I F["~" S FNUM=+F,FNAME=$P($P(F,"~"),FNUM,2),F=FNUM K FNUM
	. S FNAME=$S($L($G(FNAME)):FNAME,$L($G(^DD(2,F,.1))):$P(^(.1),"^"),1:$P(^DD(2,F,0),"^"))
	. S FVALUE=""  ;Patient Data
	. S FHELP=$G(^DD(2,F,3))
	. I F'=27.02,'$L(FHELP) S N=0 F  S N=$O(^DD(2,F,21,N)) Q:'+N  S FHELP=FHELP_^(N,0)
	. S FHELP=$TR(FHELP,"'","`")
	. S FPSC=$P(^DD(2,F,0),"^",3)
	. S SUBDIC=+$P(^DD(2,F,0),"^",2)
	. S RESULT($$INR)=FNAME_"^"_F_"^"_FVALUE_"^"_FHELP_"^"_FPSC_$S(SUBDIC:"^1",1:"^0")
	. S (FNAME,FVALUE,FHELP,FPSC)=""
	G NPTX:'DFN
	I DFN D GETS^DIQ(2,DFN_",","**","EN","AR")  ;,RESULT(0)=$$DFNID^VWREGITU
	K FIELD S N=0 F  S N=$O(RESULT(N)) Q:'+N  S FIELD($P(RESULT(N),"^",2))=""
	S X="AR" F  S X=$Q(@X) Q:X=""  D
	. S FILE=+$P(X,"(",2)
	. S FIELD=+$P(X,",",$L(X,",")-1)
	. I $D(FIELD(FIELD)) S FIELD(FIELD)=@X
	S N=0 F  S N=$O(FIELD(N)) Q:'+N  D
	. S N2=0 F  S N2=$O(RESULT(N2)) Q:'+N2  I $P(RESULT(N2),"^",2)=N S $P(RESULT(N2),"^",3)=FIELD(N)
	. S RESULT(0)=$$DFNID^VWREGITU()
NPTX	K FIELD,AR,FCAP,FILE,SUBDIC,N,N2,DFN
	Q
	;
PF(RESULT,XPF)	;Pointer file - get the stuff
	K RESULT,AR
	N X,N
	I '$L(XPF) S RESULT(0)="???" Q
	S XPF="^"_XPF
	I +$P(XPF,"(",2)=.85 G NAUTPF  ;Naughty file!
	S N=0 F  S N=$O(@(XPF_N_")")) Q:'+N  S X=$P(^(N,0),"^"),AR(X,N)=X_"("_N_")"
	S X="AR" F  S X=$Q(@X) Q:X=""  S RESULT($$INR)=@X
	K AR
	Q
	;
NAUTPF	;The "NAUGHTY" pointer file - has a numeric .01 - Bad file !!!
	S N=0 F  S N=$O(@(XPF_N_")")) Q:'+N  S X=$P(^(N,0),"^") D
	. S LANG=$P(^(0),"^",2)
	. S AR(LANG,N)=LANG_"("_N_")"
	S X="AR" F  S X=$Q(@X) Q:X=""  S RESULT($$INR)=@X
	Q
GIF	;Generic Insurance form
	K RESULT
	S RESULT($$INR)="Insurance Company^2.312;.01^^^DIC(36,^0"
	S RESULT($$INR)="Group Plan^2.312;.18^^^IBA(355.3,^0"
	S RESULT($$INR)="Policy No.^2.312;1^^^^0"
	;S RESULT($$INR)="Type of Plan^^^^^0"
	S RESULT($$INR)="Coverage^355.33;40.09^^^IBE(355.1,^0"
	S RESULT($$INR)="Effective Date^2.312;8^^^^0"
	S RESULT($$INR)="Expiration Date^.3121;^^^^0"
	S RESULT($$INR)="Guarantor^^^^^0"
	S RESULT($$INR)="Signature on File^^^^0:NO;1:YES^0"
	S RESULT($$INR)="Employer^2.312;2.015^^^^0"
	S RESULT($$INR)="Billing Address^2.312;2.02^^^^0"
	S RESULT($$INR)="Billing Address(cont)^2.312;2.03^^^^0"
	S RESULT($$INR)="Postal Code^2.312;2.07^^^^0"
	S RESULT($$INR)="City^2.312;2.05^^^^0"
	S RESULT($$INR)="County/Region/Area^^^^^0"
	S RESULT($$INR)="State/Province/Region^2.312;2.06^^^DIC(5,^0"
	Q
	;
	

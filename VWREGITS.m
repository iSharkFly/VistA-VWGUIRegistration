VWREGITS	;Portland,OR/jeb et al Save utility for VWREG* routines 11/2015
	;V.2;;**LOCAL**;;;Build 3
	;License: See License.txt that with install
	;No fall thru - jeb
	Q
	;
	;* *****************************************************************
	;* Data coming in may be for a new case or existing case           *
	;* Incoming: Array LDATA=                                          *
	;*   LDATA(1)=Field^Field number^value^[optional]DFN               *
	;*                       LDATA(N...)=Field^Field number^value      *
	;* Exception for Multiples:                                        *
	;*   LDATA(N)=Field(SUBDD;Field number):value(IEN)^...etc for every*
	;*            field that is a dependent of the parent              *
	;* Process:                                                        *
	;*  1. call is at Label SAVE                                       *
	;*  2. Some housekeeping that this programmer needs to do proper   *
	;*     string evaluations.                                         *
	;*  3. Filing of a new case with FILE^DICN.                        *
	;*  4. Remaining major fields are filed with DIE                   *
	;*  5. Multiples are filed with UPDATE^DIE                         *
	;*  6. Existing entries will contain only edited data and will     *
	;*     address those fields as in 4 & 5.                           *
	;* Bon Appettit, et al.                                            *
	;*******************************************************************
SAVE(RESULT,LDATA)	;
	K RESULT,^DIZ("DS",$J)
	M ^DIZ("DS",$J)=LDATA
	;Q ;Testing
	N DFN,DIC,DA,DR,VAR,FIELD,N,N1,X,Y,DIE,DIK
	I $D(LDATA)<10 S RESULT(0)="-1: No data sent for filing. Please contact your IT dept." Q
	;UPcase everyTHING
	S XDAT="LDATA" F  S XDAT=$Q(@XDAT) Q:XDAT=""  S @XDAT=$$UP^XLFSTR(@XDAT)
	;
	;Incoming housekeeping
	S X="LDATA" F I=1:1 S X=$Q(@X) Q:X=""  I @X[":",@X[";" S ^DIZ("DS",$J,I)=@X K @X
	I +$P(@$Q(LDATA),"^",4)!(+$P(@$Q(LDATA),"(",2)) G EXP ;DFN sent by client
	S N=0 F  S N=$O(LDATA(N)) Q:'+N  I +$P($G(^DD(2,+$P(LDATA(N),"^",2),0)),"^",2) K LDATA(N)
	S DFN=$$FIND1^DIC(2,"","M",$P(LDATA(1),"^",3),"","","ERR")
	G EXP:DFN  ;Found patient/client
	;End housekeeping;
	;
	S X=$P(LDATA(1),"^",3) D
	. S DIC="^DPT(",DIC(0)="LZ" K D0 D FILE^DICN S (DA,DFN)=+Y
	. S DIC="^AUPNPAT(",DIC(0)="LZ",X=DFN,DINUM=X,DIC(0)="L" D FILE^DICN
	. S DIE=DIC,DR=.03_"////^S X=DT" D ^DIE
	. S DR=.11_"////^S X=DUZ" D ^DIE
LDPT	L +^DPT(DFN):1 G LDPT:'$T
	S N=1 F  S N=$O(LDATA(N)) Q:'+N  D
	. Q:$P(LDATA(N),"^",2)[";"
	. Q:'+$P(LDATA(N),"^",2)  ;Marker of some kind
	. S FIELD=$P(LDATA(N),"^",2)
	. S VAR=$P(LDATA(N),"^",3)
	. I FIELD=.03 D
	.. S VAR=$$DC(VAR)
	.. S VARTIME=$P(VAR,".",2),VAR=$P(VAR,".")
	.. I $L(VARTIME) D
	... N FDA
	... S FDA(2,DFN_",",540000.1)=VARTIME
	... D FILE^DIE("E","FDA")
	... D CLEAN^DILF
	. S:VAR["(" VAR=$S($L(VAR,"(")>2:+$P(VAR,"(",$L(VAR,"(")),1:+$P(VAR,"(",2))
	. S DIE="^DPT(",DR=FIELD_"///"_$S(+VAR:"/",1:"")_"^S X=VAR" D ^DIE
	L -^DPT(DFN)
	D M  ;File any multiple fields
	S RESULT(0)="Filed..."
	;K ^DIZ("DS",$J)
	Q
	;
EXP	;Existing Patient
	K X,FNAME,FFLD,FVALUE,AR,DIC,DA,DR,DIE,AR
	S X="LDATA" F  S X=$Q(@X) Q:X=""  I @X[":" S AR($O(AR(" "),-1)+1)=@X K @X
	S N=0 F  S N=$O(LDATA(N)) Q:'+N  S X=LDATA(N) D
	. S FNAME=$P(X,"^")
	. S FFLD=$P(X,"^",2)
	. S FVALUE=$S($P(X,"^",3)["(":+$P(X,"(",2),1:$P(X,"^",3))
	. S DFN=$P(X,"^",4)
	. S DIE="^DPT(",DA=DFN,DR=FFLD_"///^S X=FVALUE" D ^DIE
	D M
	S RESULT($I(RESULT))="Filed..."
	K X,FNAME,FFL,FVALUE,DFN,AR,DIE,DA,DR,DIC
	Q
	;
M	;File any multiples values; DFN should be defined above
	Q:'$D(^DIZ("DS",$J))
	M MULTS=^DIZ("DS",$J)
	K MAR S N=0 F  S N=$O(MULTS(N)) Q:'+N  D
	. F J=1:1:$L(MULTS(N),"^")-1 S MAR(J)=$P(MULTS(N),"^",J)
	. S MX=$O(MAR(0))
	. S MXFILE=+$P(MAR(MX),"(",2)
	. S MXFLD=+$P(MAR(MX),";",2)
	. S MXVAL=$P($P(MAR(MX),":",2),"(")
	. I MXFLD=.01 S MXDATA(MXFILE,"?+1,"_DFN_",",MXFLD)=MXVAL K IEN D UPDATE^DIE("E","MXDATA","IEN","ERROR") Q:$G(DIERR)  D
	.. S RECORD=$G(IEN(1)),INC=$G(IEN(1,0))
	.. S J=MX F  S J=$O(MAR(J)) Q:'+J  D
	... s MXFILE=+$P(MAR(J),"(",2)
	... S MXFLD=+$P(MAR(J),";",2)
	... S MXVAL=$P(MAR(J),":",2),MXVAL=$S(MXVAL["(":$P(MXVAL,"("),1:MXVAL)
	... S MXDATA(MXFILE,$S(MXFLD=.01:INC,1:"")_"1,"_RECORD_","_DFN_",",MXFLD)=MXVAL
	... K IEN,ERROR D UPDATE^DIE("E","MXDATA","IEN","ERROR")
	Q
	;
DC(XDATE)	;Convert DOB to internal
	N %DT,X
	S X=XDATE,%DT="T" D ^%DT
	Q Y
	;
INSUR	;Insurance/Billing
	Q
	;
K	S DA=$P(^DPT(0),"^",3),DIK="^DPT(" D ^DIK
	S DIK="^AUPNPAT(" D ^DIK
	Q
	;

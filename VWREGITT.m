VWREGITT	;Portland\Jim Bell,  Input Template Management 2016
	;2.0**LOCAL** Copyright April 2016 ad infinitum;;;;;Build 4
	;*****************************************************************
	;* Licensed under GNU 2.0 or greater - see license.txt file      *
	;* Program/application is for the management of input templates  *
	;* owned by the user (DUZ).                                      *
	;* REMINDER: All template fields pertain only to the Patient File*
	;*  (#2)!                                                        *
	;*****************************************************************
	Q  ;No fall through
	;
AUTH(TUSER,TNUM)	;Can user edit or is IT CONTROL
	N TMO
	S TMO=$O(^DIC(19,"B","VW REG IT CONTROL",0)) I $D(^VA(200,TUSER,203,"B",TMO)) Q 1
	S TMO=$O(^DIC(19,"B","VW PATIENT REGISTRATION",0))
	I TMO,$P(^DIE(TNUM,0),"^",5)=TUSER Q 1
	Q 0
	;
INR()	Q $O(RESULT(" "),-1)+1
	;
CF(FIELD)	;If a computed field, 0, else 1
	I $P($G(^DD(2,FIELD,0)),"^",2)["C" Q 0
	Q 1
	;
EGF(RESULT,TNAME)	;Get fields for client editing via TName
	;*************************
	;* Incoming___TNAME(IEN) *
	;*************************
	K RESULT  ;N TNUM,TNAME,PF,SF
	S TNUM=+$P(TNAME,"(",2)
	S TNAME=$P(TNAME,"(")
	I 'TNUM!('$D(^DIE(TNUM))) S RESULT(0)="Template name or number not found in Template file" Q
	;Check for authorization
	I '$$AUTH(DUZ,TNUM) S RESULT(0)="Sorry, you are not authorized to edit this template." Q
	S RESULT(0)="Editing "_TNAME_"("_TNUM_")"
	S PF=$G(^DIE(TNUM,"DR",1,2))
	F I=1:1:$L(PF,";") D:$P(PF,";",I) 
	. S RESULT($$INR)=$P(^DD(2,$P(PF,";",I),0),"^")_"("_$P(PF,";",I)_")"
	. S SDD=+$P(^DD(2,$P(PF,";",I),0),"^",2) D:SDD
	.. S SDN=1 F  S SDN=$O(^DIE(TNUM,"DR",SDN)) Q:'SDN  S:$O(^(SDN,0))=SDD SF=^(SDD) D
	... F J=1:1:$L(SF,";") D:$P(SF,";",J)
	.... S SFF=$P(^DIE(TNUM,"DR",SDN,SDD),";",J)
	.... S RESULT($$INR)="  SF  "_$P(^DD(SDD,SFF,0),"^")_"("_SFF_";"_SDD_")"
	Q
	;
SFLDS	;Get sub-fields and dics
	K MULT N N,X,I,Y
	S Y="",N=0 F  S N=$O(TDATA(N)) Q:'+N  D
	. Q:TDATA(N)'["  SF"  ;Still a major field
	. F I=N:1:$O(TDATA(" "),-1) S X=TDATA(I) Q:X'["  SF"  S MULT(+$P(X,";",2),+$P(X,"(",2))=""
	Q
	;
FIELDS()	;
	N FLDLIST,N,X,FLD K MULT
	S FLDLIST=""
	S N=0 F  S N=$O(TDATA(N)) Q:'+N  D:TDATA(N)'["  SF"
	. S FLD=+$P(TDATA(N),"(",2)
	. Q:'$$CF(+$P(TDATA(N),"(",2))  ;Computed field
	. S FLDLIST=FLDLIST_FLD_";"
	;Collate thru for multiple fields:entry looks like "  SF  "
	S N=0 F  S N=$O(TDATA(N)) Q:'+N  D:TDATA(N)["  SF"
	. S X=$P(TDATA(N),"  ",3)
	. S SDD=+$P(X,";",2)
	. S SFL=+$P(X,"(",2)
	. S MULT(SDD,SFL)=""
	S N=0 F  S N=$O(MULT(N)) Q:'+N  D  S SUB(N)=MF
	. S MF="",N2=0 F  S N2=$O(MULT(N,N2)) Q:'+N2  S MF=MF_N2_";"
	K MULT
	Q FLDLIST
	;
RTF(RESULT)	;Send a refresh of regit.txt to client
	K AR,RESULT
	D LTF
	M RESULT=AR
	K AR
	Q
	;
LTF	;Load the regit.txt file into AR()
	S HD=$$GET^XPAR("ALL","VW GUI REG TEMPLATE DIRECTORY")
	S FILE="regit.txt"
	S P4=1
	S P5=""
	S X=$$FTG^%ZISH(HD,FILE,$NA(AR(1)),P4,P5)
	Q
	;
FTF	;File the AR() to regit.txt
	ZSY "cp "_HD_"regit.txt "_HD_"regitbu.txt"
	S P4=1,P5="",FILE="regit.txt"
	S X=$$GTF^%ZISH($NA(AR(1)),1,HD,FILE)
	Q
	;
ITCNTRL(USER)	;Check for control capability and user authorization
	N ITCNTRL
	S ITCNTRL=$O(^DIC(19,"B","VW REG IT CONTROL",0))
	I 'ITCNTRL D  Q 0
	. S VAL=0
	. S RESULT(0)="-1^VW REGISTRATION does not appear to be complete."
	. S RESULT(1)="Please contact your Supervisor or IT support."
	. S RESULT(2)="Thank you,"
	. S RESULT(3)="The Management"
	I '$D(^VA(200,USER,203,"B",ITCNTRL)) D  Q 0
	. S RESULT(0)="-1^User does not have authorization to modify/create"
	. S RESULT(1)="input templates. Please contact your Supervisor or"
	. S RESULT(2)="IT support. Or, questions can be referred to Jim"
	. S RESULT(3)="Bell at jbellco65@gmail.com"
	. S RESULT(4)="Thank you."
	Q 1
	;
EN(RESULT,TDATA)	;
	;************************************************
	;* Call from Client                             *
	;* TDATA Array:                                 *
	;*   0____Template Name^DUZ^ACTION^WRITEACCESS  *
	;*   1-n__Field name(number)                    *
	;************************************************
	; -- testing --
	;M ^DIZ("TDATA",$J)=TDATA
	;Q
	; -- end testing --
	;
	N TNAME,TNUM,ITCNTRL,ACTION,FIELDS,CALLER
	S CALLER=""
	S X="TDATA" F  S X=$Q(@X) Q:X=""  S @X=$$UP^XLFSTR(@X)  ;Upcase everyTHING
	I '$L($G(HD)) S HD=$$GET^XPAR("ALL","VW GUI REG TEMPLATE DIRECTORY")
	S WHO=$P(TDATA(0),"^",2)
	S ITCNTRL=$$ITCNTRL(WHO)  ;1=full action;0=create/edit own template(s)
	S TNUM=+$P($P(TDATA(0),"^"),"(",2)
	S TNAME=$P($P(TDATA(0),"^"),"(")
	I TNAME["Editing" S SPEC("Editing ")="",TNAME=$$REPLACE^XLFSTR(TNAME,.SPEC)
	S ACTION=$P(TDATA(0),"^",3)
	S WRITEACC=$S($P(TDATA(0),"^",4)="SELF":$P(^VA(200,DUZ,0),"^",4),1:"")
	S FIELDS=$$FIELDS
	I '$L(ACTION) S RESULT(0)="-1^No action sent. I don't know what to do." Q
	D @ACTION
	Q
	;
CREATE	;Create a new input template
	;******************************
	;* Check for computed fields  *
	;******************************
	K RESULT N %DT,X,Y
	S %DT="TS",X="NOW" D ^%DT S FDATE=Y
	S X=TNAME,DIC="^DIE(",DIC(0)="LZ" D FILE^DICN
	S $P(^DIE(+Y,0),"^",2)=FDATE,$P(^(0),"^",3)="",$P(^(0),"^",4)=2,$P(^(0),"^",5)=DUZ
	S $P(^DIE(+Y,0),"^",6)=WRITEACC
C2	S ^DIE(+Y,"DR",1,2)=FIELDS
	;Do mult fields here
	S N=0 F  S N=$O(SUB(N)) Q:'+N  D
	. S UP=^DD(N,0,"UP")
	. I UP=2 S ^DIE(+Y,"DR",$O(^DIE(+Y,"DR"," "),-1)+1,N)=SUB(N)
	. E  S ^DIE(+Y,"DR",$O(^DIE(+Y,"DR"," "),-1),N)=SUB(N)
	I $P(^DIE(+Y,0),"^")=$P(TDATA(0),"^") S RESULT(0)=$P(Y,"^",2)_" filed"
	Q:CALLER="EDIT"
	S TNUM=+Y,TNAME=$P(Y,"^",2)
	K AR
	D LTF  ;Get the regit.txt file loaded into AR()
	S LAST=$O(AR(" "),-1)
	S AR(LAST)=TNAME_"("_TNUM_")"
	S AR(LAST+1)="[ID]"
	;M ^DIZ("TDATA","AR",$J)=AR  ;Testing
	D FTF  ;File AR() to regit.txt
	K ^DIZ("TDATA",$J)
	Q
	;
EDIT	;Edit existing. Check for allowability
	S Y=TNUM_"^"_TNAME
EL	L -^DIE(TNUM):1 G EL:'$T
	S S=1 F  S S=$O(^DIE(TNUM,"DR",S)) Q:'+S  D
	. S SUBD=0 F  S SUBD=$O(^DIE(TNUM,"DR",S,SUBD)) Q:'+SUBD  K ^DIE(TNUM,"DR",S,SUBD)
	S CALLER="EDIT"
	D C2
	L +^DIE(TNUM)
	S DA=TNUM,DIK="^DIE(" D IX^DIK  ;Re-index record just in case...
	S RESULT(0)=Y_" modification filed..."
	Q
	;
DELETE	;********************************************
	K AR
	N I,J,X
	S X="TDATA" F  S X=$Q(@X) Q:X=""  D
	. Q:'$L($P(@X,"^",3))  ;No entry
	. K @X
	M AR=TDATA K TDATA
	D FTF
	I X S RESULT(0)="Template menu list updated."
	E  S RESULT(0)="Template list not updated. Advise Template manager to manually update "_HD_"regit.txt"
	Q
	

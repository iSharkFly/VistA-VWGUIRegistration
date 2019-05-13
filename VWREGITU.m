VWREGITU	;Portland, OR/jeb et al World Vista Registration Utilities
	;2.0;;**LOCAL**;;2015;Build 4;;c2014
	;*****************************************************************
	;* Licensed under GNU 2.0 or greater - see license.txt file      *
	;* Program/application is for the management of input templates  *
	;* owned by the user (DUZ).				         *
	;* REMINDER: All template fields pertain only to the Patient File*
	;*  (#2)!							 *
	;*****************************************************************
	;No Fall thru - jeb
	Q
	;
DFNID()	;Set NAME(IEN),TAB,DOB(AGE),TAB,HRN,TAB,PHONE#
	N DFNID,NAME,X,Y,DOB,HRN,PHONE
	I 'DFN Q ""
	S NAME=$P(^DPT(DFN,0),"^")
	S Y=$$OUTPUT^VWTIME(DFN) X ^DD("DD") S DOB=Y
	S HRN=$G(^DPT(DFN,540001.1))
	S PHONE="Phone: "_$P(^DPT(DFN,.13),"^")
	S DFNID=NAME_$C(9)_DOB_$C(9)_$S($L(HRN):"HRN: "_HRN_$C(9),1:"")_PHONE
	Q DFNID
	;
HELP(XDIC,XFIELD)	;
	N N
	K FHELP
	S FHELP=$G(^DD(XDIC,XFIELD,3))
	G:'$L(FHELP) HELPX
	S FHELP=FHELP_$S($E($L(FHELP))=".":" ",1:". ")
	I XFIELD'=27.02,$D(^DD(XDIC,XFIELD,21)) S N=0 F  S N=$O(^DD(XDIC,XFIELD,21,N)) Q:'+N  S FHELP=FHELP_^(N,0)_" "
	S FHELP=$TR(FHELP,"'","`")
HELPX	  Q FHELP
	;
M(RESULT,XMF)	;
	; **********************************************
	; * XMF_____PARENT FIELD^DFN^TEMPLATE NAME(IEN)*
	; **********************************************
	;
	;W "  ;Intentional break
	K RESULT,AR,TEMPLATE
	N XMFD,SUBD,SUBD3,SUBD4,SUBD5,F2,F3,F4,F5,DFN,N,X,SUBF,XT,FHELP
	S TNUM=+$P(XMF,"(",2)  ;Template IEN, if any
	S DFN=+$P(XMF,"^",2)   ;Client IEN, if any
	S XMF=+XMF  ;Parent field
	I '+$P(^DD(2,XMF,0),"^",2) S RESULT(0)=-1  ;Not a parent, eh?!
	S XMFD=+$P(^(0),"^",2)
	S F=0 F  S F=$O(^DD(XMFD,F)) Q:'+F  S RESULT($$INR)=$P(^(F,0),"^")_"^"_XMFD_";"_F_"^^"_$$HELP(XMFD,F)_"^"_$P(^(0),"^",3) D:+$P(^(0),"^",2)
	. S SUBD=+$P(^(0),"^",2)
	. S F2=0 F  S F2=$O(^DD(SUBD,F2)) Q:'+F2  S RESULT($$INR)=$P(^(F2,0),"^")_"^"_SUBD_";"_F2_"^^"_$$HELP(SUBD,F2)_"^"_$P(^(0),"^",3) D:+$P(^DD(SUBD,F2,0),"^",2)
	.. S SUBD3=+$P(^(0),"^",2)
	.. S F3=0 F  S F3=$O(^DD(SUBD3,F3)) Q:'+F3  S RESULT($$INR)=$P(^(F3,0),"^")_"^"_SUBD3_";"_F3_"^^"_$$HELP(SUBD3,F3)_"^"_$P(^(0),"^",3) D:+$P(^DD(SUBD3,F3,0),"^",2)
	... S SUBD4=+$P(^DD(SUBD3,F3,0),"^",2)
	... S F4=0 F  S F4=$O(^DD(SUBD4,F4)) Q:'+F4  S RESULT($$INR)=$P(^(F4,0),"^")_"^"_SUBD4_";"_F4_"^^"_$$HELP(SUBD4,F4)_"^"_$P(^(0),"^",3) D:+$P(^DD(SUBD4,F2,0),"^",2)
	.... S SUBD5=+$P(^(0),"^",2)
	.... S F5=0 F  S F5=$O(^DD(SUBD5,F5)) Q:'+F5  S RESULT($$INR)=$P(^(F5,0),"^")_"^"_SUBD5_";"_F5_"^^"_$$HELP(SUBD5,F5)_"^"_$P(^(0),"^",3)
	;Clean up of parents IN multiple fields
	M AR=RESULT K RESULT N DD,F
	S N=0 F  S N=$O(AR(N)) Q:'+N  D
	. S DD=+$P($P(AR(N),"^",2),";")  ;Is this a sub DD ?
	. S F=+$P(AR(N),";",2)
	. I +$P(^DD(DD,F,0),"^",2) K AR(N)
	;Clean up fields not in template
	M TEMPLATE=^DIE(TNUM,"DR")
	S X=$Q(@"TEMPLATE") K @X  ;Remove top, non-multiple subscript
	S X="AR" F  S X=$Q(@X) Q:X=""  D
	. S SUBD=+$P($P(@X,"^",2),";")
	. S SUBF=+$P(@X,";",2)
	. F I=1:1:20 I $D(TEMPLATE(I,SUBD)) D
	.. Q:TEMPLATE(I,SUBD)[SUBF
	.. K @X
	S N=0 F  S N=$O(AR(N)) Q:'+N  S RESULT($$INR)=AR(N)
	K AR,TEMPLATE
	Q
	;
DISV(RESULT,DFN)	;Set the Disv GLOBAL
	K RESULT
	I '$L(DFN) S RESULT=-1 Q
	S ^DISV(DUZ,"^DPT(")=+$P(DFN,"(",2),RESULT=1
	Q
	;
SR(FNAME,FNUM,FVALUE,FHELP,FSETPNTR,FMISC)	;Set values into RESULT()
	;********************************************************
	;* FNAME________Field Name                              *
	;* FNUM_________Field Number                            *
	;* FVALUE_______Data from existing client/patient       *
	;* FHELP________Help text from field                    *
	;* FSETPNTR_____Set of codes or Pointer reference       *
	;* FMISC________Locally described designator (not used) *
	;********************************************************
	S RESULT($$INR)=FNAME_"^"_FNUM_"^"_FVALUE_"^"_FHELP_"^"_FSETPNTR_"^"_FMISC
	Q
	;	
INR()	;Specific incrementer for RESULT array
	Q $O(RESULT(" "),-1)+1
	;
ZPC(RESULT,ZIP)	;Get zip,county/area/region,state/province,preferred city
	K RESULT N STP,CNTP,COUNTY,XZIP
	S XZIP=ZIP S RESULT(0)="No return" Q:'$L(XZIP)
	D POSTAL^XIPUTIL(XZIP,.ZIPDATA)
	I $D(ZIPDATA("ERROR")) Q  ;Can't be found
	S COUNTY=$G(ZIPDATA("COUNTY"))
	S STP=$G(ZIPDATA("STATE POINTER"))
	I STP,$L(COUNTY) S CNTP=$O(^DIC(5,STP,1,"B",COUNTY,0))
	K RESULT(0)
	S RESULT($$INR)=ZIPDATA("STATE")_"("_STP_")"
	S RESULT($$INR)=ZIPDATA("COUNTY")_"("_CNTP_")"
	S RESULT($$INR)=ZIPDATA("CITY")
	S RESULT($$INR)=ZIPDATA("FIPS CODE")
	K ZIPDATA
	Q
	;
SPI(RESULT,DFN)	;Simple patient inquiry display
	S LINE="----------"
	S DIC="^DPT(",DA=DFN,DIQ(0)="E",DIQ="AR"
	S DR=".01:.05;.111:.115;.1171:.1173;.117;.363"
	D EN^DIQ1
	S CITY=$G(AR(2,DFN,.114,"E"))
	S STIEN="",STATE=$G(AR(2,DFN,.115,"E")) S:$L(STATE) STIEN=$O(^DIC(5,"B",STATE,0))
	S XAGE=$G(AR(2,DFN,.033,"E"))
	S XAGE=$S(+XAGE:XAGE_" y/o",1:"")
	;;GET HRN
HRN	S HRN="",N=$O(^AUPNPAT(DFN,41,0))
	S HRN=$S('N:HRN,1:$P($G(^AUPNPAT(DFN,41,N,0)),"^",2))
	S RESULT($$INR)=AR(2,DFN,.01,"E")_"  "_AR(2,DFN,.363,"E")_" HRN: "_HRN
	S RESULT($$INR)="DOB: "_AR(2,DFN,.03,"E")_"  ("_XAGE_" "_AR(2,DFN,.02,"E")_")"
	S RESULT($$INR)="ADDRESS"_LINE_LINE
	S RESULT($$INR)=$G(AR(2,DFN,.111,"E"))_" "_$G(AR(2,DFN,.112,"E"))
	S RESULT($$INR)=$G(AR(2,DFN,.114,"E"))_", "_$G(AR(2,DFN,.115,"E"))_"  "_$S($L($G(AR(2,DFN,.1172,"E"))):AR(2,DFN,.1172,"E"),1:$G(AR(2,DFN,.1112,"E")))
	S RESULT($$INR)="Walk-ins"_LINE_LINE
	S RESULT($$INR)="Appointments"_LINE_LINE
	S RESULT($$INR)="Admissions"_LINE_LINE
	Q
	;
CONTROL()	      ;Check for CONTROL status
	       N X S X=$O(^DIC(19,"B","VW REG IT CONTROL",0))
	       I 'X Q 0  ;Ain't no option there
	       Q $S($D(^VA(200,DUZ,203,"B",X)):1,1:0)
	       ;
MISC(RESULT,VWDD)	;Get simple value from VWDD ID
	;***************************************************
	;* VWDD___________________(sub)-Dictionary number  *
	;* Multiple delimiter_____;(Semicolon)             *
	;***************************************************
	;
	I '$L(VWDD) S RESULT(0)="No value to evaluate" Q
	K RESULT
	N XDD,XDDLOC,N,X
	G MISCSD:$G(^DD(VWDD,0,"UP"))
	S CALLER=$S($P(VWDD,"^",2)="INS":1,1:0)
	S VWDD=$P(VWDD,"^")
	F I=1:1:$L(VWDD,";") S XDD=+$P(VWDD,";",I) S RESULT($$INR)="["_$P(^DIC(XDD,0),"^")_"]" D MISC1
	I CALLER S RESULT($$INR)="[GUARANTOR]" D
	. S X=$P(^DD(2.312,16,0),"^",3)
	. F I=1:1:$L(X,";") S Y=$P(X,";",I),RESULT($$INR)=$P(Y,":",2)_"("_$P(Y,":")_")"
	Q
	;
MISC1	S XDDLOC=$G(^DIC(XDD,0,"GL")) D:$L(XDDLOC)
	. S N=0 F  S N=$O(@(XDDLOC_N_")")) Q:'+N  D
	.. I XDDLOC["779.004" S XCNAME=$P(@(XDDLOC_N_",0)"),"^")_" "_$P(^(0),"^",2)_" "_+$G(^("SDS"))_"("_N_")",RESULT($$INR)=XCNAME Q
	.. S X=$P(@(XDDLOC_N_",0)"),"^")_"("_N_")",RESULT($$INR)=X
MX	Q
	;
MISCSD	;Sub-dictionary
	W ^("UP")
	Q
	;
	

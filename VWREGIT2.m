VWREGIT2	;Portland/WorldVista//Jim Bell, et al... - Post-Install for VWREG	
	;;2.0;WORLD VISTA;**HOME **;;Build 4
	;*****************************************************************
	;* Licensed under GNU 2.0 or greater - see license.txt file      *
	;* Program/application is for the management of input templates  *
	;* owned by the user (DUZ).				         *
	;* REMINDER: All template fields pertain only to the Patient File*
	;*  (#2)!							 *
	;*****************************************************************
	;
	;Continued from VWREGIT
	;
	;GNU License: See WVLIC.txt
	;Modified FOIA VISTA,
	;Copyright 2013 WorldVistA.  Licensed under the terms of the GNU
	Q
PRE	;Did this installation happen already? Avoid a re-do?
	I $O(^XMB(3.8,"B","VW REG ERROR REPORT",0))&($D(^XTV(8989.51,"B","VW GUI REG TEMPLATE DIRECTORY",0))) W !?5,"Installation has already occurred"
	W !,"Do you want to continue? NO//" R X:60 S:'$L(X) X="NO" S X=$$UP^XLFSTR(X)
	I "NON"[X W !,"OK" D ^XUSCLEAN
	Q
	;
PI	;Post Installation install
	;Checking for a home directory & file
	;I $O(^XMB(3.8,"B","VW REG ERROR REPORT",0)) W !?5,"Installation has already occurred" Q
	D PRE
	S AR=1,AR(1)="[TEMPLATES]"
	S N=0 F  S N=$O(^DIE(N)) Q:'+N  S X=$P(^(N,0),"^") D
	. I X["VW " S AR($I(AR))=X_"("_N_")"
	. I X["[World" S AR($I(AR))=X_"("_N_")"
	S AR($I(AR))="[ID]"
	S P4=1,P5="",HD=$ZDIRECTORY_"regparam/",FILE="regit.txt"
	S X=$$GTF^%ZISH($NA(AR(1)),1,HD,"regit.txt")
	ZSY "chmod 777 "_$ZDIRECTORY_"regparam/"_FILE  ;No sensitive info here
	Q:$G(TEST)  ;Straightening out regit.txt
	;
	;; NOTE: The parameter definition is installed but there is no installation for
	;;the actual parameter and value. Do it here.
	;parameter value attempt
	;Set a home directory for editing; SYSTEM (DIC(4,) and DOMAIN (DIC(4.2,) only:"/home/vista/regparam/"
	S PARD=$O(^XTV(8989.51,"B","VW GUI REG TEMPLATE DIRECTORY",0))
	I PARD D
	. L +^XTV(8989.5,0):1 D  L -^XTV(8989.5,0)
	.. S NEW=$O(^XTV(8989.5," "),-1)+1
	.. S $P(^XTV(8989.5,0),"^",3)=NEW
	.. S $P(^XTV(8989.5,0),"^",4)=$P(^(0),"^",4)+1
	.. S $P(^XTV(8989.5,NEW,0),"^")="1;DIC(4,"
	.. S $P(^XTV(8989.5,NEW,0),"^",2)=PARD
	.. S $P(^XTV(8989.5,NEW,0),"^",3)=1
	.. S ^XTV(8989.5,NEW,1)=HD
	.. S DA=NEW,DIK="^XTV(8989.5," D IX^DIK
	.. S NEW2=$O(^XTV(8989.5," "),-1)+1
	.. S $P(^XTV(8989.5,0),"^",3)=NEW2
	.. S $P(^XTV(8989.5,0),"^",4)=$P(^(0),"^",4)+1
	.. S $P(^XTV(8989.5,NEW2,0),"^")="9;DIC(4.2,"
	.. S $P(^XTV(8989.5,NEW2,0),"^",2)=PARD
	.. S $P(^XTV(8989.5,NEW2,0),"^",3)=1
	.. S ^XTV(8989.5,NEW2,1)=HD
	.. S DA=NEW2,DIK="^XTV(8989.5," D IX^DIK
	;
	;Mailgroup VW REG ERROR REPORT - add programmer's email
	S DA(1)=$O(^XMB(3.8,"B","VW REG ERROR REPORT",0))
	Q:'DA(1)
	S DIC="^XMB(3.8,"_DA(1)_",6,"
	S X="jbellco65@gmail.com"
	S DIC(0)="LZ"
	D FILE^DICN
	Q
	;

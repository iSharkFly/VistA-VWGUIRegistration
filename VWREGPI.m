VWREGPI	; VEN/SMH - VW MU REG 2.0 Post-install ; 11/5/12 12:51pm
	;;2.0;VW MU REG;;Nov 05, 2012;Build 18
	;*****************************************************************
	;* Licensed under GNU 2.0 or greater - see license.txt file      *
	;* Program/application is for the management of input templates  *
	;* owned by the user (DUZ).				         *
	;* REMINDER: All template fields pertain only to the Patient File*
	;*  (#2)!							 *
	;*****************************************************************
	; Enter VW Local Registration Template into Site Parameters
	; PEPs: POST
	;
POST	; Post install hook
	N DIE,DA,DR
	S DIE="^DG(43,",DA=1,DR="70///VW LOCAL REGISTRATION TEMPLATE"
	D ^DIE
	QUIT

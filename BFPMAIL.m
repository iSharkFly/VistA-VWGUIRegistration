BJMAIL	; PTLD/JEB	; Generic mail message	; January 1990
	;;3.0;WORLD VISTA/JEB;** **;;Build: 3
	;; General Public License: See WVLCI.txt 
	;User program sets variables calls MSG^BJMAIL (See variable List)
	; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	; | VARIABLE LIST:                                                           | 
	; |  SET 'GROUP' TO THE MAIL GROUP NAME OR NUMBER                            |
	; |  or                                                                      |
	; |  K GROUP AND SET UP A 'TO()' ARRAY WITH THE DUZ AS THE SUBSCRIPT OF      |
	; |   THE 'TO' ARRAY IF YOU WANT TO SEND A MSG TO AN INDIVIDUAL OR GROUP     |
	; |   OF INDIVIDUALS WHO ARE NOT IN AN ESTABLISHED MAIL GROUP                |
	; |  or                                                                      |
	; |  SET 'GROUP' AND INDIVIDUAL 'T()' ARRAY NODES TO SEND TO THE             |
	; |   ESTABLISHED GROUP AND INDIVIDUALS NOT IN THE GROUP                     |
	; |  SET 'TITLE'=TO THE TITLE YOU WANT DISPLAYED                             |
	; |  SET 'FROM' AS THE PERSON WHO IS SENDING THE MESSAGE ( A NULL 'FROM'     |
	; |   WILL USE THE POSTMASTER).                                              |
	; |  SET THE TEXT OF THE MESSAGE IN ARRAY USING 'T' AND SUBSCRIPT OF A NUMBER|
	; |   I.,E                                                                   |
	; |    T(1)="This is the first line of the message"                          |
	; |    T(2)="This is the second line of the message, etc.                    |
	; |  SET 'CONFIRM'=1 FOR A CONFIRMATION                                      |
	; |  SET 'INFO'=1 FOR INFORMATION ONLYH (NO REPLIES ALLOWED) MESSAGE         |
	; |  SET 'PRIO'=1 FOR PRIORITY DELIVERY                                      |
MSG	; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	N XMINSTR
	D GROUP
	Q:$O(TO(""))=""
	;
	I $G(CONFIRM) S XMINSTR("FLAGS")=$G(XMINSTR("FLAGS"))_"R"
	I $G(PRIO) S XMINSTR("FLAGS")=$G(XMINSTR("FLAGS"))_"P"
	I $G(INFO) S XMINSTR("FLAGS")=$G(XMINSTR("FLAGS"))_"I"
	I '$D(FROM) S FROM=.5
	D SETFROM^XMD(.FROM,.XMINSTR)
	I '$D(TITLE) S TITLE="Title not specified by sender"
	I $L(TITLE)>65 S TITLE=$E(TITLE,1,65)
	I $L(TITLE)<3 S TITLE=TITLE_"..."
	D SENDMSG^XMXAPI(FROM,TITLE,"T",.TO,.XMINSTR)
	K CONFIRM,FROM,GROUP,INFO,PRIO,T,TITLE,TO
	Q
GROUP	;
	Q:$G(GROUP)=""
	I +GROUP=GROUP S GROUP=$P($G(^XMB(3.8,GROUP,0)),"^")
	S TO("G."_GROUP)=""
	Q
	;

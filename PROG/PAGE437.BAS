1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 '\hamcalc\prog\PAGE437 - ASCII Code Page 437 - 27 JAN 2001 rev. 09 NOV 2006
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 
50 COLOR 7,0,0
60 UL$=STRING$(80,205)
70 U$=STRING$(79,205)
80 '
90 '.....title page
100 CLS
110 COLOR 15,2
120 PRINT " ASCII Character Code Page 437";TAB(57);"by George Murphy VE3ERP ";
130 COLOR 7,0:PRINT UL$;
140 PRINT " Press <1> to START or <0> to EXIT"
150 Z$=INKEY$:IF Z$=""THEN 150
160 COLOR 7,0,0:CLS
170 IF Z$="0"THEN RUN EX$
180 IF Z$="1"THEN 190
190 PRINT " ASCII Character Code Page 437 (Codes 1 to 31 are control codes)"
200 PRINT U$
210 Z=31
220 PRINT " ";:FOR N=1 TO 13:Z=Z+1
230 IF Z=255 THEN 270
240 Z$=CHR$(Z):PRINT USING "### ";Z;:PRINT Z$;"�";
250 NEXT N:PRINT ""
260 GOTO 220
270 COLOR 7,0:PRINT U$;
280 PRINT
290 GOSUB 470
300 '......graphic codes
310 CLS:KEY OFF
320 PRINT TAB(29);"ASCII Graphic Symbols"
330 PRINT "��vert�horiz�vert���vert�horiz�vert���vert�horiz�vert���vert�horiz�vertĿ"
340 PRINT "��179  �196  �179 ��186  �205  �186 ��186  �196  � 186��179  �205  �179 �"
350 PRINT "�����������������������������������������������������������������������͵"
360 PRINT "�� 218�� 194�� 191�� 201�� 203�� 187�� 214�� 210�� 183�� 213�� 209�� 184�"
370 PRINT "�����������������������������������������������������������������������Ĵ"
380 PRINT "�� 195�� 197�� 180�� 204�� 206�� 185�� 199�� 215�� 182�� 198�� 216��181 �"
390 PRINT "�����������������������������������������������������������������������Ĵ"
400 PRINT "�� 192�� 193�� 217�� 200�� 202�� 188�� 211�� 208�� 189�� 212�� 207�� 190�"
410 PRINT "�������������������������������������������������������������������������"
420 PRINT
430 PRINT " Other codes used frequently:"
440 PRINT "� � 129 � � 130 � � 138 � � 155 � � 156 � � 159 � � 171 � � 172 � � 227 � � 228 � � 230 � � 232 � � 234 � � 246 � � 251 � � 253 �";
450 '.....end
460 GOSUB 470:GOTO 90
470 '.....PRT
480 KEY OFF:GOSUB 550:LOCATE 25,5:COLOR 0,2
490 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
500 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
510 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 510 ELSE GOSUB 550
520 IF Z$="3"THEN RETURN
530 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
540 IF Z$="2"THEN LPRINT CHR$(12) ELSE 480
550 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

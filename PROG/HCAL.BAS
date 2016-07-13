1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,V$,D$,Z$,X$
10 '\hamcalc\menu\HCAL - HAMCALC Main menu - 03 JUN 94 - rev. 25 JAN 2011
20 '.....see STRAYS, Oct.1993 QST, page 23
30 CLS:KEY OFF
40 KEY 10,"SYSTEM"+CHR$(13)
60 V$="129,"          'version
70 D$="25 JAN 2011"   'date of latest version
80 IF EX$="\hamcalc64\prog\hcal"THEN 110
90 IF EX$<>""THEN 110
95 EX$="\hamcalc64\prog\hcal"
96 CHAIN"\hamcalc64\prog\intro"
100 '
110 COLOR  15,0,0
120 U$=STRING$(80,205)
130 DIM A$(21),B$(21),C$(21)
140 ON ERROR GOTO 350
150 COLOR 14,1
160 PRINT "    H  A  M  C  A  L  C    ";
170 COLOR 11,1
180 PRINT " Version ";V$;"   ";D$;" ";
190 COLOR 14,1
200 PRINT TAB(56);" by George Murphy VE3ERP ";
210 COLOR 7,1:PRINT STRING$(80,223);
220 COLOR 14,1
230 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3   'set screen background color
240 COLOR 14,1
250 LOCATE ,24:PRINT " PAINLESS MATH for RADIO AMATEURS "
260 COLOR 14,4
270 LOCATE ,31:PRINT " M A I N    M E N U "
280 COLOR 15,1:PRINT STRING$(80,205);
290 COLOR 15,1:PRINT TAB(10);" Turn ";
300 COLOR 14,4:PRINT " Caps Lock OFF ";
310 COLOR 15,1:PRINT " then press any letter in ( ) to select:"
320 PRINT U$;
330 N=0:GOTO 370
340 '
350 PRINT "Error ";ERR;" in line ";ERL;"B$(Z)";B$(Z):END
360 '
370 NM=15 'number of menu items
380 FOR N=1 TO NM:A$(N)=CHR$(N+95):IF N=1 THEN A$(N)="z"
390  IF N=1 OR N=11 THEN COLOR 14,4 ELSE COLOR 15,1
400  READ B$(N),C$(N)
410  LOCATE CSRLIN,24:PRINT "(";A$(N);")  ";C$(N);" "
420 NEXT N
430 COLOR 15,1
440 COLOR 7:LOCATE 24:PRINT STRING$(80,220);
450 COLOR 14,1:LOCATE 25:PRINT "   ";CHR$(34);
460 PRINT "Aversion to mathematics is not an acquired distaste - ";
470 PRINT "it comes naturally";CHR$(34);"   ";
480 COLOR 7,0
490 Z$=INKEY$:IF Z$=""THEN 490
500 FOR Z=1 TO NM
510 IF ASC(Z$)>=65 AND ASC(Z$)<=80 THEN 600
520 IF Z$=A$(Z)THEN 560
530 NEXT Z
540 GOTO 490
550 '
560 IF B$(Z)=""THEN 490
570 CLS:'CHDIR"\hamcalc64\prog"
580 COLOR 7,0,1:CHAIN B$(Z)
590 '
600 '.....caps lock warning
610 CLS:BEEP:COLOR 14,4
620 PRINT
630 PRINT " Please turn your CAPS LOCK off and make all entries in lower case ";
640 PRINT " characters only."
650 PRINT
660 PRINT " The computer will provide upper case characters where appropriate."
670 PRINT
680 PRINT " Turn CAPS LOCK off NOW then press any key to continue............."
690 IF INKEY$=""THEN 690
700 CLS:CLEAR:COLOR 7,0,0:GOTO 10
710 '
720 DATA \hamcalc64\prog\quiktabl,QuikTables
730 DATA \hamcalc64\prog\hcal-a,Program Menu A  ( A.C.- Cap )
740 DATA \hamcalc64\prog\hcal-b,Program Menu B  ( Cap - Cos )
750 DATA \hamcalc64\prog\hcal-c,Program Menu C  ( Cro - Guy )
760 DATA \hamcalc64\prog\hcal-d,Program Menu D  ( Guy - Log )
770 DATA \hamcalc64\prog\hcal-e,Program Menu E  ( Log - Op  )
780 DATA \hamcalc64\prog\hcal-f,Program Menu F  ( Op  - S-  )
790 DATA \hamcalc64\prog\hcal-g,Program Menu G  ( Sag - Tor )
800 DATA \hamcalc64\prog\hcal-h,Program Menu H  ( Tra - Whe )
810 DATA \hamcalc64\prog\hcal-i,Program Menu I  ( Win - Zep )
820 DATA \hamcalc64\prog\hamdex,INDEX
830 DATA \hamcalc64\prog\helphint,READ ME
840 DATA \hamcalc64\prog\logoclok,Clock Screen Saver
850 DATA \hamcalc64\prog\newsince,Recent program additions & upgrades
860 'DATA \hamcalc\prog\printip,Printer Tips
870 DATA \hamcalc64\prog\zexit,EXIT
880 END

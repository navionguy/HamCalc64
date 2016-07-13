1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'FOLDIPOL - Folded Dipole - 300ê Twin-Lead - 23 APR 96 rev.25 AUG 07
20 REM - Ref: The ARRL ANTENNA HANDBOOK, 14th edition, page 13-9
30 IF EX$=""THEN EX$="EXIT"
40 CLS:KEY OFF
50 COLOR 7,0,1
60 UL$=STRING$(80,205)
70 U$="####.###"
80 V=8.199999E-01   'velocity factor of 300ê Twin-Lead
90 PI=3.141592
100 DIM C(73)
110 FOR Z=0 TO 73
120 READ C(Z)    'mica capacitors
130 NEXT Z
140 '
150 '.....start
160 CLS
170 COLOR 15,2
180 PRINT " FOLDED DIPOLE - 300 ohm Twin-Lead";
190 PRINT TAB(57);;"by George Murphy VE3ERP ";
200 COLOR 1,0:PRINT STRING$(80,223);
210 COLOR 7,0
220 LOCATE 3:X=7 :GOSUB 930
230 LOCATE 3:X=44:GOSUB 760
240 PRINT UL$;
250 '
260 '.....standard antenna note
270 'OPEN"I",1,"\hamcalc\docfiles\antenna.doc"
280 'IF EOF(1)THEN 300
290 'INPUT #1,NOTE$:PRINT "   ";NOTE$:GOTO 280
300 'CLOSE
310 PRINT
320 COLOR 0,7:LOCATE CSRLIN,24:PRINT " Press 1 to continue or 0 to EXIT "
330 COLOR 7,0
340 Z$=INKEY$:IF Z$="" THEN 340
350 IF Z$="0"THEN CLS:RUN EX$
360 IF Z$="1"THEN 390
370 GOTO 340
380 '
390 '.....data input
400 VIEW PRINT 16 TO 24:CLS:VIEW PRINT:LOCATE 16
410 INPUT " ENTER: Frequency of operation..................(MHz)";F
420 LOCATE CSRLIN-1:PRINT "       ";
430 LOCATE CSRLIN,49:PRINT "...Ÿ=";USING U$;F;
440 PRINT " Mhz (";USING "###";300/F;:PRINT "m.)"
450 '
460 PRINT "        Velocity factor of 300ê twin-lead..........V=";USING U$;V
470 '
480 A=468/F   'length in feet
490 PRINT "        Length of 300ê folded dipole...............A=";
500 PRINT USING U$;A*3.048000E-01;:PRINT " m.=";USING U$;A;:PRINT " ft."
510 '
520 B=V*A
530 PRINT "        Shorting connections (A x V)...............B=";
540 PRINT USING U$;B*3.048000E-01;:PRINT " m.=";USING U$;B;:PRINT " ft."
550 '
560 C=A/9.578000
570 PRINT "        Length of matching section.................C=";
580 PRINT USING U$;C*3.048000E-01;:PRINT " m.=";USING U$;C;:PRINT " ft."
590 PRINT UL$;
600 '
610 L=A/3.340699
620 PRINT "        300ê twin-lead stub length................Ls=";
630 PRINT USING U$;L*3.048000E-01;:PRINT " m.=";USING U$;L;:PRINT " ft."
640 '
650 PRINT "                        - O R -"
660 C1=A/4.327999E-01
670 FOR Z=1 TO 73
680  MID=(C(Z)+C(Z-1))/2
690  IF C1>=C(Z-1)AND C1<MID THEN D=C(Z-1):GOTO 720
700  IF C1>=MID AND C1<C(Z) THEN D=C(Z):GOTO 720
710 NEXT Z
720 PRINT "        Capacitor reactive element................C1=";
730 PRINT USING U$;C1;:PRINT " pF (use";D;"pF)";
740 '
750 GOTO 1100
760 '.....diagram 1
770 COLOR 0,7
780 LOCATE ,X:PRINT "        CAPACITOR MATCH        "
790 LOCATE ,X:PRINT "  ³®ÄÄÄÄÄÄÄÄÄÄ A ÄÄÄÄÄÄÄÄÄÄ¯³  "
800 LOCATE ,X:PRINT "  ³   ³®ÄÄÄÄÄÄ B ÄÄÄÄÄÄ¯³   ³  "
810 LOCATE ,X:PRINT "-ìÂÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÂì-"
820 LOCATE ,X:PRINT "  ÀÄÄÄÁÄÄÄÄÄÄÂÄìÄÂÄÄÄÄÄÄÁÄÄÄÙ  "
830 LOCATE ,X:PRINT "   300 ê any ³   ³ shortÙ      "
840 LOCATE ,X:PRINT "   lengthÄÄÄ>³   ³             "
850 LOCATE ,X:PRINT "          ÚÄ¯ÃÄ×Ä´C1 silver    "
860 LOCATE ,X:PRINT "          C  ³   ³   mica      "
870 LOCATE ,X:PRINT "          ÀÄ¯ÀÄ¿ ³             "
880 LOCATE ,X:PRINT "               ÇÄÙ             "
890 LOCATE ,X:PRINT "               º®ÄÄ 50 ê coax  "
900 COLOR 7,0
910 RETURN
920 '
930 '.....diagram 2
940 COLOR 0,7
950 LOCATE ,X:PRINT "           STUB MATCH          "
960 LOCATE ,X:PRINT "  ³®ÄÄÄÄÄÄÄÄÄÄ A ÄÄÄÄÄÄÄÄÄÄ¯³  "
970 LOCATE ,X:PRINT "  ³   ³®ÄÄÄÄÄÄ B ÄÄÄÄÄÄ¯³   ³  "
980 LOCATE ,X:PRINT "-ìÂÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÂì-"
990 LOCATE ,X:PRINT "  ÀÄÄÄÁÄÄÄÄÄÄÂÄìÄÂÄÄÄÄÄÄÁÄÄÄÙ  "
1000 LOCATE ,X:PRINT "   300 ê any ³   ³ shortÙ      "
1010 LOCATE ,X:PRINT "   lengthÄÄÄ>³   ³             "
1020 LOCATE ,X:PRINT "          ÚÄ¯Ã¿ Ú´             "
1030 LOCATE ,X:PRINT "          C  ³³ ³<Ä open stub  "
1040 LOCATE ,X:PRINT "          ÀÄ¯ÀÄ¿ ³  Ls long    "
1050 LOCATE ,X:PRINT "               ÇÄÙ             "
1060 LOCATE ,X:PRINT "               º®ÄÄ 50 ê coax  "
1070 COLOR 7,0
1080 RETURN
1090 '
1100 '.....end
1110 GOSUB 1220
1120 GOTO 150
1130 END
1140 '
1150 '.....silver mica capacitors (pF)
1160 DATA 0,1,2,3,5,7,10,12,15,18,20,22,24,27,30,33,36,39,43,47,50,51,56,62,68
1170 DATA 75,82,91,100,110,120,150,160,180,200,220,240,250,270,330,360,390
1180 DATA 430,470,500,510,560,620,680,750,820,910,1000,1100,1200,1500,1600,1800
1190 DATA 2000,2200,2400,2700,3000,3300,3900,4300,4700,5000,5100,5600,6200
1200 DATA 6800,7500,8200
1210 '
1220 'PRT
1230 KEY OFF:GOSUB 1300:LOCATE 25,5:COLOR 0,2
1240 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1250 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1260 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1260 ELSE GOSUB 1300
1270 IF Z$="3"THEN RETURN
1280 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1290 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1230
1300 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

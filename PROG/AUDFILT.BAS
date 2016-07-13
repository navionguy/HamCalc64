1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'AUDFILT - Op Amp Audio filter - 13 JAN 94 rev. 08 APR 97
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,0
50 UL$=STRING$(80,205)
60 U1$="###,###"
70 E$=STRING$(80,32)
80 PI=3.141592
90 DIM C(19)
100 DIM R(14)
110 DATA 470,500,510,560,600,620,680,750,820,910
120 DATA 1000,1100,1200,1300,1500,1600,1800,2000,2200
130 FOR Z=1 TO 19:READ C(Z):NEXT Z       'standard capacitors
140 DATA 10,12,15,18,20,22,27,33,39,47,56,68,82,100
150 FOR Z=1 TO 14:READ R(Z):NEXT Z      'standard resistors
160 '
170 '.....start
180 CLS
190 COLOR 15,2
200 PRINT " OP AMP AUDIO FILTER";TAB(57);"by George Murphy VE3ERP ";
210 COLOR 1,7:PRINT STRING$(80,223);
220 COLOR 7,0
230 '
240 '.....draw diagram
250 COLOR 0,7
260 PRINT "   U1 = #741 Op Amp                                  "
270 PRINT "                         ÚÄÄÄÄC1ÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿  "
280 PRINT "                         ³          R3            ³  "
290 PRINT "                         ³          ³  ÚÄÄÄÄ¿ U1  ³  "
300 PRINT "                         ³          ³  ´1  8Ã     ³                "
310 PRINT "        AF in ¯ÄÄÄR1ÄÄÄÄÂÁÄÄÄÄC2ÄÄÄÄÁÄÄ´2  7ÃÄÄ¿  ³ 2æF/15v        "
320 PRINT "                        ³         ÚÄÄÄÄ´3  6ÃÄÄ³ÄÄÁÄÄC3ÄÄÄ¯ AF out "
330 PRINT "                        ³         ³   Ú´4  5Ã  ³    +  -           "
340 PRINT "                        ³         ³   ³ÀÄÄÄÄÙ  ³                   "
350 PRINT "                        R2        ³ \\Á\\      ³                   "
360 PRINT "                        ³         ÃÄÄÄÄÄÄR5ÄÄÄÄÁÄÄÄÄÄÄÄÄ + ®¿      "
370 PRINT "                        ³         ³                         Ã 12v  "
380 PRINT "                        ³         R4              ÚÄÄÄÄÄ - ®Ù      "
390 PRINT "                        ³         ³               ³                "
400 PRINT "                      \\Á\\     \\Á\\           \\Á\\              "
410 PRINT "                                                                   "
420 COLOR 7,0
430 LOCATE 3,55:PRINT "Single pole filter shown."
440 LOCATE  ,55:PRINT "Identical poles may be"
450 LOCATE  ,55:PRINT "cascaded to form a multi-"
460 LOCATE  ,55:PRINT "section filter."
470 FOR Z=7 TO 18
480 LOCATE Z,68:PRINT STRING$(13,32);
490 NEXT Z
500 '
510 COLOR 15,1:LOCATE 19,22
520 PRINT " Press 1 to continue or 0 to EXIT....."
530 COLOR 7,0
540 Z$=INKEY$:IF Z$=""THEN 540
550 IF Z$="0"THEN CLS:RUN EX$
560 IF Z$="1"THEN 580
570 GOTO 540
580 LOCATE CSRLIN-1:PRINT STRING$(80,32);:LOCATE CSRLIN-1
590 '
600 '.....input data
610 FOR Z=1 TO 10:PRINT USING U1$;C(Z);:NEXT Z:PRINT ""
620 FOR Z=11 TO 19:PRINT USING U1$;C(Z);:NEXT Z:PRINT "   (polystyrene)"
630 PRINT UL$;
640 LOCATE 22
650 PRINT " C1 and C2 can be any value polystyrene capacitor ";
660 PRINT "from 470 pF to 2200 pF."
670 INPUT " ENTER: Value (pF) from above list for C1 and C2 ";C
680 VIEW PRINT 19 TO 24:CLS:VIEW PRINT
690 COLOR 0,7
700 LOCATE 5,27:PRINT C;"pF"
710 LOCATE 9,27:PRINT C;"pF"
720 COLOR 7,0
730 LOCATE 19
740 PRINT " Q should not exceed 5 for multi-section filters."
750 INPUT " ENTER: Desired Q:";Q
760 VIEW PRINT 19 TO 24:CLS:VIEW PRINT
770 COLOR 0,7
780 LOCATE 4,2:PRINT "   Q =";Q
790 COLOR 7,0
800 LOCATE 19
810 PRINT " Gain should not exceed 2 for multi-section filters."
820 INPUT " ENTER: Desired Gain:";G
830 COLOR 0,7
840 LOCATE 5,2:PRINT "Gain =";G
850 COLOR 7,0
860 VIEW PRINT 19 TO 24:CLS:VIEW PRINT
870 LOCATE 19:INPUT " Desired audio bandpass frequency (Hz):";F
880 LOCATE 1:PRINT E$;
890 F$=STR$(F)+" Hz AUDIO BANDPASS ACTIVE FILTER"
900 LOCATE 1,(80-LEN(F$))/2:PRINT F$
910 VIEW PRINT 19 TO 24:CLS:VIEW PRINT
920 '
930 COLOR 0,7
940 '.....calculate R1
950 W=2*PI*F
960 R=Q*10^6/(G*W*C*10^-6)
970 GOSUB 1370
980 R1$=R$
990 LOCATE 9,17:PRINT R1$
1000 '
1010 '.....calculate R2
1020 R=Q*10^6/((2*Q^2-G)*W*C*10^-6)
1030 GOSUB 1370
1040 R2$=R$
1050 LOCATE 13,23:PRINT R2$
1060 '
1070 '.....calculate R3
1080 R=2*Q*10^6/(W*C*10^-6)
1090 GOSUB 1370
1100 R3$=R$
1110 LOCATE 5,39:PRINT R3$
1120 '
1130 '.....calculate R4,R5
1140 R3=X 'from R3 calculation
1150 R=1.999999E-02*R3
1160 GOSUB 1370
1170 R4$=R$:R5$=R$
1180 LOCATE 14,40:PRINT R5$
1190 LOCATE 15,37:PRINT R4$
1200 COLOR 7,0
1210 '
1220 '.....notes
1230 LOCATE 19
1240 PRINT "1. Capacitors C1 & C2 should be polystyrene. DO NOT use disc ";
1250 PRINT "ceramic capacitors.";
1260 PRINT "2. Q should not exceed 5 and gain should not exceed 2 for ";
1270 PRINT "multisection filters";
1280 PRINT "     to minimize audio instability and ";CHR$(34);"ringing";
1290 PRINT CHR$(34);"."
1300 PRINT "3. Resistors shown are standard values close to the optimum design";
1310 PRINT " values. This";
1320 PRINT "     may cause slight alterations in the selected gain and audio ";
1330 PRINT "frequency.";
1340 GOSUB 1510
1350 CLS:GOTO 170
1360 '
1370 '.....select standard resistor
1380 M=1.000000E-01             'multiplier base
1390 FOR Z=1 TO 14
1400 X=R(Z)*M
1410 IF X>R THEN 1450
1420 NEXT Z
1430 M=M*10
1440 GOTO 1390
1450 Y=X:Y$=" ê"
1460 IF X>=1000 THEN Y=X/1000:Y$=" kê"
1470 IF X>=10^6 THEN Y=X/10^6:Y$=" Mê":GOTO 1480
1480 R$=STR$(Y)+Y$
1490 RETURN
1500 '
1510 'PRT
1520 KEY OFF:GOSUB 1590:LOCATE 25,5:COLOR 0,2
1530 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1540 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1550 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1550 ELSE GOSUB 1590
1560 IF Z$="3"THEN RETURN
1570 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1580 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1520
1590 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

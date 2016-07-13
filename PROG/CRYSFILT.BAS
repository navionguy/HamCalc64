1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'CRYSFILT - Crystal Ladder Filters - 01 FEB 96 rev.08 APR 97
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,0
50 U1$="#####.###"
60 X$=STRING$(80,32)
70 DIM BK(5,5),CK(5,5)
80 '
90 DATA 1.414, .7071, 0, 0, 0
100 DATA 1, .7071, .7071, 0, 0
110 DATA .7654, .8409, .4512, .8409, 0
120 DATA .6180, 1, .5559, .5559, 1
130 '
140 DATA 1.6382, .7106, 0, 0, 0
150 DATA 1.4328, .6618, .6618, 0, 0
160 DATA 1.3451, .685, .5421, .685, 0
170 DATA 1.3013, .7028, .5355, .5355, .7028
180 '
190 FOR Z=2 TO 5:FOR Y=1TO 5:READ BK(Z,Y):NEXT Y:NEXT Z
200 FOR Z=2 TO 5:FOR Y=1 TO 5:READ CK(Z,Y):NEXT Y:NEXT Z
210 GOTO 290
220 '
230 '.....format input line
240 IF ZZ=N THEN U$="#####"ELSE U$=U1$
250 LOCATE CSRLIN-1:PRINT SPC(7);
260 LOCATE CSRLIN,47:PRINT STRING$(7,".");USING U$;ZZ;
270 RETURN
280 '
290 '.....start
300 CLS
310 COLOR 15,2
320 PRINT " CRYSTAL LADDER FILTER";TAB(57);"by George Murphy VE3ERP ";
330 COLOR 1,0:PRINT STRING$(80,223);:COLOR 7,0
340 GOSUB 1020    'diagram
350 PRINT
360 GOSUB 1150   'preface
370 PRINT
380 COLOR 0,7:LOCATE ,21:PRINT " Press 1 to continue or 0 to EXIT..."
390 COLOR 7,0
400 Z$=INKEY$:IF Z$=""THEN 400
410 IF Z$="0"THEN CLS:RUN EX$
420 IF Z$="1"THEN 440
430 GOTO 400
440 VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
450 '
460 '.....inputs
470 INPUT " ENTER: Matched crystal BANDWIDTH................(Hz)";F
480 ZZ=F:GOSUB 230:PRINT " Hz"
490 '
500 INPUT " ENTER: Matched crystal CENTRE FREQUENCY........(MHz)";FO
510 ZZ=FO:GOSUB 230:PRINT " MHz"
520 '
530 INPUT " ENTER: Matched crystal SERIES-LOSS RESISTANCE....(Й)";FS
540 ZZ=FS:GOSUB 230:PRINT " ohms"
550 '
560 COLOR 0,7:LOCATE ,8
570 PRINT " Choose filter: Press b for Butterworth or c for Chebyshev."
580 COLOR 7,0
590 F$=INKEY$:IF F$=""THEN 590
600 IF F$="b"THEN TYPE$="BUTTERWORTH":GOTO 630
610 IF F$="c"THEN TYPE$="CHEBYSHEV":GOTO 630
620 GOTO 590
630 LOCATE CSRLIN-1:PRINT X$;:LOCATE CSRLIN-1,2
640 COLOR 0,7:PRINT " ";TYPE$;;" FILTER:";:COLOR 7,0
650 '
660 INPUT " ENTER: Number of poles (maximum 5)..................";N
670 IF N>5 THEN LOCATE CSRLIN-1:PRINT X$;:LOCATE CSRLIN-1:GOTO 660
680 ZZ=N:GOSUB 230:PRINT ""
690 '
700 INPUT " ENTER: Desired filter bandwidth.................(Hz)";B
710 ZZ=B:GOSUB 230:PRINT " Hz"
720 '
730 CN=1    'capacitor number
740 FOR Z=2 TO N
750 IF F$="b"THEN KJK=BK(N,Z)
760 IF F$="c"THEN KJK=CK(N,Z)
770 CJK=1326*(F/(B*KJK*FO))-10
780 CN=CN+11:CN$="C"+RIGHT$(STR$(CN),2)
790 PRINT "        Capacitor ";CN$;;"................................";
800 PRINT USING U1$;CJK;:PRINT ;" pF"
810 NEXT Z
820 '
830 IF F$="b"THEN Q=BK(N,1)
840 IF F$="c"THEN Q=CK(N,1)
850 RE=(120*B/(Q*294))-23
860 PRINT "        Minimum End Resistance required..............";USING U1$;RE;
870 PRINT " ohms"
880 '
890 PRINT " ENTER: End Termination Ro (minimum";USING "####.#";RE+1.000000E-01;
900 PRINT " Й)...(ohms)";:INPUT RO
910 ZZ=RO:GOSUB 230:PRINT " ohms"
920 '
930 CEND=(1.590000*10^5/(RO*FO))*SQR(RO/RE-1)-5
940 PRINT "        End Capacitors (Ce)..........................";
950 PRINT USING U1$;CEND;:PRINT ;" pF"
960 '
970 '.....end
980 GOSUB 1330
990 GOTO 290
1000 END
1010 '
1020 '.....diagram
1030 T=9
1040 COLOR 0,7
1050 LOCATE ,T:PRINT "         здддддддддд matched crystals ддддддддд©         "
1060 LOCATE ,T:PRINT "           зд©     зд©     зд©     зд©     зд©           "
1070 LOCATE ,T:PRINT "  здддддбд╢Ё1Ёцдбд╢Ё2Ёцдбд╢Ё3Ёцдбд╢Ё4Ёцдбд╢Ё5Ёцдбддддд©  "
1080 LOCATE ,T:PRINT "  Ё     Ё  юды  Ё  юды  Ё  юды  Ё  юды  Ё  юды  Ё     Ё  "
1090 LOCATE ,T:PRINT " Ro    мьмCe   мьмC12  мьмC23  мьмC34  мьмC45  мьмCe  Ro "
1100 LOCATE ,T:PRINT "  Ё     Ё       Ё       Ё       Ё       Ё       Ё     Ё  "
1110 LOCATE ,T:PRINT "  юдддддадддддддадддддддадддддддадддддддадддддддаддддды  "
1120 COLOR 7,0
1130 RETURN
1140 '
1150 '.....preface
1160 T=7
1170 PRINT TAB(T);
1180 PRINT "An in-depth discussion of Crystal Ladder Filters appears in the"
1190 PRINT TAB(T);
1200 PRINT "A.R.R.L. publication ";CHR$(34);;"W1FB's DESIGN NOTEBOOK";CHR$(34);
1210 PRINT ", starting on page"
1220 PRINT TAB(T);
1230 PRINT "179, from an original paper by Wes Hayward, W7ZOI, in the May 1982"
1240 PRINT TAB(T);
1250 PRINT "QST, page 21."
1260 PRINT
1270 PRINT TAB(T);
1280 PRINT "This program solves the equations presented in these articles, and"
1290 PRINT TAB(T);
1300 PRINT "may also be used as a stand-alone filter design tool."
1310 RETURN
1320 '
1330 'PRT
1340 KEY OFF:GOSUB 1410:LOCATE 25,5:COLOR 0,2
1350 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1360 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1370 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1370 ELSE GOSUB 1410
1380 IF Z$="3"THEN RETURN
1390 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1400 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1340
1410 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

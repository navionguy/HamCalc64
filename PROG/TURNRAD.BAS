1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$,DIMN$,A,B,C
10 'TURNRAD - Turning Radius - Beam Antennas - 20 FEB 2002
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 COLOR 7,0,1
70 UL$=STRING$(80,205)
80 M$(1)=" Length of boom............................... A"
90 M$(2)=" Length of each leg of long end element....... B"
100 M$(3)=" Length of each leg of short end element...... C"
110 M$(4)=" Distance from pivot to long end element...... D"
120 M$(5)=" Distance from pivot to short end element..... E"
130 M$(6)=" Distance from pivot to element ends 1,2,3,4.. R"
140 P$=" = ###.###"
150 GOTO 200
160 '
170 VIEW PRINT 15 TO 24:CLS:VIEW PRINT:LOCATE 15
180 RETURN
190 '
200 '.....title page
210 CLS
220 COLOR 15,2
230 PRINT " TURNING RADIUS - Beam Antennas";TAB(57);"by George Murphy VE3ERP ";
240 COLOR 1,0:PRINT STRING$(80,223);
250 COLOR 7,0
260 '
270 '.....start
280 GOSUB 940  'diagram
290 PRINT UL$;
300 GOSUB 760  'preface
310 PRINT UL$;
320 IF A*B*C THEN FLAG=1:GOTO 500
330 '
340 COLOR 0,7:LOCATE ,23:PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
350 Z$=INKEY$:IF Z$=""THEN 350
360 IF Z$="0"THEN COLOR ,,0:CLS:CHAIN GO$
370 IF Z$="1"THEN 400
380 GOTO 350
390 '
400 '.....data input
410 GOSUB 170
420 FOR Z=1 TO 3
430 IF N(Z)THEN 440 ELSE GOSUB 170:PRINT " ENTER:";M$(Z);:INPUT N(Z):
440 NEXT Z
450 A=N(1):B=N(2):C=N(3)
460 IF A*B*C THEN 470 ELSE 420
470 GOSUB 560
480 GOSUB 1100:A=0:B=0:C=0:D=0:R=0:FOR Z=1 TO 6:N(Z)=0:NEXT Z:GOTO 200
490 '
500 '.....chain program
510 N(1)=A:N(2)=B:N(3)=C
520 GOSUB 560
530 PRINT " Sketch is typical for any number of intermediate elements."
540 GOSUB 1100:CHAIN PROG$
550 '
560 '.....calculate
570 F1=(B-C)/2
580 F2=(B+C)/2
590 F3=A/2
600 E=F1*F2/F3+F3:N(5)=E
610 D=A-E:N(4)=D
620 R=SQR(C^2+E^2):N(6)=R
630 GOSUB 170
640 '
650 '.....screen display
660 FOR Z=1 TO 6
670 PRINT M$(Z)USING P$;N(Z);
680 PRINT USING " ft. = ###.### m.";N(Z)*3.048000E-01
690 IF Z=5 THEN PRINT " Turning radius:"
700 NEXT Z
710 PRINT UL$;
720 FLAG=0
730 RETURN
740 END
750 '
760 '.....preface
770 T=8
780 PRINT TAB(T);
790 PRINT "Minimum turning radius of a beam is achieved when the boom is"
800 PRINT TAB(T);
810 PRINT "attached to the mast at a point where tips 1,2,3,4 of the endmost"
820 PRINT TAB(T);
830 PRINT "radiators are all the same distance from the pivot. This program"
840 PRINT TAB(T);
850 PRINT "calculates the location of the pivot point. If this point does not"
860 PRINT TAB(T);
870 PRINT "coincide with the centre of gravity of the beam, addition of"
880 PRINT TAB(T);
890 PRINT "suitably located counterweights is recommended to obtain both"
900 PRINT TAB(T);
910 PRINT "static balance and minimum turning radius.";
920 RETURN
930 '
940 '.....diagram
950 COLOR 0,7:T=25
960 LOCATE ,T:PRINT "                              "
970 LOCATE ,T:PRINT "          ณฎฤ C ฤฏณฎฤ C ฤฏณ   "
980 LOCATE ,T:PRINT "  ฺฏฺฤฤฤฏ 1อออออออหอออออออ2   "
990 LOCATE ,T:PRINT "  ณ ณ             บ           "
1000 LOCATE ,T:PRINT "  ณ E             บ           "
1010 LOCATE ,T:PRINT "  A ณ     ออออออออฮออออออออ   "
1020 LOCATE ,T:PRINT "  ณ รฤฤฤฤฤฤฤฤฤฤฤฏ ื ฎฤ pivot  "
1030 LOCATE ,T:PRINT "  ณ D             บ           "
1040 LOCATE ,T:PRINT "  ภฏภฤฏ 4อออออออออสอออออออออ3 "
1050 LOCATE ,T:PRINT "        ณฎฤฤ B ฤฤฏณฎฤฤ B ฤฤฏณ "
1060 LOCATE ,T:PRINT "                              "
1070 COLOR 7,0
1080 RETURN
1090 '
1100 '.....PRT
1110 KEY OFF:GOSUB 1180:LOCATE 25,5:COLOR 0,2
1120 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1130 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1140 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1140 ELSE GOSUB 1180
1150 IF Z$="3"THEN RETURN
1160 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1170 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1110
1180 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

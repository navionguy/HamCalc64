1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$,D,L,Q,F,FQ,UH,LD,MIN,MAX,NN,I,I$,W,LL,DD,FD,WW,LW,FF,C
10 'COILTRUE - Coil Self-Capacitance - 03 JUL 2001 rev. 28 JAN 06, 30 MAY 10
20 CLS:KEY OFF:COLOR 7,0,0
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 
70 PI=3.141592
80 JJ=1/(4*PI^2)*10^6    'jj=25330.29
90 UU=UH
100 IF GO$="mowhip"THEN RUN EX$
110 IF GO$="miniquad"THEN FF=F/10^6:UU=UH:DD=D*2.539999:LL=L*2.539999:WW=W*2.539999:GOTO 760
120 IF GO$="trapcalc"THEN H=2.539999:GOTO 760
130 IF I$=""OR I$=" cm."THEN H=1:H$=" cm."
140 IF I$=" in."THEN H=2.539999
150  DD=D*2.539999
160  LL=L*2.539999
170  WW=W*2.539999
180  FF=F/10^6
190 ON ERROR GOTO 220
200 GOTO 260
210 '
220 '.....error trap
230 BEEP:CLS:COLOR 7,0:LOCATE 12,18
240 PRINT " error";ERR;;"in line";ERL:STOP
250 '
260 '.....title page
270 CLS
280 COLOR 15,2,0
290 PRINT " COIL Q - True vs. Apparent";TAB(57);"by George Murphy VE3ERP ";
300 COLOR 1,0:PRINT STRING$(80,223);
310 COLOR 7,0
320 IF PROG$="coilq"OR PROG$="coildsgn"THEN 760
330 IF PROG$="coilequa"OR PROG$="shortant"THEN 760
340 '
350 TB=7:TA=28
360 PRINT TAB(TB);
370 PRINT "This program calculates the self-capacitance (distributed capacity)"
380 PRINT TAB(TB);
390 PRINT "of single-layer air core coils, and its effect on the difference"
400 PRINT TAB(TB);
410 PRINT "between the true Q of a coil and the apparent Q indicated by a"
420 PRINT TAB(TB);
430 PRINT "Q meter. The self-resonant frequency of the coil is also calculated."
440 PRINT
450 COLOR 0,7
460 LOCATE ,(TA):PRINT "                         "
470 LOCATE ,(TA):PRINT "             ãD          "
480 LOCATE ,(TA):PRINT " Co = ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ "
490 LOCATE ,(TA):PRINT "      3.6 x arccosh(s/d) "
500 LOCATE ,(TA):PRINT "                         "
510 COLOR 7,0
520 PRINT
530 PRINT "      where Co = distributed capacity in pF"
540 PRINT "             D = coil diameter in centimetres"
550 PRINT "             s = spacing (pitch) of turns"
560 PRINT "             d = conductor diameter"
570 PRINT "             s and d are in same units"
580 PRINT
590 PRINT TAB(TB);
600 PRINT "Ref.   Electronic Equations Handbook, Tab/McGraw Hill, p.6, eq.1-38."
610 PRINT
620 COLOR 0,7
630 LOCATE ,21:PRINT " Press 1 for dimensions in centimetres "
640 LOCATE ,21:PRINT " Press 2 for dimensions in inches      "
650 LOCATE ,21:PRINT " Press 0 to EXIT                       ";
660 COLOR 7,0
670 Z$=INKEY$:IF Z$=""THEN 670
680 IF Z$="0"THEN CLS:CHAIN GO$
690 IF Z$="1"THEN H$=" cm.":H=1:GOTO 730
700 IF Z$="2"THEN H$=" in.":H=2.539999:GOTO 730
710 GOTO 670
720 '
730 '.....start
740 IF FLAG THEN 770
750 DD=0:LL=0:UU=0:FF=0:QA=0:QT=0:WW=0
760 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
770 '.....data input
780 IF UU=0 THEN INPUT " ENTER: True Inductance ( æH )...";UU
790 IF UU=0 THEN LOCATE CSRLIN-1:GOTO 780
800 IF DD=0 THEN PRINT " ENTER: Coil Pitch Dia.(";H$;;")....";:INPUT DD:DD=DD*H
810 IF DD=0 THEN LOCATE CSRLIN-1:GOTO 800
820 IF LL=0 THEN PRINT " ENTER: Coil Length (";H$;;").......";:INPUT LL:LL=LL*H
830 IF LL=0 THEN LOCATE CSRLIN-1:GOTO 820
840 IF NN=0 THEN INPUT " ENTER: Number of turns..........";NN
850 IF NN=0 THEN LOCATE CSRLIN-1:GOTO 840
860 IF H=1 THEN TPI=NN/LL*2.539999
870 IF H=2.539999 THEN TPI=N/LL*H
880 TPI=NN/LL*2.539999
890 PT=LL/NN
900 IF WW=0 THEN PRINT  " ENTER: Wire diameter (";H$;;").....";:INPUT WW:WW=WW*H
910 FD=DD-WW   'coil form diameter
920 IF WW=0 THEN LOCATE CSRLIN-1:GOTO 900
930 '
940 P=WW*PI     'wire perimeter in cm.
950 WL=PI*DD*NN 'length of wire in cm.
960 '
970 QA=0:QC=0:QT=0:COLOR 0,7:LIN=CSRLIN
980 IF FF=0 THEN INPUT " ENTER: Frequency (MHz)..........";FF
990 COLOR 7,0:IF FF=0 THEN LOCATE CSRLIN-1:GOTO 980
1000 '
1010 QT=SQR(FF)*10^3/((6.900000/DD*2+5.400000/LL)) 'true Q
1020 CR=JJ/FF^2/UU       'capacitance to resonate
1030 X=10^6/(2*PI*FF*CR) 'reactance at resonant frequency
1040 IF FLAG=1 THEN 1060
1050 LD=LL/DD
1060 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1070 '
1080 '.....find self-capacitance
1090 IF FLAG=1 THEN DD=DD/2.539999
1100 'REM DD = coil dia. in cm.
1110 SW=PT/WW         'pitch:wire dia. ratio
1120 PRINT SWSTOP
1130 IHC=LOG(SW+SQR(SW^2-1))  'inverse hyperbolic cosine
1140 CO=PI*DD/(3.599999*IHC)
1150 FS=SQR((2.533028E+04/(CO*LL)))   'resonant frequency
1160 C=CR-CO          'additional external capacitance
1170 RT=2*PI*FF*UU/QT 'total true resistance
1180 RA=RT*(1+CO/C)^2
1190 UA=UU*(1+CO/C)   'apparent inductance
1200 QC=QT/(1+CO/C)   'apparent Q due to distributed capacity
1210 XA=2*PI*FF*UA    'apparent reactance of Ua
1220 '.....display
1230 CLS:COLOR 7,0
1240 PRINT USING " True inductance........................= #####.### uH";UU
1250 PRINT USING " Number of turns........................= #####.###";NN
1260 IF FLAG=1 THEN FD=(D-W)*2.539999
1270 PRINT USING " Coil form diameter...................fd= #####.### cm.";FD;
1280 PRINT USING " = ###.### in.";FD/2.539999
1290 IF FLAG=1 THEN DD=D*2.539999
1300 PRINT USING " Coil pitch diameter...................d= #####.### cm.";DD;
1310 PRINT USING " = ###.### in.";DD/2.539999
1320 PRINT USING " Wire diameter.........................W= #####.### cm.";WW;
1330 PRINT USING " = ###.### in.";WW/2.539999;
1340 J=WW/2.539999
1350 K=(4.600000E-01/4.999999E-03)^(1/39)
1360 WG=LOG(4.600000E-01/J)/(LOG(K))-3
1370 LW=PI*DD*NN  'wire length
1380 PRINT USING "(ñ AWG ##)";WG
1390 PRINT USING " Pitch (c.c. spacing of turns)........Pt= #####.### cm.";PT;
1400 PRINT USING " = ###.### in.";PT/2.539999
1410 PRINT USING " Coil Length...................Lcm, Lgth= #####.### cm.";LL;
1420 PRINT USING " = ###.### in.";LL/2.539999
1430 PRINT USING " Wire required........................Lw= #####.### cm.";LW;
1440 PRINT USING " = ###.### in.";LW/2.539999
1450 PRINT USING " Turns per inch (2.54 cm)............Tpi= #####.###";TPI
1460 PRINT USING " Coil Length/diameter ratio...........Ld= #####.##:1";LD
1470 PRINT USING " Turns pitch/wire diameter ratio......Sw= #####.###";SW
1480 PRINT USING " Self-Capacitance.....................Co= #####.### pF ";CO
1490 COLOR 7,0
1500 PRINT USING " Added capacity required to resonate..Cx= #####.### pF";CR-CO
1510 COLOR 14
1520 PRINT USING " Frequency.............................Ÿ= #####.### MHz";FF
1530 COLOR 12
1540 PRINT USING " Self-Resonant frequency..............Ÿs= #####.### MHz";FS
1550 COLOR 0,7
1560 PRINT USING "   Apparent inductance................La= #####.### æH  ";UA
1570 COLOR 7,0
1580 IF FF>FS THEN 1900
1590 PRINT USING " True reactance at resonance...........X= #####.### ohms";X
1600 COLOR 0,7
1610 PRINT USING "   Apparent reactance at resonance....Xa= #####.### ohms";XA
1620 COLOR 7,0
1630 PRINT USING " Total true resistance................Rc= #####.### ohms";RT
1640 COLOR 0,7
1650 PRINT USING "   Apparent total resistance..........Rt= #####.### ohms";RA
1660 COLOR 14,0
1670 PRINT USING " True Q...............................Qt= #####         ";QT
1680 COLOR 0,7
1690 PRINT USING "   Apparent Q.........................Qc= #####         ";QC
1694 COLOR 7,0:R=INT(QT*X/100):BW=INT(FF/QT*10000)/10
1695 PRINT " Load resistance=";R;" megê.......Bandwidth=";BW;" Mhz"
1700 COLOR 7,0:G=58:LOCATE 10
1710 LOCATE ,G:PRINT " ÚÄÄÄÄ COIL DETAILS ÄÄ¿";
1720 LOCATE ,G:PRINT " ³Apparent values are ³";
1730 LOCATE ,G:PRINT " ³as read by a meter. ³";
1740 LOCATE ,G:PRINT " ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´";
1750 LOCATE ,G:PRINT " ³Coil form losses are³";
1760 LOCATE ,G:PRINT " ³not included because³";
1770 LOCATE ,G:PRINT " ³the forms used with ³";
1780 LOCATE ,G:PRINT "®´single layer coils  ³";
1790 LOCATE ,G:PRINT " ³have little effect  ³";
1800 LOCATE ,G:PRINT "®´upon losses provided³";
1810 LOCATE ,G:PRINT " ³the dielectrics     ³";
1820 LOCATE ,G:PRINT "®´involved do not     ³";
1830 LOCATE ,G:PRINT " ³absorb moisture.    ³";
1840 LOCATE ,G:PRINT "®´ (Terman - p.74)    ³";
1850 LOCATE ,G:PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ";
1860 IF PROG$="coilq"AND FLAG=1 THEN GOSUB 2150:RUN"coilq"
1870 IF PROG$="coilq"AND FLAG=0 THEN FLAG=1:CHAIN"coildsgn"
1880 GOTO 1970
1890 PRINT
1900 '.....frequency error
1910 BEEP:PRINT :COLOR 12:PRINT ;" Q CANNOT BE COMPUTED AT FREQUENCIES";
1920 PRINT " HIGHER THAN THE SELF-RESONANT FREQUENCY!"
1930 LOCATE 15,57:PRINT "®- ****"
1940 LOCATE 16,57:PRINT "®- ****"
1950 COLOR 7,0
1960 '
1970 GOSUB 2150:LOCATE 25,13:COLOR 15,2
1980 IF PROG$="shortant"THEN UH=0
1990 IF PROG$="coildsgn"THEN CLEAR:RUN"coildsgn"
2000 IF PROG$="coilequa"OR PROG$="shortant"THEN 2070
2010 IF GO$="trapcalc"THEN 2070
2020 PRINT " Press 1 for another frequency or 0 to return to menu ";:COLOR 7,0
2030 Z$=INKEY$
2040 IF Z$="0"THEN 260
2050 IF Z$="1"THEN 2130
2060 GOTO 2030
2070 LOCATE 25,26:PRINT " . . . Press any key . . . ";
2080 Z$=INKEY$
2090 IF INKEY$=""THEN 2090
2100 IF PROG$="shortant"THEN RUN PROG$
2110 IF GO$="trapcalc"THEN CHAIN GO$
2120 GOTO 2080
2130 FF=0:VIEW PRINT LIN TO 24:CLS:VIEW PRINT:LOCATE LIN:GOTO 950
2140 '
2150 '.....PRT
2160 KEY OFF:GOSUB 2230:LOCATE 25,5:COLOR 0,2
2170 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2180 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2190 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2190 ELSE GOSUB 2230
2200 IF Z$="3"THEN RETURN
2210 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2220 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2160
2230 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

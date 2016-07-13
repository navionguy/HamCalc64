1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON PROG$,EX$
10 'PRECIRES - Precision Resistor - 07 SEP 94 rev. 23 NOV 2000
20 IF EX$=""THEN EX$="EXIT"
30 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
40 
50 CLS:KEY OFF
60 COLOR 7,0,1
70 DIM R(25)            'resistors
80 UL$=STRING$(80,205)  'underline
90 ER$=STRING$(80,32)   'erase
100 U1$="########,##.#"
110 U2$="#########.###"
120 U3$="#########.##"+" "
130 U4$="#######,###"+"  "
140 U5$="####.#"
150 O$=" ê"
160 VIEW PRINT 3 TO 23:CLS:VIEW PRINT
170 DATA 10,11,12,13,15,16,18,20,22,24,27,30,33,36,39
180 DATA 43,47,51,56,62,68,75,82,91,100
190 FOR Z=0 TO 24:READ R(Z):NEXT Z
200 '
210 '.....start
220 CLS
230 COLOR 15,2
240 PRINT " PRECISION RESISTOR";TAB(57);;"by George Murphy VE3ERP ";
250 COLOR 1,0:PRINT STRING$(80,"ß");
260 COLOR 7,0
270 '
280 '.....diagram
290 T=24       'tab
300 COLOR 0,7
310 LOCATE  3,T:PRINT "                                   "
320 LOCATE  4,T:PRINT "  ³®ÄÄÄÄÄÄÄÄÄÄÄ R ÄÄÄÄÄÄÄÄÄÄÄÄÄ¯³  "
330 LOCATE  5,T:PRINT "  ³                             ³  "
340 LOCATE  6,T:PRINT "  ³    ÚÄÄÄÄÄÄ/\/\/\/ÄÄÄÄÄÄ¿    ³  "
350 LOCATE  7,T:PRINT "       ³        Ra         ³       "
360 LOCATE  8,T:PRINT "  XÄÄÄÄ´                   ÃÄÄÄÄY  "
370 LOCATE  9,T:PRINT "       ³                   ³       "
380 LOCATE 10,T:PRINT "       ÀÄÄÄÄÄÄ/\/\/\/ÄÄÄÄÄÄÙ       "
390 LOCATE 11,T:PRINT "                Rb                 "
400 COLOR 7,0
410 LOCATE 12:PRINT UL$;
420 IF R THEN 660
430 '
440 M=7
450 PRINT TAB(M);
460 PRINT "This program calculates the values of two standard value resistors"
470 PRINT TAB(M);
480 PRINT "Ra and Rb which, when connected in parallel, will result in a net"
490 PRINT TAB(M);
500 PRINT "resistance R that will be within very close tolerances of almost any"
510 PRINT TAB(M);
520 PRINT "value you want."
530 PRINT UL$;
540 '
550 '.....inputs
560 COLOR 0,7:LOCATE CSRLIN,22
570 PRINT " Press 1 to continue or 0 to EXIT.....";:COLOR 7,0
580 Z$=INKEY$:IF Z$=""THEN 580
590 IF Z$="0"THEN CLS:CHAIN GO$
600 IF Z$="1"THEN 630
610 GOTO 580
620 '
630 VIEW PRINT 13 TO 24:CLS:VIEW PRINT:LOCATE 13
640 PRINT " ENTER:";
650 '
660 LOCATE CSRLIN,8
670 COLOR 14,4
680 PRINT " Sought precise resistance R between X & Y ......(ohms)=";
690 COLOR 7,0
700 IF R THEN PRINT:GOTO 720 ELSE INPUT R  'if R chained from another program
710 IF R=0 THEN LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1:GOTO 550
720 IF R<10 THEN 970
730 LOCATE CSRLIN-1:PRINT STRING$(7,32)
740 LOCATE CSRLIN-1,55:COLOR 14,4:PRINT "......R ="
750 LOCATE CSRLIN-1,64:PRINT USING U1$;R;:PRINT O$:COLOR 7,0
760 '
770 '.....find next larger standard resistor
780 M=1.000000E-01                   'multiplier
790 '
800 FOR Z=1 TO 24
810  RA=R(Z)*M             'Ra= standard resistor
820  IF RA=R THEN 880
830  IF RA>=R THEN 1060    'exit for/next loop
840 NEXT Z
850 M=M*10
860 GOTO 800               'run loop again
870 '
880 '.....standard resistor entered
890 BEEP:PRINT
900 COLOR 0,7
910 LOCATE CSRLIN,17:PRINT " ...... This is a standard resistor value ...... "
920 LOCATE CSRLIN,17:PRINT " ................ Press any key ................ "
930 COLOR 7,0
940 IF INKEY$=""THEN 940
950 GOTO 2150   'end
960 '
970 '.....resistor <10 ohms entered
980 BEEP:PRINT :COLOR 0,7
990 PRINT " For precision resistors less than 10 ê refer to the "
1000 PRINT " HAMCALC program RESISTORS - COPPER WIRE WOUND.      "
1010 PRINT " .................. Press any key .................. "
1020 COLOR 7,0
1030 IF INKEY$=""THEN 1030
1040 GOTO 2150  'start
1050 '
1060 PRINT "        Value of next standard resistor greater than R.....Ra =";
1070 PRINT USING U4$;RA;:PRINT O$
1080 '
1090 RX=R*RA/(RA-R)         'parallel resistor
1100 '
1110 '.....find nearest standard resistor
1120 M=1                    'multiplier
1130 '
1140 FOR Z=1 TO 24
1150  RH=R(Z)*M             'RS= next higher standard resistor
1160  IF RH>=RX THEN 1210
1170 NEXT Z
1180 M=M*10
1190 GOTO 1140               'run loop again
1200 '
1210 RL=R(Z-1)*M            'next lower standard resistor
1220 RM=(RL+RH)/2           'median value
1230 IF RX<RM THEN RB=RL
1240 IF RX>=RM THEN RB=RH
1250 '
1260 '.....display results
1270 PRINT "        Value of parallel resistor to obtain R precisely..... =";
1280 PRINT USING U1$;RX;:PRINT O$
1290 '
1300 PRINT "        Value of closest standard parallel resistor........Rb =";
1310 PRINT USING U4$;RB;:PRINT O$
1320 '
1330 RC=1/(1/RA+1/RB)
1340 PRINT "       ";
1350 COLOR 15,0
1360 PRINT " RESISTANCE OF Ra & Rb IN PARALLEL BETWEEN X & Y....Rc =";
1370 PRINT USING U1$;RC;:PRINT O$
1380 COLOR 7,0
1390 '
1400 D=ABS(R-RC)/R*100
1410 COLOR 14,4:LOCATE CSRLIN,8
1420 PRINT " DIVERGENCE of Rc from R.............................. =";
1430 PRINT USING U2$;D;:PRINT " %"
1440 COLOR 7,0
1450 '
1460 IF E=-1 THEN 1800                'if E chained from another program
1470 IF E<>0 THEN PRINT :GOTO 1590    'if E chained from another program
1480 COLOR 15,1:LOCATE CSRLIN,8
1490 PRINT " Do you want to calculate current through Ra & Rb ?  (y/n) ";
1500 COLOR 7,0
1510 Z$=INKEY$
1520 IF Z$="n"OR Z$="N"THEN 1560
1530 IF Z$="y"OR Z$="Y"THEN 1590
1540 GOTO 1510
1550 '
1560 LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1:PRINT UL$;:GOSUB 1880
1570 GOTO 2150   'end
1580 '
1590 '.....calculate current
1600 LOCATE CSRLIN-1:PRINT STRING$(80,32);:LOCATE CSRLIN-1
1610 PRINT " ENTER: Voltage drop between X & Y..........................E =";
1620 IF E THEN PRINT :GOTO 1630 ELSE INPUT E
1630 LOCATE CSRLIN-1:PRINT STRING$(7,32)
1640 LOCATE CSRLIN-1,64:PRINT USING U2$;E;:PRINT " v."
1650 '
1660 I=E/RA:GOSUB 1830
1670 PRINT "        Current through Ra @............................I(Ra) =";
1680 PRINT USING U2$;II;:PRINT V$
1690 P=I*E:LOCATE CSRLIN-1,29:PRINT USING "#####.###";P;:PRINT " watts"
1700 '
1710 I=E/RB:GOSUB 1830
1720 PRINT "        Current through Rb @............................I(Rb) =";
1730 PRINT USING U2$;II;:PRINT V$
1740 P=I*E:LOCATE CSRLIN-1,29:PRINT USING "#####.###";P;:PRINT " watts"
1750 '
1760 I=E/RC:GOSUB 1830
1770 PRINT "        Total current between X and Y...................I(Rc) =";
1780 PRINT USING U3$;II;:PRINT V$
1790 '
1800 PRINT UL$;:GOSUB 1880
1810 GOTO 2150   'end
1820 '
1830 '.....unit of current
1840 IF I>=1 THEN II=I:V$=" A."
1850 IF I<1 THEN II=I*10^3:V$=" mA"
1860 RETURN
1870 '
1880 '.....add to diagram
1890 '
1900 N=1
1910 IF 10^N<=R THEN N=N+1:GOTO 1910
1920 COLOR 7,0
1930 X1=RA:X1$=" ê"
1940 IF RA>10^3 THEN X1=RA/10^3:X1$=" kê"
1950 IF RA>10^6 THEN X1=RA/10^6:X1$=" megê"
1960 '
1970 X2=RB:X2$=" ê"
1980 IF RB>10^3 THEN X2=RB/10^3:X2$=" kê"
1990 IF RB>10^6 THEN X2=RB/10^6:X2$=" megê"
2000 '
2010 X3=RC:X3$="ê"
2020 IF RC>10^3 THEN X3=RC/10^3:X3$="kê"
2030 IF RC>10^6 THEN X3=RC/10^6:X3$="megê"
2040 X3=INT(X3*10^3+5.000000E-01)/10^3
2050 N=1
2060 IF 10^N<X3 THEN N=N+1:GOTO 2060
2070 UX$=STRING$(N,"#")+".###"
2080 '
2090 COLOR 14,4
2100 LOCATE 4,T+10:PRINT " Rc= ";USING UX$;X3;:PRINT " ";X3$
2110 LOCATE 7,T+10:PRINT " Ra=";USING U5$;X1;:PRINT X1$
2120 LOCATE 11,T+10:PRINT " Rb=";USING U5$;X2;:PRINT X2$
2130 COLOR 7,0
2140 '
2150 '.....end
2160 GOSUB 2220
2170 IF EX$<>GO$ THEN CLS:CHAIN GO$
2180 E=0:R=0:RA=0:RB=0:RC=0:RH=0:RX=0
2190 GOTO 210
2200 END
2210 '
2220 'PRT
2230 KEY OFF:GOSUB 2300:LOCATE 25,5:COLOR 0,2
2240 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2250 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2260 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2260 ELSE GOSUB 2300
2270 IF Z$="3"THEN RETURN
2280 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2290 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2230
2300 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

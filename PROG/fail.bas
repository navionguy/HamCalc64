1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'TRANSMAT - Transmatch Design - 28 SEP 95 rev.06 SEP 97
20 'adapted from TUNER.BAS version 1.5 by Brian Egan, ZL1LE
30 NONOTE=0
40 IF EX$=""THEN EX$="EXIT"
50 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
60 
70 CLS:KEY OFF
80 COLOR 7,0,1
90 V=0
100 PI=3.141592
110 U1$="#####.###"
120 U2$="#####.##"
130 UL$=STRING$(80,205)
140 E$=STRING$(79,32)
150 '
160 '.....start
170 CLS
180 COLOR 15,2
190 PRINT " TRANSMATCH DESIGN";TAB(61);;"by Brian Egan ZL1LE ";
200 PRINT STRING$(80,32);
210 LOCATE CSRLIN-1,20:PRINT "edited for HAMCALC by George Murphy VE3ERP"
220 COLOR 1,0:PRINT STRING$(80,223);:COLOR 7,0
230 '
240 '.....main menu
250 T=(9)
260 PRINT TAB(T);
270 PRINT "This program may be used to:"
280 PRINT TAB(T);
290 PRINT STRING$(28,196)
300 PRINT
310 PRINT " 1.";TAB(T);
320 PRINT "Design Transmatch circuits of several different types...."
330 PRINT TAB(T);
340 PRINT
350 PRINT " 2.";TAB(T);
360 PRINT "Compute SWR when transmatch components are varied in value...."
370 PRINT TAB(T);
380 PRINT "(e.g. to show effect of variable capacitor/inductor adjustments)."
390 PRINT
400 PRINT " 3.";TAB(T);
410 PRINT "Measure an unknown load by using a matched transmatch as an"
420 PRINT TAB(T);
430 PRINT "impedance bridge...."
440 PRINT
450 PRINT UL$;
460 COLOR 0,7:LOCATE 17,22
470 PRINT " Press 1 to continue or 0 to EXIT....."
480 COLOR 7,0
490 Z$=INKEY$:IF Z$=""THEN 490
500 IF Z$="0"THEN CLS:CHAIN GO$
510 IF Z$="1"THEN 530
520 GOTO 490
530 LOCATE CSRLIN-1:PRINT E$
540 IF NONOTE=1 THEN 630
550 LOCATE 17,9:PRINT "Do you wish to read the program notes (y/n)?"
560 Y$=INKEY$:IF Y$=""THEN 560
570 IF Y$="N"OR Y$="n"THEN NONOTE=1:GOTO 610
580 IF Y$="Y"OR Y$="y"THEN 6820
590 GOTO 560
600 '
610 VIEW PRINT 4 TO 24:CLS:VIEW PRINT:LOCATE 4
620 NONOTE=0
630 PRINT " PRESS a number in ( ) below to select program option:"
640 PRINT UL$;
650 T=3
660 PRINT TAB(T);;"(a)   PI-COUPLER TRANSMATCH DESIGN"
670 PRINT TAB(T);;"(b)   PI-COUPLER IMPEDANCE BRIDGE"
680 PRINT TAB(T);;"(c)   SPC (includes High Pass Tee) TRANSMATCH DESIGN"
690 PRINT TAB(T);;"(d)   SPC (includes High Pass Tee) IMPEDANCE BRIDGE"
700 PRINT TAB(T);;"(e)   HIGH PASS TEE TRANSMATCH (finite Q conductor)";
710 PRINT TAB(T);;"(f)   LOW PASS TEE TRANSMATCH DESIGN"
720 PRINT TAB(T);;"(g)   LOW PASS TEE IMPEDANCE BRIDGE"
730 PRINT TAB(T);;"(h)   `ULTIMATE' TRANSMATCH DESIGN"
740 PRINT TAB(T);;"(i)   `ULTIMATE' IMPEDANCE BRIDGE"
750 PRINT TAB(T);;"(j)   2-ELEMENT LADDER MATCHING NETWORK"
760 PRINT
770 COLOR 14
780 PRINT TAB(T);;"(z)   QUIT"
790 COLOR 7
800 Z$=INKEY$:IF Z$=""THEN 800
810 IF Z$="z"THEN S=11 ELSE S=ASC(Z$)-96
820 CLS:COLOR 7,0,1
830 ON S GOSUB 1040,1680,2080,3550,3990,4020,5020,5380,6450,850,860
840 END
850 CLS:CHAIN"ladder2"
860 CLS:CHAIN EX$
870 '
880 '.....format input line
890 LOCATE CSRLIN-1:PRINT SPC(7);
900 LOCATE CSRLIN,47:PRINT STRING$(7,".");USING U$;ZZ;
910 RETURN
920 '
930 '.....enter 4 of 5 transmatch parameters for PI-COUPLER and SPC
940 INPUT " ENTER: Transmatch Input Impedance R1..........(ohms)";R1
950 ZZ=R1:U$=U2$:GOSUB 880:PRINT " �"
960 INPUT " ENTER: Load Resistance RL.....................(ohms)";RL
970 ZZ=RL:U$=U2$:GOSUB 880:PRINT " �"
980 INPUT " ENTER: Load Reactance.........................(ohms)";XS
990 ZZ=XS:U$=U2$:GOSUB 880:PRINT " �"
1000 INPUT " ENTER: Frequency...............................(MHz)";F
1010 ZZ=F :U$=U1$:GOSUB 880:PRINT " MHz"
1020 RETURN
1030 '
1040 '.....PI COUPLER transmatch
1050 T$="PI-COUPLER TRANSMATCH DESIGN"
1060 T=(80-LEN(T$))/2
1070 PRINT TAB(T);T$
1080 PRINT UL$;
1090 GOSUB 1560   'diagram
1100 PRINT UL$;
1110 GOSUB 930    'data input
1120 '
1130 '.....calculation of transmatch components
1140 W=2*PI*F*10^6
1150 Z=RL^2+XS^2:R2=Z/RL:CP=-XS/Z/W:LMAX=SQR(R1*R2)/W
1160 IF CP<=1/(W^2*LMAX)THEN 1200
1170 A1=1+W^2*CP^2*R2^2:A2=-2*W*CP*R2^2:A3=R1*R2-R2*R2
1180 LM1=-A2/2/A1+SQR(A2^2/4/A1/A1+A3/A1):LM1=LM1/W
1190 IF LM1<LMAX THEN LMAX=LM1
1200 PRINT " Inductor must be less than";:PRINT USING "###.###";LMAX*10^6;
1210 PRINT " uH. ";
1220 INPUT " ENTER: Value of INDUCTOR in uH: ";L
1230 IF L>LMAX*10^6 THEN LOCATE CSRLIN-1:PRINT E$:LOCATE CSRLIN-1:GOTO 1200
1240 LOCATE CSRLIN-1:PRINT E$:LOCATE CSRLIN-1
1250 PRINT "        Inductor L...................................";USING U1$;L;
1260 PRINT " �H"
1270 PRINT "        SWR..........................................    1:1"
1280 XL=W*L*10^-6
1290 K=SQR(R1*R2-XL^2)/R1:C1=(1-K)/W/XL:C2=(1-R1/R2*K)/W/XL
1300 C1A=(1+K)/W/XL:C2A=(1+R1/R2*K)/W/XL
1310 PRINT " SOLUTION 1:"
1320 IF C2-CP<0 THEN 1410
1330 IF K>1 THEN PRINT TAB(9);;"(no solution)";:GOTO 1400
1340 ZZ=C1*10^12
1350 PRINT TAB(9);;"Capacitor C1.................................";USING U2$;ZZ;
1360 PRINT " pF"
1370 ZZ=(C2-CP)*10^12
1380 PRINT TAB(9);;"Capacitor C2.................................";USING U2$;ZZ;
1390 PRINT " pF"
1400 LOCATE 19:PRINT " SOLUTION 2:"
1410 ZZ=C1A*10^12
1420 PRINT TAB(9);;"Capacitor C1.................................";USING U2$;ZZ;
1430 PRINT " pF"
1440 ZZ=(C2A-CP)*10^12
1450 PRINT TAB(9);;"Capacitor C2.................................";USING U2$;ZZ;
1460 PRINT " pF"
1470 GOSUB 8820     'calculate �
1480 LOCATE 23,9:PRINT "Do you wish to vary the inductor size  (y/n)?"
1490 Y$=INKEY$:IF Y$="" THEN 1490
1500 IF Y$="Y"OR Y$="y"THEN 1540
1510 GOSUB 7650    'calculate SWR
1520 LOCATE CSRLIN-1:PRINT E$
1530 GOTO 9210
1540 VIEW PRINT 14 TO 24:CLS:VIEW PRINT:LOCATE 14:GOTO 1200
1550 '
1560 '.....PI COUPLER circuit diagram
1570 COLOR 0,7
1580 T=26
1590 LOCATE CSRLIN,T:PRINT "              L              "
1600 LOCATE CSRLIN,T:PRINT " R1 �������������������į RL "
1610 LOCATE CSRLIN,T:PRINT " input  �           �   load "
1620 LOCATE CSRLIN,T:PRINT "       ��� C1      ��� C2    "
1630 LOCATE CSRLIN,T:PRINT "        �           �        "
1640 LOCATE CSRLIN,T:PRINT "  grnd ///         ///       "
1650 COLOR 7,0
1660 RETURN
1670 '
1680 '.....PI COUPLER impedance bridge
1690 CLS
1700 T$="PI-COUPLER IMPEDANCE BRIDGE"
1710 T=(80-LEN(T$))/2
1720 PRINT TAB(T);T$
1730 PRINT UL$;
1740 GOSUB 1560
1750 PRINT UL$;
1760 '
1770 INPUT " ENTER: Transmatch Input Impedance R1..........(ohms)";R1
1780 ZZ=R1:U$=U2$:GOSUB 880:PRINT " �"
1790 INPUT " ENTER: Value of inductor L....................  (�H)";L
1800 ZZ=L:U$=U2$:GOSUB 880:PRINT " �H"
1810 INPUT " ENTER: Value of capacitor C1..................  (pF)";C1
1820 ZZ=C1:U$=U2$:GOSUB 880:PRINT " pF"
1830 INPUT " ENTER: Value of capacitor C2..................  (pF)";C2
1840 ZZ=C2:U$=U2$:GOSUB 880:PRINT " pF"
1850 INPUT " ENTER: Frequency...............................(MHz)";F
1860 ZZ=F :U$=U1$:GOSUB 880:PRINT " MHz"
1870 W=2*PI*F*10^6
1880 C1=C1*10^-12:C2=C2*10^-12:L=L*10^-6
1890 K=(1-W^2*L*C1):R2=R1*(K^2+W^2*L^2/R1/R1)
1900 C0=C2+(C1*R1^2*K-L)/R1/R2
1910 B=1+W^2*C0^2*R2^2
1920 RL=R2/B:XL=W*C0*R2^2/B
1930 IF XL<0 THEN SUM$="-" ELSE SUM$="+"
1940 XL=ABS(XL)
1950 PRINT TAB(9);;"Load impedance...............................";
1960 PRINT USING "#####.#";INT(RL*10)/10;:PRINT " ";SUM$;" j";INT(XL*10)/10;"�"
1970 U=RL-R1:V=RL+R1:P=U^2+XL^2:Q=V^2+XL^2
1980 GAMMA=SQR(P/Q):SWR=(1+GAMMA)/(1-GAMMA)
1990 PRINT TAB(9);;"Load SWR.....................................";USING U2$;SWR;
2000 PRINT ":1"
2010 LOCATE 23,9:PRINT "Do you wish to make another calculation  (y/n)"
2020 Y$=INKEY$:IF Y$="" THEN 2020
2030 IF Y$="Y"OR Y$="y" THEN 2060
2040 LOCATE 23:PRINT E$
2050 GOTO 9210  'end
2060 VIEW PRINT 10 TO 24:CLS:VIEW PRINT:LOCATE 10:GOTO 1680
2070 '
2080 '.....SPC/HIGHPASS TEE transmatch design
2090 CLS
2100 T$="SPC TRANSMATCH DESIGN"
2110 T=(80-LEN(T$))/2
2120 PRINT TAB(T);T$
2130 PRINT UL$;
2140 GOSUB 3430  'diagram
2150 PRINT UL$;
2160 GOSUB 3040  'option menu
2170 GOSUB 930   'input R1,R,X,& F
2180 '
2190 '.....compute transmatch components
2200 W=2*PI*F*10^6
2210 IF RL>R1 THEN GOSUB 2660 ELSE GOSUB 2800
2220 '
2230 INPUT " ENTER: Value of C1 in pF .....";C1
2240 IF C1>=C1MAX THEN LOCATE CSRLIN-1:PRINT E$:LOCATE CSRLIN-1:GOTO 2230
2250 LN=CSRLIN-2:VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
2260 PRINT TAB(9);;"Capacitor C1.................................";USING U2$;C1;
2270 PRINT " pF"
2280 PRINT TAB(9);;"SWR..........................................    1:1"
2290 '
2300 '.....inductor calculation
2310 A=1/(W^2*RL*(R1-RL)+RL/(C1^2*10^-24*R1))
2320 CL=SQR(A):GOSUB 2950
2330 '
2340 PRINT " SOLUTION 1:"
2350 ZZ=C*10^12
2360 PRINT TAB(9);;"Capacitor C..................................";USING U2$;ZZ;
2370 PRINT " pF"
2380 ZZ=L*10^6
2390 PRINT TAB(9);;"Inductor L...................................";USING U2$;ZZ;
2400 PRINT " �H"
2410 GOSUB 9010     '� calculation
2420 '
2430 LOCATE 19:PRINT " SOLUTION 2:"
2440 CL=-SQR(A):GOSUB 2950
2450 IF XS<=0 OR V=0 THEN PRINT TAB(9);;"No second solution";:PRINT:GOTO 2580
2460 IF C<0 THEN LOCATE 20,9:PRINT "No second solution"
2470 IF C<0 THEN LOCATE 21,9:PRINT "for this value of C1";:GOTO 2580
2480 IF C<=1500*10^-12 AND L<>10^-9 THEN 2510
2490 LOCATE 20,9:PRINT "Impractical values"
2500 LOCATE 21,9:PRINT "for this value of C1";:GOTO 2580
2510 ZZ=C*10^12
2520 PRINT TAB(9);;"Capacitor C..................................";USING U2$;ZZ;
2530 PRINT " pF"
2540 ZZ=L*10^6
2550 PRINT TAB(9);;"Inductor L...................................";USING U2$;ZZ;
2560 PRINT " �H"
2570 GOSUB 9060  'calculate �
2580 LOCATE 23,9:PRINT "Do you wish to vary C1 (y/n)?:"
2590 Y$=INKEY$:IF Y$=""THEN 2590
2600 IF Y$="Y"OR Y$="y"THEN 2640
2610 GOSUB 7950  'calculate SWR
2620 LOCATE CSRLIN-1:PRINT E$
2630 GOTO 9210
2640 VIEW PRINT 14 TO 24:CLS:VIEW PRINT:LOCATE 14:GOTO 2210
2650 '
2660
2800
2950
3040
3430
3550
3990
4020
5020
5380
6450
6820 RETURN
7650
7950
8820
9010
9060
9210 RETURN

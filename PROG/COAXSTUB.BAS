1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$
10 'COAXSTUB - Stubs for Coaxial Lines - 03 JUN 96 rev. 05 SEP 2009
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 COLOR 7,0,1
70 ON ERROR GOTO 100
80 GOTO 130
90 '
100 IF ERR=1 AND ERL=1480 THEN COLOR 7,0:RESUME 1800
110 BEEP:PRINT "Error";ERR;;"in line";ERL:END
120 '
130 PI=3.141592
140 UL$=STRING$(80,205)
150 L$="#####.###"
160 V$="####.###"
170 X$=STRING$(80,32)
180 DIM C$(40,9)    'coax data
190 '
200 '.....read data
210 OPEN"I",1,"\hamcalc\coaxfile\coaxial.fil"
220 N=40    'no.of data groups
230  FOR Z=1 TO N
240   FOR Y=1 TO 9
250   INPUT #1, C$(Z,Y)
260   NEXT Y
270  NEXT Z
280 CLOSE
290 '
300 '.....start
310 CLS
320 B=0:C=0:E=0:I=0:O=0:U=0:V=0:ZO=0
330 COLOR 15,2
340 PRINT " STUBS - COAXIAL TRANSMISSION LINES";
350 PRINT TAB(57);"by George Murphy VE3ERP ";
360 COLOR 1,0:PRINT STRING$(80,223);
370 COLOR 7,0
380 GOSUB 2300
390 PRINT UL$;
400 PRINT " Press number in < > to choose standard units of measure:"
410 PRINT UL$;
420 PRINT "   < 1 >  Metric"
430 PRINT "   < 2 >  U.S.A./Imperial"
440 PRINT UL$;
450 PRINT "     or Press < 0 > to EXIT....."
460 Z$=INKEY$
470 IF Z$="0"THEN CLS:CHAIN GO$
480 IF Z$="1"THEN UM=3.048000E-01:UM$="m.":GOTO 520
490 IF Z$="2"THEN UM=1:UM$="ft.":GOTO 520
500 GOTO 460
510 '
520 '.....transmission line stubs
530 CLS
540 LOCATE 1:COLOR 0,7,0:T=1
550 LOCATE ,T:PRINT "           COAXIAL TRANSMISSION LINE STUBS           "
560 LOCATE ,T:PRINT "   ∫ ≥Æ◊ƒƒƒƒƒ Lg ƒƒƒƒƒØ≥     ∫ ≥Æ◊ƒƒƒƒƒ Lg ƒƒƒƒƒØ≥   "
570 LOCATE ,T:PRINT "   ∫ ≥ »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ∏     ∫ ≥ »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ∏   "
580 LOCATE ,T:PRINT " ⁄ƒ◊Ø√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ   ⁄ƒ◊Ø√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø  "
590 LOCATE ,T:PRINT " Z ∫ ≥ …ÕÕÕÕshieldÕÕÕÕÕæ   Z ∫ ≥ …ÕÕÕÕshieldÕÕÕÕ—æ≥  "
600 LOCATE ,T:PRINT "   ∫ ≥Æ◊ƒline                ∫ ≥Æ◊ƒline         ¿ƒŸ  "
610 LOCATE ,T:PRINT "                                                     "
620 LOCATE ,T:PRINT " CAPACITIVEE (open) STUB   INDUCTIVE (shorted) STUB  "
630 LOCATE ,T:PRINT "                                                     "
640 LOCATE ,T:PRINT "     (ref. 1996 ARRL HANDBOOK, pp.16.21 & 16.22)     "
650 COLOR 7,0
660 T=55:LOCATE 3
670 LOCATE ,T:PRINT "Transmission line stubs  "
680 LOCATE ,T:PRINT "should extend away from  "
690 LOCATE ,T:PRINT "the line at an angle as  "
700 LOCATE ,T:PRINT "close to 90¯ as possible."
710 LOCATE 10:PRINT UL$;
720 GOSUB 750
730 GOTO 840
740 '
750 '.....cable table
760 FOR Z=1 TO 10
770 PRINT TAB(1)USING "##:";Z;:PRINT C$(Z,1);
780 PRINT TAB(21)USING "##:";Z+10;:PRINT C$(Z+10,1);
790 PRINT TAB(41)USING "##:";Z+20;:PRINT C$(Z+20,1);
800 PRINT TAB(61)USING "##:";Z+30;:PRINT C$(Z+30,1)
810 NEXT Z
820 PRINT TAB(27);"* denotes foam dielectric"
830 RETURN
840 PRINT UL$;
850 '
860 INPUT " ENTER: An item number from above list to select coaxial cable";N
870 IF N>=1 AND N<=40 THEN 890
880 BEEP:LOCATE CSRLIN-1:PRINT X$;:LOCATE CSRLIN-1:GOTO 860
890 VIEW PRINT 11 TO 24:CLS:VIEW PRINT
900 FOR Z=1 TO 9:LOCATE Z,55:PRINT STRING$(26,32);:NEXT Z
910 T$=C$(N,1)
920 ZIN=VAL(C$(N,5))
930 E=VAL(C$(N,3))
940 VF=1/SQR(E)
950 COLOR 15,4
960 LOCATE 1,54:PRINT " ";T$;;" COAX CABLE "
970 COLOR 7,0
980 LOCATE ,55:PRINT "Impedance Z (ohms) =";USING "###.#";ZIN
990 LOCATE ,55:PRINT "Inner wire dia.,in.=";USING " .###";VAL(C$(N,8))
1000 LOCATE ,55:PRINT "Dielectric constant=";USING "##.##";E
1010 LOCATE ,55:PRINT "Velocity factor....=";USING " .###";VF
1020 LOCATE ,55:PRINT "XL= Inductive Reactance";
1030 LOCATE ,55:PRINT " L= Inductance"
1040 LOCATE ,55:PRINT "XC= Capacitive Reactance"
1050 LOCATE ,55:PRINT " C= Capacitance"
1060 LOCATE 11
1070 '
1080 INPUT " ENTER: Frequency in MHz......";FO
1090 COLOR 7,0
1100 VIEW PRINT 23 TO 24:CLS:VIEW PRINT:LOCATE 11
1110 WL=300/FO          'wavelength in free space
1120 W=300/(SQR(E)*FO)  'electrical wavelength
1130 '
1140 T=9
1150 PRINT TAB(T);"Frequency.......................";USING V$;FO;
1160 PRINT " MHz    =";
1170 PRINT USING V$;300/FO;:PRINT ;" metres"
1180 PRINT TAB(T);"Electrical wavelength.........w=";USING V$;W;
1190 PRINT " metres =";USING V$;W/3.048000E-01;:PRINT ;" feet"
1200 PRINT TAB(T);"Electrical ¨-wavelength.....w/4=";USING V$;W/4;
1210 PRINT " metres =";USING V$;W/4/3.048000E-01;:PRINT ;" feet"
1220 PRINT UL$;
1230 PRINT " TYPICAL VALUES:";TAB(38);"INDUCTIVE STUB";
1240 PRINT TAB(58);"CAPACITIVE STUB"
1250 PRINT TAB(9);"Lg/w";TAB(16);"Lg(m.)";TAB(25);"Lg(ft)";
1260 PRINT TAB(38);;"XL(Í)";TAB(48);"L(ÊH)";TAB(58);"XC(Í)";TAB(68);"C(pF)"
1270 '
1280 K=-5.000000E-02
1290 FOR Z=1 TO 7
1300 IF Z=4 OR Z=5 THEN K=K+2.500000E-02:GOTO 1320
1310 K=K+5.000000E-02
1320 LG=K*W
1330 '
1340 IF K=0 THEN XL=0:XC=10^6:GOTO 1390
1350 IF K=2.500000E-01 THEN XC=0:XL=10^6:GOTO 1390
1360 XC=ZIN/TAN(2*PI*LG/W):      C=10^6/(2*PI*FO*XC)
1370 XL=ZIN/(1/TAN(2*PI*LG/W)):  L=XL/(2*PI*FO)
1380 '
1390 PRINT TAB(9)USING ".###";K;
1400 PRINT TAB(13);USING L$;LG;
1410 PRINT TAB(22);USING L$;LG/3.048000E-01;
1420 IF K<=0   THEN PRINT TAB(38);"- 0 -     - 0 -     - Ï -     - 0 -";:GOTO 1480
1430 IF K>=2.500000E-01 THEN PRINT TAB(38);"- Ï -     - Ï -     - 0 -     - Ï -";:GOTO 1480
1440 PRINT TAB(34)USING L$;XL;
1450 PRINT TAB(44)USING L$;L;
1460 PRINT TAB(54)USING L$;XC;
1470 PRINT TAB(64)USING L$;C;
1480 NEXT Z
1490 GOSUB 2530   'hardcopy
1500 '
1510 '.....sub-menu
1520 COLOR 7,0
1530 LOCATE 25,1:PRINT X$;
1540 LOCATE 24,3:COLOR 15,2
1550 PRINT " Press a number for a specific: 1-Frequency, 2-Length, ";
1560 PRINT "3-Inductance,        ";
1570 LOCATE 25,3
1580 PRINT " 4-Capacitance, 5-Inductive Reactance, 6-Capacitive Reactance,";
1590 PRINT " or 7 to QUIT ";
1600 '
1610 COLOR 7,0
1620 Z$=INKEY$:IF Z$=""THEN 1620
1630 IF VAL(Z$)<1 OR VAL(Z$)>7 THEN 1620
1640 LOCATE 25,1:PRINT X$;
1650 IF Z$="1"THEN 1820
1660 IF Z$="2"THEN 1730
1670 IF Z$="3"THEN 1860
1680 IF Z$="4"THEN 2070
1690 IF Z$="5"THEN 1970
1700 IF Z$="6"THEN 2190
1710 IF Z$="7"THEN 2500
1720 '
1730 '.....length
1740 COLOR 7,0:VIEW PRINT 23 TO 24:CLS:VIEW PRINT:LOCATE 23
1750 COLOR 0,7:PRINT " ENTER: Specified length....(";UM$;;")";:INPUT LG
1760 IF UM=1 THEN LG=LG*3.048000E-01
1770 K=LG/W
1780 LOCATE CSRLIN-1:PRINT X$;
1790 LOCATE CSRLIN-1,8:GOTO 1340
1800 GOSUB 2530:GOTO 1510
1810 '
1820 '.....frequency
1830 COLOR 7,0:VIEW PRINT 23 TO 24:CLS:VIEW PRINT:LOCATE 23
1840 COLOR 0,7:GOTO 1080
1850 '
1860 '.....inductance
1870 COLOR 7,0:VIEW PRINT 23 TO 24:CLS:VIEW PRINT:LOCATE 23
1880 COLOR 0,7:LOCATE 23
1890 INPUT " ENTER: Inductance (ÊH)";L
1900 XL=L*2*PI*FO
1910 LG=W*ATN(XL/ZIN)/(2*PI)
1920 K=LG/W
1930 LOCATE CSRLIN-1:PRINT X$;
1940 LOCATE CSRLIN-1,8:GOTO 1340
1950 GOSUB 2530:GOTO 1510
1960 '
1970 '.....inductive reactance
1980 COLOR 7,0:VIEW PRINT 23 TO 24:CLS:VIEW PRINT:LOCATE 23
1990 COLOR 0,7:LOCATE 23
2000 INPUT " ENTER: Inductance Reactance XL (ohms)";XL
2010 LG=W*ATN(XL/ZIN)/(2*PI)
2020 K=LG/W
2030 LOCATE CSRLIN-1:PRINT X$;
2040 LOCATE CSRLIN-1,8:GOTO 1340
2050 GOSUB 2530:GOTO 1510
2060 '
2070 '.....capacitance
2080 COLOR 7,0:VIEW PRINT 23 TO 24:CLS:VIEW PRINT:LOCATE 23
2090 COLOR 0,7:LOCATE 23
2100 INPUT " ENTER: Capacitance (pF)";C
2110 IF C<=0 THEN LG=0:K=0:GOTO 2150
2120 XC=10^6/(2*PI*FO*C)
2130 LG=W*ATN(ZIN/XC)/(2*PI)
2140 K=LG/W
2150 LOCATE CSRLIN-1:PRINT X$;
2160 LOCATE CSRLIN-1,8:GOTO 1340
2170 GOSUB 2530:GOTO 1510
2180 '
2190 '.....capacitive reactance
2200 COLOR 7,0:VIEW PRINT 23 TO 24:CLS:VIEW PRINT:LOCATE 23
2210 COLOR 0,7:LOCATE 23
2220 INPUT " ENTER: Capacitance Reactance XC (ohms)";XC
2230 IF XC<=0 THEN K=2.500000E-01:LG=W/4:GOTO 2260
2240 LG=W*ATN(ZIN/XC)/(2*PI)
2250 K=LG/W
2260 LOCATE CSRLIN-1:PRINT X$;
2270 LOCATE CSRLIN-1,8:GOTO 1340
2280 GOSUB 2530:GOTO 1510
2290 '
2300 '.....diagram
2310 J=14
2320 COLOR 0,7
2330 LOCATE ,J:PRINT "        ⁄ƒƒƒƒƒƒƒƒƒø                 ⁄ƒƒƒƒƒƒƒƒƒø       "
2340 LOCATE ,J:PRINT " ÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕ    ÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕ "
2350 LOCATE ,J:PRINT " ƒƒƒƒlineƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒ    ƒƒƒƒlineƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒ "
2360 LOCATE ,J:PRINT " ÕÕÕÕÕÕÕ—ÕÕª ≥ …ÕÕ—ÕÕÕÕÕÕ    ÕÕÕÕÕÕÕ—ÕÕª ≥ …ÕÕ—ÕÕÕÕÕÕ "
2370 LOCATE ,J:PRINT "        ¿ƒø∫ ≥ ∫⁄ƒŸÆƒtee connector-Ø¿ƒø∫ ≥ ∫⁄ƒŸ       "
2380 LOCATE ,J:PRINT "          ¿∂ ≥ «Ÿ                     ¿∂ ≥ «Ÿ         "
2390 LOCATE ,J:PRINT "           ∫ ≥ ∫Æƒƒƒƒƒƒ shield ƒƒƒƒƒƒƒØ∫ ≥ ∫          "
2400 LOCATE ,J:PRINT "           ∫ ≥ ∫                       ∫ ≥ ∫          "
2410 LOCATE ,J:PRINT "           ” ¡ Ω                      ⁄– ≥ Ω          "
2420 LOCATE ,J:PRINT "                                      ¿ƒƒŸ            "
2430 LOCATE ,J:PRINT "         OPEN STUB                 SHORTED STUB       "
2440 LOCATE ,J:PRINT "                                                      "
2450 COLOR 7,0
2460 RETURN
2470 '
2480 '.....end
2490 GOSUB 2530
2500 GOTO 300   'start
2510 END
2520 '
2530 'PRT
2540 KEY OFF:GOSUB 2610:LOCATE 25,5:COLOR 0,2
2550 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2560 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2570 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2570 ELSE GOSUB 2610
2580 IF Z$="3"THEN RETURN
2590 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2600 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2540
2610 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

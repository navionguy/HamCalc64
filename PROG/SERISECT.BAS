1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$,FRQ,ZP   'chained from ocfdipol.bas - off-centre-fed dipole
10 'SERISECT - Series Section Matching Transformer - 31 JAN 94 rev. 14 AUG 01
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 PROG$="serisect"
60 
70 UL$=STRING$(80,205)
80 ER$=STRING$(80,32)
90 U1$="####.###"
100 U2$="####.##"
110 Q$=CHR$(34)
120 UM=3.048000E-01:UM$=" metres "
130 PI=3.141592
140 DIM C$(51,9)   'coax specs.
150 '
160 '.....start
170 OPEN"I",1,"\hamcalc\coaxfile\coaxial.fil"
180 FOR Z=1 TO 47:FOR Y=1 TO 9
190 INPUT #1,Z$
200 C$(Z,Y)=Z$
210 NEXT Y:NEXT Z
220 FOR Z=48 TO 51:FOR Y=1 TO 7
230 INPUT #1,Z$
240 C$(Z,Y)=Z$
250 NEXT Y:NEXT Z
260 CLOSE
270 '.....compile transmission line data base
280 FOR Z=1 TO 40
290 C$(Z,2)=C$(Z,5)                     'char.impedance
300 C$(Z,3)=STR$(1/SQR(VAL(C$(Z,3))))   'velocity factor
310 NEXT Z
320 FOR Z=41 TO 44
330 C$(Z,1)=C$(Z+7,1):C$(Z,2)=C$(Z+7,2):C$(Z,3)=STR$(VAL(C$(Z+7,3))/100)
340 NEXT Z
350 '
360 '.....start
370 COLOR 7,0,0:CLS:COLOR 15,2,1
380 PRINT " SERIES-SECTION MATCHING TRANSFORMER";
390 PRINT TAB(57);;"by George Murphy VE3ERP ";
400 COLOR 1,0:PRINT STRING$(80,223);
410 COLOR 7,0
420 T=20:GOSUB 2640    'print diagram 1
430 GOSUB 2930   'preface
440 COLOR 0,7
450 LOCATE ,28:PRINT "  Press number in ( ) to:  ";:COLOR 7,0
460 PRINT TAB(18);"(1)  RUN program"
470 PRINT TAB(18);"(2)  REVIEW coaxial cable characteristics"
480 PRINT TAB(18);"(0)  EXIT";
490 LOCATE 25,14:COLOR 0,7
500 PRINT " (ref. The ARRL ANTENNA BOOK, 17th edition, p. 26-15) ";:COLOR 7,0
510 Z$=INKEY$:IF Z$=""THEN 510
520 IF Z$="1"THEN 570
530 IF Z$="2"THEN CLS:CHAIN "coaxchar"
540 IF Z$="0"THEN CLS:RUN EX$
550 GOTO 510
560 '
570 '.....select main feed line
580 CLS:COLOR 7,0,0
590 GOSUB 2260      'print data base
600 LN=CSRLIN
610 COLOR 15,4:LOCATE 23
620 COLOR 15,2
630 '
640 INPUT " ENTER: No. from above table to select type of MAIN FEED LINE ";MF
650 COLOR 7,0
660 IF MF>=1 AND MF<=48 THEN 680
670 BEEP:LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1:GOTO 620
680 MF$=C$(MF,1)        'name of main feed line
690 Z0=VAL(C$(MF,2))    'impedance of main feed line
700 VF1=VAL(C$(MF,3))   'vel.factor of main feed line
710 '
720 '.....select matching section
730 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
740 COLOR 15,4:LOCATE ,32:PRINT " MATCHING SECTION ";:COLOR 7,0
750 COLOR 15,2:LOCATE 22
760 PRINT " ";MF$;;" was selected for MAIN FEED LINE "
770 COLOR 7,0:LOCATE 23
780 '
790 COLOR 0,7
800 PRINT " Want a (s)ingle line or (p)arallel line matching section? (s/p) "
810 COLOR 7,0
820 L$=INKEY$:IF L$=""THEN 820
830 IF L$="s"OR L$="p"THEN 850
840 GOTO 820
850 COLOR 15,4:LOCATE 23
860 '
870 INPUT " ENTER: No. from above table to select type of MATCHING SECTION ";MS
880 COLOR 7,0
890 IF MS>=1 AND MS<=48 THEN 910
900 BEEP:LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1:GOTO 850
910 MS$=C$(MS,1)       'name of matching section
920 Z1=VAL(C$(MS,2))   'impedance of matching section
930 IF L$="p"THEN Z1=Z1/2
940 VF2=VAL(C$(MS,3))  'vel.factor of matching section
950 '
960 CLS:COLOR 0,7:LOCATE ,27:PRINT " SERIES-SECTION TRANSFORMER "
970 COLOR 7,0:PRINT UL$;
980 '
990 F=0:PRINT " ENTER: Frequency of operation............(MHz)";
1000 IF FRQ THEN F=FRQ:PRINT "";:GOTO 1030
1010 INPUT F
1020 IF F=0 THEN LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1:GOTO 990
1030 WFT=984/F          'wavelength in feet
1040 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1050 PRINT " Frequency of operation.................";USING U1$;F;:PRINT " MHz"
1060 LN=CSRLIN
1070 IF ZP THEN RL=ZP:PRINT "";:GOTO 1110
1080 RL=0:PRINT " ENTER: Antenna load, resistive component......(ohms)";
1090 INPUT RL
1100 IF RL=0 THEN LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1:GOTO 1080
1110 LOCATE CSRLIN-1:PRINT STRING$(7,32)
1120 IF ZP THEN Z$="n":GOTO 1190
1130 COLOR 0,7
1140 PRINT " Is the reactive component (i)nductive, (c)apacitive or (n)il?";
1150 PRINT "   (i/c/n)"
1160 COLOR 7,0:Z$=INKEY$:IF Z$=""THEN 1160
1170 IF Z$="i"THEN X$="inductive component..":XS=1:XS$="+j":GOTO 1210
1180 IF Z$="c"THEN X$="capacitive component.":XS=-1:XS$="-j":GOTO 1210
1190 IF Z$="n"THEN XS=1:XS$="-j":XL=0:Z=0:GOTO 1230
1200 VIEW PRINT LN+1 TO 24:CLS:VIEW PRINT:LOCATE LN+1:GOTO 1130
1210 Z=0:PRINT " ENTER: Value of ";X$;;".....(ohms)";:INPUT Z
1220 XL=ABS(Z)*XS
1230 ANT$=STR$(RL)+XS$+RIGHT$(STR$(Z),LEN(STR$(Z))-1)+" ê"
1240 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
1250 PRINT " Impedance of antenna................... ";ANT$
1260 '
1270 PRINT " Impedance of main feed line............";USING U2$;Z0;
1280 PRINT " ê ";MF$;:LOCATE ,69:PRINT "Vf= ";USING "#.###";VF1
1290 'PRINT ""
1300 '
1310 PRINT " Impedance of matching section..........";USING U2$;Z1;
1320 PRINT " ê ";MS$;:LOCATE ,69:PRINT "Vf= ";USING "#.###";VF2
1330 '
1340 '.....normalize impedances
1350 N=Z1/Z0
1360 R=RL/Z0
1370 X=XL/Z0
1380 P=SQR(((RL-Z0)^2+XL^2)/((RL+Z0)^2+XL^2))
1390 SWR=(1+P)/(1-P):IF SWR<1 THEN SWR=R
1400 PRINT " SWR....................................";USING "###.##";SWR;
1410 PRINT ":1"
1420 '
1430 '.....calculation
1440 LOZ=Z0/SQR(SWR)
1450 HIZ=Z0*SQR(SWR)
1460 IF Z1<LOZ OR Z1>HIZ THEN 1630
1470 '
1480 '.....inappropriate impedances
1490 PRINT:BEEP
1500 COLOR 14,4
1510 LOCATE CSRLIN,8
1520 PRINT " Matching section impedance must be either more than";USING U1$;HIZ;
1530 PRINT " ê  "
1540 LOCATE CSRLIN,8
1550 PRINT "                                        or less than";USING U1$;LOZ;
1560 PRINT " ê  "
1570 COLOR 7,0
1580 PRINT
1590 PRINT TAB(9);;"Press any key to start over........."
1600 IF INKEY$=""THEN 1600
1610 GOTO 160   'start
1620 '
1630 IF RL=Z0 THEN L1=0:L2=0:GOTO 1790
1640 EQ1=(R-1)^2+X^2
1650 EQ2=R*(N-1/N)^2
1660 EQ3=(R-1)^2-X^2
1670 '
1680 B=SQR(EQ1/(EQ2-EQ3))
1690 IF SGN(B)=-1 THEN B=B*-1
1700  L2=ATN(B)*180/PI
1710 EQ4=(N-R/N)*B+X
1720 EQ5=R+X*N*B-1
1730 '
1740 A=EQ4/EQ5
1750  L1=ATN(A)*180/PI
1760  IF SGN(L1)=-1 THEN L1=L1+180
1770 '
1780 W=WFT*VF1:L1FT=L1*W/360
1790 W=WFT*VF2:L2FT=L2*W/360
1800 DIF=LEN(C$(MS,1))-LEN(C$(MF,1))
1810 '
1820 PRINT " Line from antenna to matching section..";USING U1$;L1;
1830 PRINT "ø length of ";MF$;
1840 IF SGN(DIF)=1 THEN PRINT STRING$(DIF+1," ");ELSE PRINT ;" "
1850 '
1860 PRINT " Matching section.......................";USING U1$;L2;
1870 PRINT "ø length of ";MS$;
1880 IF SGN(DIF)=-1 THEN PRINT STRING$(ABS(DIF)+1," "):ELSE PRINT " "
1890 PRINT UL$;
1900 '
1910 T=2:LOCATE 11:GOSUB 2750   'print diagram 2
1920 COLOR 0,7
1930 LOCATE 13,T+7:PRINT USING "###.#";Z0;:PRINT " ê"
1940 LOCATE 17,T+2:PRINT USING "###.#";Z1;:PRINT " ê"
1950 LOCATE 21,T+7:PRINT USING "###.#";Z0;:PRINT " ê"
1960 '
1970 COLOR 0,7
1980 LOCATE 13,T+26:PRINT MF$
1990 COLOR 7,0
2000 LOCATE 12,T+45:PRINT "Ú";USING " ####.###ø";L1
2010 LOCATE 13,T+43:PRINT "®Ä´";USING "=####.### m.";L1FT*3.048000E-01
2020 LOCATE 14,T+45:PRINT "À";USING "=####.### ft.";L1FT
2030 '
2040 COLOR 0,7
2050 LOCATE 17,T+30:PRINT MS$
2060 IF L$="s"THEN 2110
2070  LOCATE 18,T+22:PRINT "2 in parallel"
2080  LOCATE 16,T+19:PRINT "Ú¿"
2090  LOCATE 17,T+19:PRINT "³³"
2100  LOCATE 18,T+19:PRINT "ÀÙ"
2110 COLOR 7,0
2120 LOCATE 16,T+45:PRINT "Ú";USING " ####.###ø";L2
2130 LOCATE 17,T+43:PRINT "®Ä´";USING "=####.### m.";L2FT*3.048000E-01
2140 LOCATE 18,T+45:PRINT "À";USING "=####.### ft.";L2FT
2150 '
2160 COLOR 0,7
2170 LOCATE 21,T+26:PRINT MF$
2180 COLOR 7,0
2190 LOCATE 24:PRINT UL$;
2200 '
2210 '.....end
2220 GOSUB 3210
2230 GOTO 360   'start
2240 END
2250 '
2260 '.....print coax list
2270 CLS
2280 PRINT "No.  Type       Zo(ê) Vf ³";
2290 PRINT "No.  Type        Zo(ê) Vf ³";
2300 PRINT "No.  Type        Zo(ê) Vf "
2310 '
2320    FOR Z=1 TO 15
2330 IF RIGHT$(C$(Z,1),1)="*" THEN COLOR 4,0 ELSE COLOR 7,0
2340 Z$=STR$(Z):IF LEN(Z$)<3 THEN Z$=" "+Z$
2350 Z$=RIGHT$(Z$,2)+":":PRINT Z$;
2360 PRINT TAB(4);C$(Z,1);TAB(16);USING "###.#";VAL(C$(Z,2));
2370 PRINT TAB(22);USING ".###";VAL(C$(Z,3));
2380 COLOR 7,0
2390 PRINT TAB(26);;"³";
2400 '
2410 IF RIGHT$(C$(Z+15,1),1)="*" THEN COLOR 4,0 ELSE COLOR 7,0
2420 Z$=STR$(Z+15)
2430 Z$=RIGHT$(Z$,2)+":":PRINT Z$;
2440 PRINT TAB(30);C$(Z+15,1);TAB(43);USING "###.#";VAL(C$(Z+15,2));
2450 PRINT TAB(49);USING ".###";VAL(C$(Z+15,3));
2460 COLOR 7,0
2470 PRINT TAB(53);;"³";
2480 IF Z>14 THEN PRINT "";:GOTO 2570
2490 '
2500 IF RIGHT$(C$(Z+30,1),1)="*" THEN COLOR 4,0 ELSE COLOR 7,0
2510 Z$=STR$(Z+30)
2520 Z$=RIGHT$(Z$,2)+":":PRINT Z$;
2530 PRINT TAB(57);C$(Z+30,1);
2540 IF Z+30<40 THEN V$="##.#"ELSE V$="###"
2550 PRINT TAB(71);USING V$;VAL(C$(Z+30,2));
2560 PRINT TAB(76);USING ".###";VAL(C$(Z+30,3))
2570    NEXT Z
2580 '
2590 COLOR 0,7:LOCATE ,10
2600 PRINT " * denotes foam type - DO NOT USE         Vf=Velocity Factor "
2610 COLOR 7,0
2620 RETURN
2630 '
2640 '.....diagram 1
2650 COLOR 0,7
2660 LOCATE ,T:PRINT "             matching section       load "
2670 LOCATE ,T:PRINT "                ÚÄÄÄÄÄÄÄÄ¿          ÚÄÄ¿ "
2680 LOCATE ,T:PRINT " ÍÍÍÍÍÍÍZ0ÍÍÍÍÍÍµ   Z1   ÆÍÍÍÍZ0ÍÍÍÍµZL³ "
2690 LOCATE ,T:PRINT "    main line   ÀÄÄÄÄÄÄÄÄÙ main lineÀÄÄÙ "
2700 LOCATE ,T:PRINT "                ³®Ä L2 Ä¯³®ÄÄ L1 ÄÄ¯³    "
2710 LOCATE ,T:PRINT "                                         "
2720 COLOR 7,0
2730 RETURN
2740 '
2750 '......diagram 2
2760 COLOR 0,7
2770 LOCATE CSRLIN,T:PRINT "           antenna ÂÂ feed point           "
2780 LOCATE CSRLIN,T:PRINT "                   ³³                      "
2790 LOCATE CSRLIN,T:PRINT "               Main³³Line:                 "
2800 LOCATE CSRLIN,T:PRINT "                   ³³                      "
2810 LOCATE CSRLIN,T:PRINT "                  ÚÙÀ¿                     "
2820 LOCATE CSRLIN,T:PRINT "                  ³  ³                     "
2830 LOCATE CSRLIN,T:PRINT "          Matching³  ³Section:             "
2840 LOCATE CSRLIN,T:PRINT "                  ³  ³                     "
2850 LOCATE CSRLIN,T:PRINT "                  À¿ÚÙ                     "
2860 LOCATE CSRLIN,T:PRINT "                   ³³                      "
2870 LOCATE CSRLIN,T:PRINT "               Main³³Line:                 "
2880 LOCATE CSRLIN,T:PRINT "                   ³³ (any length)         "
2890 LOCATE CSRLIN,T:PRINT "              from station                 "
2900 COLOR 7,0
2910 RETURN
2920 '
2930 '.....preface
2940 TB=8
2950 PRINT TAB(TB);
2960 PRINT "  The series-section transformer has the following advantages over";
2970 PRINT TAB(TB);
2980 PRINT "stub tuning or the ¬-wave transformer:"
2990 PRINT TAB(TB);
3000 PRINT " - The matching section need not be located exactly at the load."
3010 PRINT TAB(TB);
3020 PRINT " - The matching section may be less than ¬-wavelength long."
3030 PRINT TAB(TB);
3040 PRINT " - The matching section can have ANY impedance not too close to"
3050 PRINT TAB(TB);
3060 PRINT "   that of the main line (the program rejects too-close choices)."
3070 PRINT TAB(TB);
3080 PRINT "  The matching section may be a single length of line, or two equal"
3090 PRINT TAB(TB);
3100 PRINT "parallel lengths to provide a net characteristic impedance one-half"
3110 PRINT TAB(TB);
3120 PRINT "that of a single length."
3130 PRINT TAB(TB);
3140 PRINT "  Avoid using foam type cable. Inherent manufacturing limitations"
3150 PRINT TAB(TB);
3160 PRINT "can cause significant variance in electrical properties between"
3170 PRINT TAB(TB);
3180 PRINT "different production runs, and between different manufacturers."
3190 RETURN
3200 '
3210 'PRT
3220 KEY OFF:GOSUB 3290:LOCATE 25,5:COLOR 0,2
3230 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
3240 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
3250 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 3250 ELSE GOSUB 3290
3260 IF Z$="3"THEN RETURN
3270 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
3280 IF Z$="2"THEN LPRINT CHR$(12) ELSE 3220
3290 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

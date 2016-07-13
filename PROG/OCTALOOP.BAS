1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'OCTALOOP - Subminiature Loop Antenna - 09 DEC 02 rev.20 SEP 05
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PI=4*ATN(1)  '3.141593
70 LF=1/LOG(10)  'factor to convert natural log to base 10
80 UL$=STRING$(80,205)
90 DIM C$(35,9)
100 DIM J(35,3)
110 '
120 '.....title page
130 CLS
140 COLOR 15,2,1
150 PRINT " OCTALOOP Subminiature Loop Antenna";
160 PRINT TAB(57);"by George Murphy VE3ERP ";
170 COLOR 1,0:PRINT STRING$(80,223);
180 COLOR 7,0
190 GOSUB 2050  'diagram
200 LN=CSRLIN
210 PRINT
220 GOSUB 2310  'preamble
230 COLOR 0,3:LOCATE 25,23:PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
240 Z$=INKEY$:IF Z$=""THEN 240
250 IF Z$="0"THEN COLOR 7,0,0:CLS:CHAIN GO$
260 IF Z$="1"THEN 290
270 GOTO 240
280 '
290 '.....data input
300 COLOR 7,0,0:GOSUB 3190
310 PRINT:LN=CSRLIN
320 PRINT "The OCTALOOP design is similar to the popular W5QJR Hart loop. It i";
330 PRINT "s smaller   "
340 PRINT "and therefore less efficient, but performs well where space is rest";
350 PRINT "ricted. If  "
360 PRINT "you want the most efficient loop design, go for the W5QJR Hart loop";
370 PRINT ", but if you"
380 PRINT "don't have the space, try the W2AHW OCTALOOP!                      "
390 PRINT
400 PRINT "Because of the narrow bandwidth of a loop antenna, it should be des";
410 PRINT "igned for a "
420 PRINT "specific operating frequency of interest rather than a band edge.  "
430 PRINT
440 PRINT "If a capacitor is used at (3) to replace the stub, it should be a h";
450 PRINT "igh voltage "
460 PRINT "split stator type."
470 '
480 PRINT :LN=CSRLIN-1
490 INPUT "ENTER: Frequency (MHz)";F
500 WL=300/F   'wavelength in metres
510 LN=CSRLIN-1:GOSUB 3190
520 INPUT "ENTER: Transmitter output (watts)";PWR
530 GOSUB 3190   'erase to bottom of screen
540 COLOR 0,7
550 LN=20:GOSUB 3190
560 PRINT "  Very high voltages appear at the apex gap (3) in the loop. This v";
570 PRINT "oltage is a "
580 PRINT " function of the RF field strength in volts per metre (ARRL Antenna";
590 PRINT " Book, 18th "
600 PRINT " edition, page 5-3). At the design stage this value is unknown so s";
610 PRINT "elect a     "
620 PRINT " cable with a high voltage breakdown point as your initial choice. "
630 COLOR 7,0
640 COLOR 0,3:LOCATE 25,26:PRINT " Press any key to continue ";:COLOR 7,0
650 IF INKEY$=""THEN 650
660 CLS:GOSUB 2610   'coax cable list
670 '
680 CLS:COLOR 7,0,0
690 PRINT "Due to manufacturing tolerances of coaxial cable, the outside diam";
700 PRINT "eter and     "
710 PRINT "capacity-per-foot may vary from the published values, so it is adv";
720 PRINT "isable to    "
730 PRINT "measure these values in a sample of actual cable you will use. If ";
740 PRINT "you do not   "
750 PRINT "know these values, the program will calculate them and the voltage";
760 PRINT " breakdown   "
770 PRINT "point from published data.:";
780 PRINT
790 PRINT "If the capacity and braid data are unknown, it is assumed the brea";
800 PRINT "kdown voltage"
810 PRINT "is also unknown."
820 PRINT
830 PRINT
831 PRINT "Physical dimensions may vary between manufacturers so it suggested";
832 PRINT "Physical dimensions may vary between manufacturers so it suggested";
833 PRINT "that you measure the braid O.D. of the cable used."
840 PRINT
850 IF FLAG THEN 910
860 PRINT USING "Want to alter the braid O.D. listed as #.### in.?  (y/n)";D
870 Z$=INKEY$:IF Z$=""THEN 870
880 IF Z$="n"THEN 920
890 IF Z$="y"THEN 910
900 GOTO 870
910 INPUT "ENTER: Actual measured O.D.(inches)";D
920 PRINT USING "#.### in. confirmed";D
930 PRINT
940 IF FLAG THEN 1000
950 PRINT USING "Want to alter the pF/ft. listed as ##.## pF?  (y/n)";CFT
960 Z$=INKEY$:IF Z$=""THEN 960
970 IF Z$="n"THEN 1010
980 IF Z$="y"THEN 1000
990 GOTO 960
1000 INPUT "ENTER: Actual measured capacitance per foot (pF)";CFT
1010 PRINT USING "###.## pF/ft. confirmed";CFT
1020 PRINT :PRINT "Press any key . . . . ."
1030 IF INKEY$=""THEN 1030
1040 GOTO 1050
1050 CLS:COLOR 0,3,0:LOCATE 1,23
1060 PRINT USING " W2AHW OCTALOOP for ###.### metres ";WL:COLOR 7,0
1070 PRINT UL$;
1080 LOCATE 3:GOSUB 2050    'standard coax specs
1090 COLOR 0,7
1100 LOCATE 3,26:PRINT LOOP$
1110 COLOR 7,0
1120 '
1130 P=0:N=10              'seed factors
1140 '.....iteration
1150 P=P+N
1160 SIDE=P/8              'length of one side
1170 A=SIDE/TAN(2.250000E+01*PI/180)
1180 '
1190 '.....calculate loop size and capacitance
1200 B=A*TAN(2.250000E+01*PI/180)  'length of one side in inches
1210 P=8*B                 'loop perimeter in inches
1220 CLOOP=P/12*CFT        'loop capacitance
1230 BCM=B*2.539999            'side length in centimeters
1240 DCM=D*2.539999            'conductor diameter in centimeters
1250 '
1260 '.....ARRL Antenna Book, 18th edition, page 5-4, Table 1
1270 E1=1.600000E-02*BCM
1280 E2=2.612999*BCM/2^DCM
1290 E3=7.152999E-02*2^DCM/BCM
1300 LUH=E1*(LOG(E2)+7.514299E-01+E3)  'inductance of octagonal loop
1310 '
1320 CRES=2.533028E+04/F^2/LUH 'capacitance to resonate
1330 CABL=CRES/CFT*12      'cable length in inches
1340 STUB=SQR(B^2/2)       'stub length in inches
1350 REQD=P+STUB           'required cable length
1360 IF ABS(CABL-REQD)<1.000000E-03 THEN 1400
1370 IF CABL<REQD THEN P=P-N:N=N/10
1380 GOTO 1150
1390 '
1400 XL=10^6/(2*PI*F*CRES) 'reactance
1410 PW=P/3.938000E+01/WL         'perimeter in wavelengths
1420 CSTUB=CRES-CLOOP
1430 STUB=CSTUB/CFT*12     'stub length in inches
1440 '
1450 '.....display
1460 LN=11:GOSUB 3190
1470 PRINT STRING$(80,196);
1480 LOCATE CSRLIN-1,41:PRINT "Â"
1490 PRINT USING " Resonant Frequency........ ####.### MHz³";F;
1500 PRINT USING "Reactance at resonance.... ####.### ê  ";XL
1510 PRINT USING " Length of each side.....B= ####.### in.³";B;
1520 PRINT USING "Perimeter: #.#### wavelgth ####.### in.";PW,P
1530 PRINT USING " Distance between sides..A= ####.### in.³";A;
1540 PRINT USING "Inductance of loop........ ####.### æH.";LUH
1550 PRINT USING " O.D. of coax braid........ ####.### in.³";D;
1560 PRINT USING "Coax cap'y per foot....... ####.##  pF.";CFT
1570 PRINT USING " Cap'y needed to resonate.. ####.### pF.³";CRES;
1580 PRINT USING "Cap'y of cable in loop.... ####.### pF.";CLOOP
1590 PRINT USING " Length of stub..........S= ####.### in.³";STUB;
1600 PRINT USING "Cap'y of cable in stub.... ####.### pF.";CSTUB
1610 '
1620 '.....calculate variables
1630 BM=B*2.539999E-02                     'octagon side in metres
1640 LR=(9.959999E-01*10^-6*SQR(F*10^6))/D  'loss resistance - ohms per foot
1650 RL=P/12*LR                     'loop loss resistance
1660 AR=4.828000*BM^2                  'octagon area in square metres
1670 RR=3.119999*10^4*(1*AR/WL^2)^2     'radiation resistance
1680 Q=XL/(2*(RR+RL))               'loop Q
1690 BAND=F/Q*10^3                  'bandwidth, kHz
1700 VC=SQR(PWR*XL*Q)               'voltage across capacitor
1710 EFF=RR/(RR+RL)*100             'efficiency %
1720 IL=SQR(PWR*Q/XL)               'loop current at resonance
1730 '
1740 PRINT USING " Radiation resistance...... ####.#### ê ³";RR;
1750 PRINT USING "AC loss resistance........ ####.#### ê ";RL
1760 PRINT USING " Bandwidth................. ####.### kHz³";BAND;
1770 PRINT USING "Loop Q.................... ####        ";Q
1780 PRINT USING " Efficiency................ ####.### %  ³";EFF;
1790 PRINT USING "Current at resonance...... ####.### amp";IL
1800 IF VC<STUBV THEN 1830
1810 COLOR 14,4
1820 LN=CSRLIN:LOCATE 3,26:PRINT LOOP$:LOCATE LN
1830 PRINT USING " Transmitter power (watts). ####.### W. ³";PWR;
1840 PRINT USING "Voltage across loop ends.. ######## v. ";VC
1850 COLOR 7,0
1860 PRINT STRING$(80,196);
1870 LOCATE CSRLIN-1,41:PRINT "Á"
1880 '
1890 C=3*CSTUB
1900 CC=CLOOP+C:F1=SQR(2.533028E+04/CC/LUH)
1910 F2=SQR(2.533028E+04/CLOOP/LUH)
1920 Z$="  (A ### pF variable capacitor in place of stub will "
1930 C$=Z$+"tune ##.### to ##.### MHz) "
1940 PRINT USING C$;C,F1,F2;
1950 PRINT UL$;
1960 'IF FLAG=1 THEN FLAG=0:GOTO 1980
1970 IF VC<STUBV THEN 2010
1980 COLOR 14,4
1990 IF FLAG=1 THEN FLAG=0:GOTO 2010
2000 LOCATE 22,15:PRINT " D A N G E R !  Voltage exceeds rating of cable ! ";
2010 COLOR 7,0
2020 GOTO 3230
2030 END
2040 '
2050 '.....diagram
2060 T=5
2070 COLOR 0,7
2080 LOCATE ,T:PRINT "    ÉÍÍÍÍ(3)ÍÍÍÍ»                                  "
2090 LOCATE ,T:PRINT "    º     º     º    1¯ Coaxial cable loop         "
2100 LOCATE ,T:PRINT "  1¯º   2¯º     º    2¯ Coaxial cable tuning stub  "
2110 LOCATE ,T:PRINT "    º           º   (3) (see enlarged detail)      "
2120 LOCATE ,T:PRINT "    º  ³®-B-¯³  º    B = 1/8 of loop perimeter     "
2130 LOCATE ,T:PRINT "    ÈÍÍÑÍÍÍÍÍÑÍÍ¼                                  "
2140 LOCATE ,T:PRINT "       ÀÄ¿ ÚÄÙ®ÄDelta match to 50 ê coax feedline  "
2150 COLOR 0,3
2160 LOCATE ,T:PRINT "(4-sided loop shown, but octagonal loop described.)"
2170 COLOR 0,7
2180 LOCATE 3:T=58
2190 LOCATE ,T:PRINT "       1 in.       "
2200 LOCATE ,T:PRINT "ÍÍÍÍÍÍÑ gap ÑÍÍÍÍÍÍ"
2210 LOCATE ,T:PRINT "ÄÄÄÄÄÄÄ     ÄÄÄÄÄÄÄ"
2220 LOCATE ,T:PRINT "Í®1¯ÍÑÏ     ÏÑÍ®1¯Í"
2230 LOCATE ,T:PRINT "     ³   ÚÄÄÄÙ     "
2240 LOCATE ,T:PRINT "     ÀÄÒ ³ ·       "
2250 LOCATE ,T:PRINT "       º ³ º®2     "
2260 COLOR 0,3
2270 LOCATE ,T:PRINT "    Detail  (3)    "
2280 COLOR 7,0
2290 RETURN
2300 '
2310 '.....preamble
2320 TB=7
2330 PRINT TAB(TB);
2340 PRINT " The OCTALOOP design was conceived by Harold Kane, W2AHW, based on "
2350 PRINT TAB(TB);
2360 PRINT "the W5QJR Hart loop (The ARRL Antenna Book, 18th edition, p. 5-11)."
2370 PRINT TAB(TB);
2380 PRINT "The OCTALOOP consists of coaxial cable formed into an octagon with "
2390 PRINT TAB(TB);
2400 PRINT "a one inch gap (3) at the apex."
2410 PRINT TAB(TB);
2420 PRINT " It uses the outer conductor (shield) of coaxial cable as its      "
2430 PRINT TAB(TB);
2440 PRINT "radiating element, and the distributed capacitance of the coaxial  "
2450 PRINT TAB(TB);
2460 PRINT "cable for most of the capacitance required for resonance (ref. The "
2470 PRINT TAB(TB);
2480 PRINT "W1FB Loop, The ARRL Antenna Book, 18th edition, p. 5-19).          "
2490 PRINT TAB(TB);
2500 PRINT " The balance of the required capacitance is provided by a coaxial  "
2510 PRINT TAB(TB);
2520 PRINT "tuning stub. To expand the inherent narrow bandwidth of a small    "
2530 PRINT TAB(TB);
2540 PRINT "loop the stub may be replaced by a high-voltage variable capacitor."
2550 PRINT TAB(TB);
2560 PRINT " The primary equation by F.W.Grover of the US National Bureau of   "
2570 PRINT TAB(TB);
2580 PRINT "Standards is in the ARRL Antenna Book, 18th ed., p. 5-4, Table 1. ";
2590 RETURN
2600 '
2610 '.....coax cable list
2620 REM ..read data
2630 OPEN"I",1,"\hamcalc\coaxfile\coaxial.fil"
2640 FOR Z=1 TO 34:FOR Y=1 TO 9
2650 INPUT #1,Z$:C$(Z,Y)=Z$
2660 NEXT Y:NEXT Z
2670 CLOSE
2680 '.....physical dimensions
2690 CLS:COLOR 7,0,0
2700 T1$=" Max    Braid    pF "
2710 T2$="volts   OD(in.) /ft."
2720 PRINT "Item";TAB(18);T1$;TAB(39);"³Item";TAB(59);T1$
2730 PRINT " No.   Type";TAB(18);T2$;TAB(39);"³ No.   Type";TAB(59);T2$
2740 FOR Z=1 TO 17
2750 HV=VAL(C$(Z,6)):J(Z,3)=HV
2760 HV1=VAL(C$(Z+17,6)):J(Z+17,3)=HV1
2770 E=VAL(C$(Z,3))       'dielectric constant
2780 E1=VAL(C$(Z+17,3))   'dielectric constant
2790 A$=C$(Z,1)           'type
2800 A1$=C$(Z+17,1)       'type
2810 B=VAL(C$(Z,5))       'impedance
2820 B1=VAL(C$(Z+17,5))   'impedance
2830 DD=10^(B*SQR(E)/138)    'D/d factor
2840 DD1=10^(B1*SQR(E1)/138) 'D/d factor
2850 ID=VAL(C$(Z,8))      'inner conductor diameter
2860 ID1=VAL(C$(Z+17,8))  'inner conductor diameter
2870 D=DD*ID*1.049999         'braid OD (approx)
2880 J(Z,1)=D
2890 D1=DD1*ID1*1.049999      'braid OD (approx)
2900 J(Z+17,1)=D1
2910 CX=(7.260000*E/(LOG(DD)*LF))
2920 J(Z,2)=CX
2930 CX1=(7.260000*E1/(LOG(DD1)*LF))
2940 J(Z+17,2)=CX1
2950 Q$="######    .###   ##.##"
2960 IF Z<10 THEN PRINT " ";STR$(Z);;" ";:GOTO 2980
2970 PRINT STR$(Z);;" ";
2980 PRINT A$;
2990 PRINT TAB(17);USING Q$;HV,D,CX;
3000 PRINT TAB(39);;"³";
3010 PRINT STR$(Z+17);;" ";
3020 PRINT A1$;
3030 PRINT TAB(58);USING Q$;HV1,D1,CX1
3040 NEXT Z
3050 PRINT "* = foam dielectric.";TAB(39);"³ 35 UNKNOWN"
3060 COLOR 0,7
3070 PRINT " Note: The greater the loop braid OD, the higher the efficiency. ";
3080 PRINT "Cables with   "
3090 PRINT " braid dia. less then .30 not recommended. For maximum efficiency";
3100 PRINT " use RG-218.  "
3110 COLOR 7,0
3120 INPUT "ENTER: Item number to select a cable";Z
3130 IF Z>35 OR Z<1 THEN BEEP:LOCATE CSRLIN-1:GOTO 3120
3140 D=J(Z,1):CFT=J(Z,2):LOOP$=C$(Z,1)+" -"+STR$(J(Z,3))+" max.volts"
3150 STUBV=J(Z,3)
3160 IF Z=35 THEN FLAG=1:D=0:CFT=O:STUBV=0:LOOP$=" RG type unknown "
3170 RETURN
3180 '
3190 '.....erase bottom of screen
3200 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
3210 RETURN
3220 '
3230 '.....end
3240 GOSUB 3260:GOTO 120
3250 '
3260 '.....PRT
3270 KEY OFF:GOSUB 3340:LOCATE 25,5:COLOR 0,2
3280 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
3290 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
3300 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 3300 ELSE GOSUB 3340
3310 IF Z$="3"THEN RETURN
3320 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
3330 IF Z$="2"THEN LPRINT CHR$(12) ELSE 3270
3340 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

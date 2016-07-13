1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'TRANCCT - Transistor Circuits - 21 NOV 95 rev.13 APR 97
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF:COLOR 7,0,1
40 UL$=STRING$(80,205)
50 E$=STRING$(80,32)
60 U$="###,###"
70 '
80 '.....start
90 CLS
100 R=1:C=1:GOSUB 340:PRINT "";      'diagrams
110 R=1:C=42:GOSUB 520:PRINT ""
120 R=13:C=1:GOSUB 700:PRINT ""
130 R=13:C=42:GOSUB 880
140 '
150 COLOR 15,1:LOCATE CSRLIN,26
160 PRINT " TYPICAL TRANSISTOR CIRCUITS ";
170 COLOR 14,4:LOCATE 25,22
180 PRINT " Press 1 to continue or 0 to EXIT... ";
190 COLOR 7,0
200 Z$=INKEY$:IF Z$=""THEN 200
210 IF Z$="0"THEN CLS:RUN EX$
220 IF Z$="1"THEN CLS:GOTO 250
230 GOTO 200
240 '
250 '.....title page
260 COLOR 15,2
270 PRINT " TRANSISTOR CIRCUIT DESIGN";TAB(57);;"by George Murphy VE3ERP ";
280 COLOR 1,0:PRINT STRING$(80,223);
290 COLOR 7,0
300 GOSUB 1060  'preface
310 R=0:C=0:T=0
320 GOSUB 3560:GOTO 1470  'transistor design program
330 '
340 '.....common emitter
350 COLOR 0,7:LOCATE R
360 LOCATE ,C:PRINT "    SIGNAL       Vcc    Vcc            "
370 LOCATE ,C:PRINT " ÚÄ SOURCE ÄÄ¿    Ú¯     Ú¯  I outÄ¯   "
380 LOCATE ,C:PRINT "  Ri=internal     R1     RL  ®ÄR out   "
390 LOCATE ,C:PRINT "  resistance      ³      ÃÄÄÄÄÄÄÄÄÄþ®¿ "
400 LOCATE ,C:PRINT "       I inÄ¯     ³ItÄ¯ÚÄÁ¿c         ³ "
410 LOCATE ,C:PRINT " +ÚÄÄÄÄRiÄÄÄÄÄRsÄÄÅÄÄÄÄ´b ³U1     Vout "
420 LOCATE ,C:PRINT " ÚÁ¿®¿ R inÄ¯     ³RtÄ¯ÀÄÂÙe         ³ "
430 LOCATE ,C:PRINT " ³~³Vin           R2     ³        Úþ®Ù "
440 LOCATE ,C:PRINT " ÀÂÙ®Ù            ³      ³        ³    "
450 LOCATE ,C:PRINT " -ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ    "
460 COLOR 15,2
470 LOCATE ,C:PRINT "            ";
480 COLOR 7,0
490 R=0:C=0
500 RETURN
510 '
520 '.....common emitter with emitter resistor
530 COLOR 0,7:LOCATE R
540 LOCATE ,C:PRINT "    SIGNAL       Vcc    Vcc            ";
550 LOCATE ,C:PRINT " ÚÄ SOURCE ÄÄ¿    Ú¯     Ú¯  I outÄ¯   ";
560 LOCATE ,C:PRINT "  Ri=internal     R1     RL  ®ÄR out   ";
570 LOCATE ,C:PRINT "  resistance      ³      ÃÄÄÄÄÄÄÄÄÄþ®¿ ";
580 LOCATE ,C:PRINT "       I inÄ¯     ³ItÄ¯ÚÄÁ¿c         ³ ";
590 LOCATE ,C:PRINT " +ÚÄÄÄÄRiÄÄÄÄÄRsÄÄÅÄÄÄÄ´b ³U1     Vout ";
600 LOCATE ,C:PRINT " ÚÁ¿®¿ R inÄ¯     ³RtÄ¯ÀÄÂÙe         ³ ";
610 LOCATE ,C:PRINT " ³~³Vin           R2     R3       Úþ®Ù ";
620 LOCATE ,C:PRINT " ÀÂÙ®Ù            ³      ³        ³    ";
630 LOCATE ,C:PRINT " -ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ    ";
640 COLOR 15,2
650 LOCATE ,C:PRINT "  ";
660 COLOR 7,0
670 R=0:C=0
680 RETURN
690 '
700 '.....common collector
710 COLOR 0,7:LOCATE R
720 LOCATE ,C:PRINT "    SIGNAL       Vcc    Vcc            "
730 LOCATE ,C:PRINT " ÚÄ SOURCE ÄÄ¿    Ú¯     Ú¯            "
740 LOCATE ,C:PRINT "  Ri=internal     R1     ³             "
750 LOCATE ,C:PRINT "  resistance      ³      ³             "
760 LOCATE ,C:PRINT "       I inÄ¯     ³ItÄ¯ÚÄÁ¿c           "
770 LOCATE ,C:PRINT " +ÚÄÄÄÄRiÄÄÄÄÄRsÄÄÅÄÄÄÄ´b ³U1 I outÄ¯  "
780 LOCATE ,C:PRINT " ÚÁ¿®¿ R inÄ¯     ³RtÄ¯ÀÄÂÙe  ®ÄR out  "
790 LOCATE ,C:PRINT " ³~³Vin           R2     ÃÄÄÄÄÄÄÄÄÄþ®¿ "
800 LOCATE ,C:PRINT " ÀÂÙ®Ù            ³      RL       Vout "
810 LOCATE ,C:PRINT " -ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄþ®Ù "
820 COLOR 15,2
830 LOCATE ,C:PRINT "           ";
840 COLOR 7,0
850 R=0:C=0
860 RETURN
870 '
880 '.....common base
890 COLOR 0,7:LOCATE R
900 LOCATE ,C:PRINT "    SIGNAL       Vcc    Vcc            ";
910 LOCATE ,C:PRINT " ÚÄ SOURCE ÄÄ¿    Ú¯     Ú¯  I outÄ¯   ";
920 LOCATE ,C:PRINT "  Ri=internal     R1     RL  ®ÄR out   ";
930 LOCATE ,C:PRINT "  resistance      ³      ÃÄÄÄÄÄÄÄÄÄþ®¿ ";
940 LOCATE ,C:PRINT "       I inÄ¯     ³ItÄ¯ÚÄÁ¿c         ³ ";
950 LOCATE ,C:PRINT " +ÚÄÄÄÄRiÄÄÄÄÄRsÄÄÅÄÄÄÄ´e ³U1     Vout ";
960 LOCATE ,C:PRINT " ÚÁ¿®¿ R inÄ¯     ³RtÄ¯ÀÄÂÙb         ³ ";
970 LOCATE ,C:PRINT " ³~³Vin           R2     ³        Úþ®Ù ";
980 LOCATE ,C:PRINT " ÀÂÙ®Ù            ³      ³        ³    ";
990 LOCATE ,C:PRINT " -ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ    ";
1000 COLOR 15,2
1010 LOCATE ,C:PRINT "              ";
1020 COLOR 7,0
1030 R=0:C=0
1040 RETURN
1050 '
1060 '.....preface
1070 T=7
1080 PRINT TAB(T);
1090 PRINT "This program is based on a program by David Leithauser in 'Programs"
1100 PRINT TAB(T);
1110 PRINT "for Electronic Circuit Design', Radio Shack cat.# 62-1054, pages"
1120 PRINT TAB(T);
1130 PRINT "105-118. The calculations are for standard NPN or PNP bipolar"
1140 PRINT TAB(T);
1150 PRINT "junction transistors."
1160 PRINT
1170 PRINT TAB(T);
1180 PRINT "It is recommended that you make a hard copy printout of the four"
1190 PRINT TAB(T);
1200 PRINT "typical circuits to use as a reference while running the program."
1210 PRINT TAB(T);
1220 PRINT "You have the option of asking the computer to suggest which of the"
1230 PRINT TAB(T);
1240 PRINT "four circuits might be appropriate for your application."
1250 PRINT
1260 PRINT TAB(T);
1270 PRINT "You will be asked for the source resistance of your circuit. The"
1280 PRINT TAB(T);
1290 PRINT "source resistance is the sum of the internal resistance Ri of the"
1300 PRINT TAB(T);
1310 PRINT "signal source plus physical resistor Rs, if there is one in your"
1320 PRINT TAB(T);
1330 PRINT "circuit."
1340 PRINT
1350 PRINT TAB(T);
1360 PRINT "Some of the values you will be asked to enter are essential. If you"
1370 PRINT TAB(T);
1380 PRINT "indicate that such values are unknown, the program will enter"
1390 PRINT TAB(T);
1400 PRINT "typical values, however it is recommended that for best results"
1410 PRINT TAB(T);
1420 PRINT "the actual values be obtained from the transistor manufacturer's"
1430 PRINT TAB(T);
1440 PRINT "data sheets."
1450 RETURN
1460 '
1470 '.....Transistor Circuits Program
1480 GOTO 1550
1490 '
1500 '.....format input line
1510 U$="######.###":LOCATE CSRLIN-1:PRINT STRING$(7,32);
1520 LOCATE CSRLIN,45:PRINT STRING$(19,".");USING U$;ZZ;
1530 RETURN
1540 '
1550 CLS
1560 PRINT " TRANSISTOR CIRCUITS MENU OPTIONS"
1570 PRINT UL$;
1580 PRINT " Press number in ( ) for:"
1590 PRINT UL$;
1600 PRINT "  (1) Transistor Circuit Characteristics"
1610 PRINT "  (2) Transistor Circuit Selection"
1620 PRINT "  (3) Emitter Resistor Computation"
1630 PRINT "  (4) QUIT"
1640 Z$=INKEY$:IF Z$="" THEN 1640
1650 IF Z$="1"THEN 1710
1660 IF Z$="2"THEN 2790
1670 IF Z$="3"THEN 3220
1680 IF Z$="4"THEN 80      'master menu
1690 GOTO 1640
1700 '
1710 CLS '.....transistor circuit characteristics
1720 PRINT " TRANSISTOR CIRCUIT CHARACTERISTICS"
1730 PRINT UL$;
1740 PRINT " Press number in ( ) for:"
1750 PRINT UL$;
1760 PRINT "  (1) Common Emitter Circuit"
1770 PRINT "  (2) Common Emitter Circuit with Emitter Resistor"
1780 PRINT "  (3) Common Collector Circuit"
1790 PRINT "  (4) Common Base Circuit"
1800 Z$=INKEY$:IF Z$=""THEN 1800
1810 N=VAL(Z$):IF N>=1 AND N<=4 THEN 1840
1820 GOTO 1800
1830 '
1840 CLS '.....circuit menu
1850 R=1:C=21
1860 IF N=1 THEN GOSUB 340
1870 IF N=2 THEN GOSUB 520
1880 IF N=3 THEN GOSUB 700
1890 IF N=4 THEN GOSUB 880
1900 '
1910 PRINT UL$;
1920 INPUT " ENTER: Small Signal Current Gain (Hfe).........(0 if unknown)...";F
1930 IF F=0 THEN F=50
1940 ZZ=F:GOSUB 1500:PRINT ""
1950 INPUT " ENTER: Value of Load Resistor RL...............................ê";L
1960 IF L=0 THEN LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1:GOTO 1950
1970 ZZ=L:GOSUB 1500:PRINT " ê"
1980 INPUT " ENTER: Value of Bias Resistor R1...............(0 if none).....ê";X
1990 IF X=0 THEN X=10^-6:LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1:GOTO 2020
2000 ZZ=X:GOSUB 1500:PRINT " ê"
2010 '
2020 IF X<>10^-6 THEN 2090
2030 LN=CSRLIN
2040 COLOR 0,7
2050 FOR Q=1 TO 5:LOCATE Q,38:PRINT "   ";:NEXT Q:LOCATE 5,40:PRINT "I"
2060 LOCATE 6,39:PRINT "Â"
2070 COLOR 7,0:LOCATE LN
2080 '
2090 INPUT " ENTER: Value of Bias Resistor R2...............(0 if none).....ê";Y
2100 IF Y=0 THEN Y=10^-6:LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1:GOTO 2130
2110 ZZ=Y:GOSUB 1500:PRINT " ê"
2120 '
2130 IF Y<>10^-6 THEN 2220
2140 LN=CSRLIN
2150 COLOR 0,7
2160 LOCATE 6,39:PRINT "Á"
2170 FOR Q=7 TO 9:LOCATE Q,39:PRINT " ";:NEXT Q
2180 LOCATE 8,40:PRINT " "
2190 LOCATE 10,39:PRINT "Ä"
2200 COLOR 7,0:LOCATE LN
2210 '
2220 IF X=10^-6 AND Y=10^-6 THEN 2230 ELSE 2250
2230 LOCATE 6,39:COLOR 0,7:PRINT "Ä";:COLOR 7,0:LOCATE LN
2240 '
2250 A=1/X+1/Y
2260 R=1/A
2270 '
2280 PRINT " Does resistor Rs physically exist in your application?  (y/n)"
2290 Z$=INKEY$:IF Z$="" THEN 2290
2300 IF Z$="y"THEN S$="Ri+Rs":GOTO 2330
2310 IF Z$="n"THEN S$="Ri...":GOTO 2330
2320 GOTO 2290
2330 LN=CSRLIN-1:LOCATE LN:PRINT E$;:LOCATE LN
2340 IF Z$="n"THEN LOCATE 6,36:COLOR 0,7:PRINT "ÄÄ";:COLOR 7,0:LOCATE LN
2350 PRINT " ENTER: Source Resistance ";S$;;"...............................ê";
2360 INPUT S
2370 ZZ=S:GOSUB 1500:PRINT " ê"
2380 IF N<>2 THEN 2410
2390 INPUT " ENTER: Value of Emitter Resistor Re............................ê";E
2400 ZZ=E:GOSUB 1500:PRINT " ê"
2410 INPUT " ENTER: Transistor Input Impedance (Hie).......(0 if unknown)..ê";C
2420 IF C=0 THEN C=1100
2430 ZZ=C:GOSUB 1500:PRINT " ê"
2440 IF N<>1 AND N<>3 THEN 2490
2450 INPUT " ENTER: Output Conductance (Hoe) in siemens.....(0 if unknown)...";Z
2460 IF Z=0 THEN Z=10^-6
2470 ZZ=Z:GOSUB 1500:PRINT " S"
2480 T=1/Z
2490 IF N<>1 THEN 2530
2500 K=R/(R+C)*(-F*T/(T+L))
2510 U=T
2520 P=S+1/(A+1/C)
2530 IF N<>2 THEN 2560
2540 K=-F*R/(R+C+(1+F)*E)
2550 P=S+1/(A+1/(C+(1+F)*E))
2560 IF N<>3 THEN 2610
2570 H=(1+F)/(1+Z*L)
2580 K=H*R/(R+C+H*L)
2590 P=S+1/(A+1/(C+H*L))
2600 U=1/((1+F)/(S*R/(S+R)+C)+Z)
2610 IF N<>4 THEN 2640
2620 K=F/(1+F)*R/(R+C/(1+F))
2630 P=S+1/(A+(1+F)/C)
2640 '
2650 PRINT "        Current Gain........................................... "
2660 ZZ=K:GOSUB 1500:PRINT ""
2670 PRINT "        Input Impedance........................................ "
2680 ZZ=P:GOSUB 1500:PRINT " ê"
2690 PRINT "        Voltage Gain........................................... "
2700 ZZ=K*L/P:GOSUB 1500:PRINT ""
2710 IF N=2 OR N=4 THEN J=L
2720 IF N=1 OR N=3 THEN J=U*L/(U+L)
2730 PRINT "        Output Resistance...................................... "
2740 ZZ=J:GOSUB 1500:PRINT " ê"
2750 N=0:F=0:L=0:X=0:Y=0:A=0:R=0:S=0:E=0:C=0:Z=0:T=0:K=0:U=0:P=0:H=0
2760 GOTO 3510
2770 '
2780 '
2790 CLS  '.....transistor circuit selection
2800 PRINT " TRANSISTOR CIRCUIT SELECTION"
2810 PRINT UL$;
2820 INPUT " ENTER: Transistor Input Impedance (Hie)....(0 if unknown).....ê";Z
2830 IF Z=0 THEN Z=1111
2840 ZZ=Z:GOSUB 1500:PRINT " ê"
2850 INPUT " ENTER: Transistor Gain (Hfe)...............(0 if unknown).....ê";F
2860 IF F=0 THEN F=51
2870 ZZ=F:GOSUB 1500:PRINT ""
2880 INPUT " ENTER: Load Resistance RL..................(0 if Don't Care)..ê";L
2890 ZZ=L:GOSUB 1500:PRINT " ê"
2900 INPUT " ENTER: Source Resistance Ri+Rs................................ê";S
2910 IF S=0 THEN LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1:GOTO 2900
2920 ZZ=S:GOSUB 1500:PRINT " ê"
2930 INPUT " ENTER: Desired Current Gain................(1 if Don't Care)...";K
2940 ZZ=K:GOSUB 1500:PRINT ""
2950 INPUT " ENTER: Desired Voltage Gain................(1 if Don't Care)...";V
2960 ZZ=V:GOSUB 1500:PRINT ""
2970 INPUT " ENTER: Desired Output Impedance............(1 if Don't Care)..ê";U
2980 ZZ=U:GOSUB 1500:PRINT " ê"
2990 INPUT " ENTER: Desired Input Impedance.............(1 if Don't Care)..ê";E
3000 ZZ=E:GOSUB 1500:PRINT " ê"
3010 PRINT TAB(9);;" Enter Desired Output Polarity as -1 if inverted."
3020 PRINT TAB(9);;" Enter Desired Output Polarity as  0 if Don't Care."
3030 PRINT TAB(9);;" Enter Desired Output Polarity as  1 if non-inverted."
3040 INPUT " ENTER: Desired Output Polarity.................................";P
3050 LOCATE CSRLIN-1:PRINT STRING$(7,32);
3060 LOCATE CSRLIN,45:PRINT STRING$(19,".");USING "+#####";P
3070 PRINT UL$;
3080 N=0:PRINT " The following circuits may work:"
3090 PRINT
3100 IF P<=0 AND V=>1 AND K=>1 AND (F=51 OR K<=F*1.200000) AND (E=1 OR Z=1111 OR E<S+Z*1.200000) AND (L=0 OR U=1 OR ABS(U-L)<U/5)THEN PRINT " ";
3110 '
3120 IF P=>0 AND K=>1 AND (F=51 OR K<=1+F*1.200000) AND V<=1 AND (E=1 OR L=0 OR Z=1111 OR E>Z+S) AND (U=1 OR L=0 OR F=51 OR Z=1111 OR ABS(U-(S+Z)/(1+F))<U/5) THEN PRINT " ";
3130 '
3140 IF P=>0 AND K<=1 AND V=>1 AND (E=1 OR Z=1111 OR F=51 OR E<Z) AND (U=1 OR L-0 OR ABS(U-L)<U/5) THEN PRINT " ";
3150 '
3160 IF P<=0 AND K=>1 AND (F=51 OR K<=F*1.200000) AND V=>1 AND (E=1 OR Z=1111 OR E>Z) AND (L=0 OR U=1 OR ABS(U-L)<U/5) THEN PRINT " ";
3170 IF N=0 THEN PRINT " NONE OF THESE CIRCUITS WILL WORK!"
3180 N=0:Z=0:F=0:L=0:S=0:K=0:V=0:U=0:E=0:P=0
3190 GOTO 3510
3200 '
3210 '
3220 CLS'.....emitter resistor calculation
3230 R=1:C=21:GOSUB 520:PRINT ""
3240 PRINT
3250 PRINT " Emitter Resistor R3:"
3260 PRINT UL$;
3270 INPUT " ENTER: Desired Voltage Gain................(0 if Don't Care)...";V
3280 ZZ=V:GOSUB 1500:PRINT ""
3290 IF V=0 THEN 3320
3300 INPUT " ENTER: Load Resistor RL....................(0 if Don't Care)..ê";L
3310 ZZ=L:GOSUB 1500:PRINT " ê"
3320 INPUT " ENTER: Desired Circuit Input Impedance.....(0 if Don't Care)..ê";R
3330 ZZ=R:GOSUB 1500:PRINT " ê"
3340 IF R=0 THEN 3410
3350 INPUT " ENTER: Transistor's Input Impedance........(0 if unknown).....ê";Z
3360 ZZ=Z:GOSUB 1500:PRINT " ê"
3370 IF Z=0 THEN Z=1111
3380 INPUT " ENTER: Transistor's Current Gain (Hfe).....(0 if unknown)......";F
3390 ZZ=F:GOSUB 1500:PRINT ""
3400 IF F=0 THEN F=51
3410 PRINT UL$;
3420 IF V=0 THEN PRINT " Emitter Resistor R3 =";USING U$;(R-Z)/(1+F);:PRINT " ê"
3430 IF R=0 AND V*L<>0 THEN PRINT " Emitter Resistor R3 =";USING U$;L/V;:PRINT " ê"
3440 IF R=0 AND L=0 AND V<>0 THEN PRINT " Any Value Emitter Resistor R3 as long as R3 = RL ö";V;;" ohms"
3450 IF R=0 AND V=0 THEN PRINT " Any value of R3 will work."
3460 IF L<>0 AND R<>0 AND V<>0 THEN IF L/V=(R-Z)/(1+F) THEN PRINT " Emitter Resistor R3 =";USING U$;L/V;:PRINT " ohms
3470 IF R<>0 AND V<>0 THEN IF L/V<>(R-Z)/(1+F) AND L<>0 THEN PRINT " CONTRADICTORY CONDITIONS. NO VALUE OF R3 WILL WORK!"
3480 N=0:V=0:L=0:R=0:Z=0:F=0
3490 GOTO 3510
3500 '
3510 '.....end
3520 GOSUB 3560
3530 CLS:GOTO 80
3540 END
3550 '
3560 'PRT
3570 KEY OFF:GOSUB 3640:LOCATE 25,5:COLOR 0,2
3580 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
3590 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
3600 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 3600 ELSE GOSUB 3640
3610 IF Z$="3"THEN RETURN
3620 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
3630 IF Z$="2"THEN LPRINT CHR$(12) ELSE 3570
3640 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

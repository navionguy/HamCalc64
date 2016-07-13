1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'PSYCHROM - Thermodynamics - 22 APR 2004
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PI=4*ATN(1)  '3.141593
70 UL$=STRING$(80,205)
80 U1$="#####.###"
90 T$="THERMODYNAMICS"
100 '
110 COLOR 15,2,1
120 PRINT " ";T$;TAB(55);;"by H.K.McMillan and J.Kim ";
130 COLOR 1,0:PRINT STRING$(80,223);;";
140 COLOR 15,2:LOCATE 2,27:PRINT " algorithm by R.J.Dehoney "
150 '
160 COLOR 7,0
170 PRINT
180 PRINT "This program determines the thermodynamics of moist air in the"
190 PRINT "temperature range 0-50ø C (32-122ø  F). The program has 8 outputs:"
200 PRINT "      Barometric Pressure,   Dry Bulb Temperature,"
210 PRINT "      Wet Bulb Temperature,  Dew Point Temperature,"
220 PRINT "      Relative Humidity,     Humidity Ratio,"
230 PRINT "      Specific Enthalpy of the moist air, and"
240 PRINT "      Specific Volume of the moist air."
250 PRINT "User can specify outputs and inputs in either English/USA or SI (";
260 PRINT "metric) units."
270 PRINT "      Three inputs are required"
280 PRINT "      1. Barometric Pressure"
290 PRINT "      2. Dry Bulb Temperature"
300 PRINT "      3. User preference of either:"
310 PRINT "             a) Wet Bulb Temperature"
320 PRINT "             b) Dew Point Temperature"
330 PRINT "             c) Relative Humidity"
340 PRINT "The program assumes that dry air and water vapor behave as perfect";
350 PRINT " gases."
360 PRINT
370 PRINT "To continue.........press 1"
380 PRINT "To EXIT.............press 0"
390 Z$=INKEY$:IF Z$=""THEN 390
400 IF Z$="1" THEN 430
410 IF Z$="0" THEN CLS:CHAIN GO$
420 GOTO 390
430 CLS:COLOR 7,0,0
440 PRINT "Specify the barometric pressure units by entering the number."
450 PRINT "         1) U.S. Standard Atmosphere (14.696 PSI)";
460 PRINT "         2) Pounds per square inch (PSI)";
470 PRINT "         3) Kilopascals (KPA)";
480 PRINT "         4) Inches of mercury";
490 INPUT "Enter <1 to 4> ";UNIP :PRINT
500 ON UNIP GOTO 510,520,540,560
510 PT=1.469600E+01 :GOTO 590
520 PRINT "Enter: pressure in pounds per square inch (PSI)";
530 INPUT " ";PT :GOTO 590
540 PRINT "Enter: pressure in kilopascals (KPA) ";
550 INPUT " ";PT :PT=1.450400E-01*PT :GOTO 590
560 PRINT "Enter: pressure in decinal inches of mercury";
570 INPUT " ";PT :PT=4.909999E-01*PT
580 PRINT
590 CLS: PRINT "Input temperatures in (f)ahrenheit or (c)elsius   (f/c)?"
600 A$=INKEY$:IF A$=""THEN 600
610 IF A$="f"OR A$="F"THEN UNI=1:GOTO 640
620 IF A$="c"OR A$="C"THEN UNI=2:GOTO 640
630 GOTO 600
640 CLS
650 PRINT "For program ouputs in English/USA units.............press 1"
660 PRINT "For program ouputs in SI (metric) units.............press 2"
670 Z$=INKEY$:IF Z$=""THEN 670
680 IF Z$="1"THEN UNO=1:GOTO 710
690 IF Z$="2"THEN UNO=2:GOTO 710
700 GOTO 670
710 CLS
720 PRINT "Choose which of the following three optional inputs you want to use:";
730 PRINT "          1) Wet Bulb Temperature";
740 PRINT "          2) Dew Point Temperature";
750 PRINT "          3) Relative Humidity";
760 PRINT
770 INPUT "Enter number (1,2,3) ";VAR3
780 ON VAR3 GOSUB 970, 1310, 1560
790 GOSUB 2210
800 PRINT STRING$(80,205);
810 COLOR 0,7
820 PRINT "(See also the HAMCALC ";CHR$(34);;"Barometer Reading Equivalents";
830 PRINT CHR$(34);;" program)";:COLOR 7,0
840 PRINT
850 GOSUB 2880
860 CLS
870 PRINT "To re-run program............press 1";
880 PRINT "To QUIT......................press 0";
890 Z$=INKEY$:IF Z$=""THEN 890
900 IF Z$="0"THEN 110
910 IF Z$="1"THEN 930
920 CLS:INPUT "To re-run the program, enter <1> else <CR> ",RA
930 CLS
940 INPUT "New pressure? <y/n) ",A$
950 IF A$="y" THEN 430 ELSE 590
960 '
970 'Subroutine to calc moist air properties using pressure and wet/dry temp.
980 CLS : ON UNI GOTO 1070,990
990 PRINT "Mandatory input:"
1000 INPUT "Enter: dry bulb temperature between 0 and 50 degrees C ";TDB
1010 PRINT
1020 PRINT "Optional input:"
1030 INPUT "Enter: wet bulb temperature in degrees C ";TWB
1040 TDB=1.799999*TDB+32
1050 TWB=1.799999*TWB+32
1060 GOTO 1110
1070 PRINT "Mandatory input:"
1080 INPUT "Enter: dry bulb temp between 32 and 122 degrees F ";TDB
1090 INPUT "Enter: wet bulb temp in degrees F";TWB
1100 PRINT
1110 T=TWB
1120 GOSUB 1800
1130 PSTWB=P
1140 HVTDB=1.061800E+03+4.399999E-01*TDB
1150 HVTWB=1.061800E+03+4.399999E-01*TWB
1160 HFTWB=TWB-32
1170 WTWB=(6.219800E-01*PSTWB)/(PT-PSTWB)
1180 W=(2.399999E-01*(TWB-TDB)+WTWB*(HVTWB-HFTWB))/(HVTDB-HFTWB)
1190 PV=(W*PT)/(6.219800E-01+W)
1200 T=TDB
1210 GOSUB 1800
1220 PS=P
1230 RH=PV/PS
1240 H=2.399999E-01*TDB+W*HVTDB
1250 V=(5.335200E+01*(TDB+460))/((PT-PV)*1.440000E+02)
1260 P=PV
1270 GOSUB 1840
1280 TDP=T
1290 RETURN
1300 '
1310 'Subroutine to calculate moist air properties given pressure, dry bulb temp
1320 'and dew point temperature
1330 CLS : ON UNI GOTO 1400,1340
1340 PRINT "Mandatory input:"
1350 INPUT "Enter: dry bulb temp between 0.0 and 49.0 degrees C ";TDB
1360 INPUT "Enter: dew point temp between 0.0 and 49.0 degrees C ";TDP
1370 TDB=3.200000E+01+1.799999*TDB
1380 TDP=3.200000E+01+1.799999*TDP
1390 GOTO 1430
1400 PRINT "Mandatory input:"
1410 INPUT "Enter: dry bulb temp between 32.0 and 120.0 degrees F ";TDB
1420 INPUT "Enter: dew point temp between 32.0 and 120.0 degrees F ";TDP
1430 T=TDP
1440 GOSUB 1800
1450 PV=P
1460 W=6.219800E-01*PV/(PT-PV)
1470 H=2.399999E-01*TDB+W*(4.399999E-01*TDB+1.061800E+03)
1480 V=5.335200E+01*(TDB+460)/((PT-PV)*1.440000E+02)
1490 T=TDB
1500 GOSUB 1800
1510 PS=P
1520 RH=PV/PS
1530 GOSUB 1880
1540 RETURN
1550 '
1560 'Subroutine to calculate moist air properties given pressure, dry bulb temp
1570 'and relative humidity.
1580 CLS : ON UNI GOTO 1630,1590
1590 PRINT "Optional input:"
1600 INPUT "Enter: dry bulb temp between 0 and 50 degrees C ";TDB
1610 TDB=3.200000E+01+1.799999*TDB
1620 GOTO 1650
1630 PRINT "Mandatory input:"
1640 INPUT "Enter: dry bulb temp between 32.0 and 122.0 degrees F ";TDB
1650 INPUT "Enter: relative humidity as <xx.x> percent ";RH
1660 RH=RH/100
1670 T=TDB
1680 GOSUB 1800
1690 PS=P
1700 PV=RH*PS
1710 W=6.219800E-01*PV/(PT-PV)
1720 H=2.399999E-01*TDB+W*(4.399999E-01*TDB+1.061800E+03)
1730 V=5.335200E+01*(TDB+460)/((PT-PV)*1.440000E+02)
1740 P=PV
1750 GOSUB 1840
1760 TDP=T
1770 GOSUB 1880
1780 RETURN
1790 '
1800 'Subroutine to calculate saturation pressure
1810 P=2.066300E-02+1.173999E-03*T+1.570899E-05*T^2+3.996499E-07*T^3+4.525899E-10*T^4                     +2.091599E-11*T^5
1820 RETURN
1830 '
1840 'Subroutine to calculate Saturation Temperature
1850 T=1.933600E-01+4.848900E+02*P-1.744599E+03*P^2+4378*P^3-6.961500E+03*P^4+6.884600E+03*P^5                       -4.088399E+03*P^6+1.330500E+03*P^7-1.819499E+02*P^8
1860 RETURN
1870 '
1880 'Subroutine to calculate wet bulb temp
1890 WTDP=W
1900 FTDP=(TDP-TDB)*(2.399999E-01+4.399999E-01*WTDP)
1910 WTDB=6.219800E-01*PS/(PT-PS)
1920 FTDB=(WTDB-WTDP)*(1.093800E+03-TDB*5.600000E-01)
1930 TWBLI=TDP-(TDB-TDP)*(FTDP/(FTDB-FTDP))
1940 WBT=TWBLI
1950 GOSUB 2120
1960 IF FWBT>0 THEN 1970 ELSE 2030
1970 WBT=WBT-1
1980 GOSUB 2120
1990 IF FWBT>0 THEN 1970 ELSE 2000
2000 WBT=WBT+1.000000E-01
2010 GOSUB 2120
2020 IF FWBT>0 THEN 2090 ELSE 2000
2030 WBT=WBT+1
2040 GOSUB 2120
2050 IF FWBT>0 THEN 2060 ELSE 2030
2060 WBT=WBT-1.000000E-01
2070 GOSUB 2120
2080 IF FWBT>0 THEN 2060 ELSE 2090
2090 TWB=WBT
2100 RETURN
2110 '
2120 'Subroutine to "Temp Function" used in finding the wet bulb temp
2130 T=WBT
2140 GOSUB 1800
2150 PSWBT=P
2160 WWBT=6.219800E-01*PSWBT/(PT-PSWBT)
2170 WTDP=W
2180 FWBT=2.399999E-01*(WBT-TDB)+4.399999E-01*(WWBT*WBT-WTDP*TDB)+(WWBT-WTDP)*(1.093800E+03-WBT)
2190 RETURN
2200 '
2210 'Subroutine to convert outputs and print
2220 CLS
2230 RH=100*RH
2240 ON UNO GOTO 2590, 2250
2250 PT=6.894599*PT
2260 TDB=5.555499E-01*(TDB-32)
2270 TWB=5.555499E-01*(TWB-32)
2280 TDP=5.555499E-01*(TDP-32)
2290 H=2.325999*H
2300 V=6.242800E-02*V
2310 PRINT T$
2320 PRINT STRING$(80,205);
2330 PRINT "Barometric Pressure      ";
2340 PRINT USING "####.##"; PT;
2350 PRINT " KPA"
2360 PRINT "Dry Bulb Temperature      ";
2370 PRINT USING "####.#"; TDB;
2380 PRINT "øC"
2390 PRINT "Wet Bulb Temperature      ";
2400 PRINT USING "####.#"; TWB;
2410 PRINT "øC"
2420 PRINT "Dew Point Temperature     ";
2430 PRINT USING "####.#"; TDP;
2440 PRINT "øC"
2450 PRINT "Relative Humidity         ";
2460 PRINT USING "####.#"; RH;
2470 PRINT " %"
2480 PRINT "Humidity Ratio            ";
2490 PRINT USING "##.###"; W;
2500 PRINT " Kg/Kg Dry Air"
2510 PRINT "Specific Enthalpy         ";
2520 PRINT USING "####.#"; H;
2530 PRINT " Kg/Kg Dry Air"
2540 PRINT "Specific Volume           ";
2550 PRINT USING "#.####"; V;
2560 PRINT " Cu.M/Kg Dry Air"
2570 PT=PT/6.394599
2580 GOTO 2850
2590 PRINT T$
2600 PRINT STRING$(80,205);
2610 PRINT "Barometric Pressure       ";
2620 PRINT USING "###.###"; PT;
2630 PRINT " PSI"
2640 PRINT "Dry Bulb Temperature      ";
2650 PRINT USING "###.#"; TDB;
2660 PRINT "øF"
2670 PRINT "Wet Bulb Temperature      ";
2680 PRINT USING "###.#"; TWB;
2690 PRINT "øF"
2700 PRINT "Dew Point Temperature     ";
2710 PRINT USING "###.#"; TDP;
2720 PRINT "øF"
2730 PRINT "Relative Humidity         ";
2740 PRINT USING "###.#"; RH;
2750 PRINT " %"
2760 PRINT "Humidity Ratio            ";
2770 PRINT USING "#.###"; W;
2780 PRINT " Lb/Lb Dry Air"
2790 PRINT "Specific Enthalpy         ";
2800 PRINT USING "###.#"; H;
2810 PRINT " BTU/Lb Dry Air"
2820 PRINT "Specific Volume           ";
2830 PRINT USING "##.##"; V;
2840 PRINT " Cu.Ft/Lb Dry Air";
2850 RETURN
2860 END
2870 '
2880 '.....PRT
2890 KEY OFF:GOSUB 2960:LOCATE 25,5:COLOR 0,2
2900 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2910 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2920 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2920 ELSE GOSUB 2960
2930 IF Z$="3"THEN RETURN
2940 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2950 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2890
2960 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
2970 'PRINT "Optional input:"
2980 PRINT "Optional input:"

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'ACCALC - A.C. CALCULATOR - 22 APR 94 rev. 20 APR 2000
15 'OMMON EX$
20 IF EX$=""THEN EX$="EXIT"
25 ''OMMON EX$
30 CLS:KEY OFF
40 ON ERROR GOTO 2850
50 COLOR 7,0,1
60 UL$=STRING$(80,205)
70 U1$="###,###.#"
80 U2$="###,###.###"
90 U3$="###,###.##"
100 PI=3.141592
110 '
120 '.....start
130 CLS
140 RMS=0:AVG=0:PK=0:I=0:Z=0:PA=0:R=0:XL=0:XC=0
150 E=0:PR=0:PH=0:PF=0:X=0:F=0:C=0:U=0:PP=0
160 COLOR 15,2
170 PRINT " A.C. CALCULATOR";TAB(57);"by George Murphy VE3ERP ";
180 COLOR 1,0
190 PRINT STRING$(80,"�");
200 COLOR 7,0
210 PRINT " Press number in < > for:"
220 PRINT UL$;
230 PRINT "  < 1 >  Ohm's Law for A.C."
240 PRINT "  < 2 >  Impedance, Power Factor and Phase Angle"
250 PRINT "  < 3 >  Reactance, Frequency, Capacitance & Inductance (tuned cct.)"
260 PRINT "  < 4 >  Peak, Peak-to-Peak, RMS & Average voltages"
270 PRINT UL$;
280 PRINT "  < 0 >  Return to Main Menu"
290 Y$=INKEY$
300 IF Y$="0"THEN CLS:RUN EX$
310 IF Y$="1"THEN 370
320 IF Y$="2"THEN 960
330 IF Y$="3"THEN 1810
340 IF Y$="4"THEN 2230
350 GOTO 290
360 '
370 '.....ohm's law
380 CLS
390 COLOR 15,2
400 PRINT " OHM's LAW for A.C. "
410 COLOR 1,0
420 PRINT STRING$(80,"�");
430 COLOR 7,0
440 PRINT " If you do not have the data requested, press <ENTER>...
450 PRINT UL$;
460 IF RMS THEN 490
470 INPUT " ENTER: RMS Voltage.................Erms= ";RMS
480 GOSUB 670
490 IF AVG THEN 520
500 INPUT " ENTER: Average Voltage.............Eavg= ";AVG
510 GOSUB 670
520 IF PK THEN 550
530 INPUT " ENTER: Peak Voltage................Epk = ";PK
540 GOSUB 670
550 IF I THEN 580
560 INPUT " ENTER: Current (in amperes)..........I = ";I
570 GOSUB 670
580 IF Z THEN 610
590 INPUT " ENTER: Impedance (in ohms))..........Z = ";Z
600 GOSUB 670
610 IF PA THEN 640
620 INPUT " ENTER: Apparent Power (in watts)....Pa = ";PA
630 GOSUB 670
640 VIEW PRINT 5 TO 24:CLS:VIEW PRINT:LOCATE 5
650 GOTO 460
660 '
670 '.....calculate
680 IF RMS*PK*AVG*I*Z*PA THEN 860
690 IF PK=0 AND RMS<>0      THEN PK=RMS*SQR(2):GOTO 670
700 IF RMS=0 AND PK<>0      THEN RMS=PK/SQR(2):GOTO 670
710 IF RMS=0 AND PA*Z<>0    THEN RMS=SQR(PA*Z):GOTO 670
720 IF RMS=0 AND PA*I<>0    THEN RMS=PA/I:GOTO 670
730 IF AVG=0 AND PK<>0      THEN AVG=2/PI*PK:GOTO 670
740 IF PK=0 AND AVG<>0      THEN PK=PI/2*AVG:GOTO 670
750 IF RMS=0 AND I*Z<>0     THEN RMS=I*Z:GOTO 670
760 IF I=0 AND RMS*Z<>0     THEN I=RMS/Z:GOTO 670
770 IF I=0 AND PA*Z<>0      THEN I=SQR(PA/Z):GOTO 670
780 IF I=0 AND PA*RMS<>0    THEN I=PA/RMS:GOTO 670
790 IF Z=0 AND RMS*I<>0     THEN Z=RMS/I:GOTO 670
800 IF Z=0 AND PA*I<>0      THEN Z=PA/I^2:GOTO 670
810 IF Z=0 AND PA*RMS<>0    THEN Z=RMS^2/PA:GOTO 670
820 IF PA=0 AND I*Z<>0      THEN PA=I^2*Z:GOTO 670
830 IF PA=0 AND RMS*Z<>0    THEN PA=RMS^2/Z:GOTO 670
840 IF PA=Z AND RMS*I<>0    THEN PA=RMS*I:GOTO 670
850 RETURN
860 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
870 PRINT " AC Voltage (RMS).........";USING U1$;RMS
880 PRINT " AC Voltage (average).....";USING U1$;AVG
890 PRINT " AC Voltage (peak)........";USING U1$;PK
900 PRINT " Current (amperes)........";USING U2$;I
910 PRINT " Impedance (ohms).........";USING U1$;Z
920 PRINT " Volt-Amperes (watts).....";USING U1$;PA
930 PRINT UL$;
940 GOTO 2940
950 '
960 '.....impedance, power factor & phase angle
970 CLS
980 COLOR 15,2
990 PRINT " IMPEDANCE, POWER FACTOR & PHASE ANGLE "
1000 COLOR 1,0
1010 PRINT STRING$(80,"�");
1020 COLOR 7,0
1030 PRINT " If you do not have the data requested, press <ENTER>...
1040 PRINT UL$;
1050 '
1060 IF Z THEN 1090
1070 INPUT " ENTER: Impedance (ohms)................Z= ";Z
1080 GOSUB 1410
1090 IF R THEN 1120
1100 INPUT " ENTER: DC resistance (ohms)............R= ";R
1110 GOSUB 1410
1120 IF XL THEN 1150
1130 INPUT " ENTER: Inductive reactance (ohms).....Xl= ";XL
1140 GOSUB 1410
1150 IF XC THEN 1180
1160 INPUT " ENTER: Capacitive reactance (ohms)....Xc= ";XC
1170 GOSUB 1410
1180 IF E THEN 1210
1190 INPUT " ENTER: Volts (RMS).....................E= ";E
1200 GOSUB 1410
1210 IF I THEN 1240
1220 INPUT " ENTER: Current (amperes)...............I= ";I
1230 GOSUB 1410
1240 IF PA THEN 1270
1250 INPUT " ENTER: Apparent power (volt-amperes)..Pa= ";PA
1260 GOSUB 1410
1270 IF PR THEN 1300
1280 INPUT " ENTER: True power (watts).............Pr= ";PR
1290 GOSUB 1410
1300 IF PH THEN 1340
1310 INPUT " ENTER: Phase angle (degrees)..........Ph= ";PH
1320 PH=PH*PI/180      'radians
1330 GOSUB 1410
1340 IF PF THEN 1380
1350 INPUT " ENTER: Power Factor (from 0 to 1).....Pf= ";PF
1360 IF PF>1 THEN 1350
1370 GOSUB 1410
1380 VIEW PRINT 5 TO 24:CLS:VIEW PRINT:LOCATE 5
1390 GOTO 1060
1400 '
1410 '.....calculate
1420 IF Z=0 AND R*XL*XC<>0     THEN Z=SQR(R^2+(XL-XC)^2):GOTO 1410
1430 IF Z=0 AND R*PF<>0        THEN Z=R/PF:GOTO 1410
1440 IF R=0 AND Z*XL*XC<>0     THEN R=SQR(Z^2-(XL-XC)^2):GOTO 1410
1450 IF R=0 AND I*PR<>0        THEN R=PR/I^2:GOTO 1410
1460 IF R=0 AND Z*PF<>0        THEN R=Z*PF:GOTO 1410
1470 IF E=0 AND I*Z<>0         THEN E=I*Z:GOTO 1410
1480 IF E=0 AND I*PA<>0        THEN E=PA/I:GOTO 1410
1490 IF I=0 AND E*Z<>0         THEN I=E/Z:GOTO 1410
1500 IF I=0 AND E*PA<>0        THEN I=PA/E:GOTO 1410
1510 IF I=0 AND R*PR<>0        THEN I=SQR(PR/R):GOTO 1410
1520 IF PA=0 AND E*I<>0        THEN PA=E*I:GOTO 1410
1530 IF PA=0 AND PF*PR<>0      THEN PA=PR/PF:GOTO 1410
1540 IF PR=0 AND I*R<>0        THEN PR=I^2*R:GOTO 1410
1550 IF PR=0 AND PF*PA<>0      THEN PR=PF*PA:GOTO 1410
1560 IF PF=0 AND PR*PA<>0      THEN PF=PR/PA:GOTO 1410
1570 IF PF=0 AND R*Z<>0        THEN PF=R/Z:GOTO 1410
1580 IF PF=0 AND PH<>0         THEN PF=COS(PH):GOTO 1410
1590 IF PH=0 AND XL*XC*R<>0    THEN PH=ATN((XL-XC)/R):GOTO 1410
1600 IF R*XL*XC*I*PF THEN 1650
1610 IF R*Z*PF THEN 1650
1620 IF PH*PF THEN 1650
1630 RETURN
1640 '
1650 '.....print results
1660 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1670 IF Z  THEN PRINT " Impedance (ohms)...............";USING U1$;Z
1680 IF R  THEN PRINT " DC resistance (ohms)...........";USING U1$;R
1690 IF XL THEN PRINT " Inductive reactance (ohms).....";USING U1$;XL
1700 IF XC THEN PRINT " Capacitive reactance (ohms)....";USING U1$;XC
1710 IF E  THEN PRINT " AC Voltage (RMS)...............";USING U1$;E
1720 IF I  THEN PRINT " Current (amperes)..............";USING U2$;I
1730 IF PA THEN PRINT " Apparent power (volt-amperes)..";USING U1$;PA
1740 IF PR THEN PRINT " True power (watts).............";USING U1$;PR
1750 IF PF THEN PRINT " Power factor...................";USING U2$;PF
1760 IF PH THEN PH=PH*180/PI
1770 IF PH THEN PRINT " Phase angle (degrees)..........";USING U2$;PH
1780 PRINT UL$;
1790 GOTO 2940
1800 '
1810 '.....reactance, frequency, capacitance & inductance
1820 CLS
1830 COLOR 15,2:PRINT " REACTANCE, FREQUENCY, CAPACITANCE & INDUCTANCE "
1840 COLOR 1,0:PRINT STRING$(80,"�");
1850 COLOR 7,0
1860 IF X THEN 1890
1870 INPUT " ENTER: Reactance (ohms).......X= ";X
1880 GOSUB 2010
1890 IF F THEN 1920
1900 INPUT " ENTER: Frequency (MHz)........F= ";F
1910 GOSUB 2010
1920 IF C THEN 1950
1930 INPUT " ENTER: Capacitance (pF).......C= ";C
1940 GOSUB 2010
1950 IF U THEN 1980
1960 INPUT " ENTER: Inductance (�H)........U= ";U
1970 GOSUB 2010
1980 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1990 GOTO 1860
2000 '
2010 '.....calculate
2020 IF U=0 AND C*F<>0 THEN U=25330/F^2/C:GOTO 2010
2030 IF U=0 AND X*F<>0 THEN U=X/(2*PI*F):GOTO 2010
2040 IF C=0 AND F*U<>0 THEN C=25330/F^2/U:GOTO 2010
2050 IF F=0 AND C*U<>0 THEN F=SQR(25330/C/U):GOTO 2010
2060 IF F=0 AND X*U<>0 THEN F=X/(2*PI*U):GOTO 2010
2070 IF F=0 AND X*C<>0 THEN F=10^6/(2*PI*X*C):GOTO 2010
2080 IF X=0 AND F*C<>0 THEN X=10^6/(2*PI*F*C):GOTO 2010
2090 IF C=0 AND F*X<>0 THEN C=10^6/(2*PI*F*X):GOTO 2010
2100 IF U*C*F*X<>0 THEN 2130
2110 RETURN
2120 '
2130 '.....print results
2140 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
2150 PRINT " Reactance (ohms)..........";USING U2$;X
2160 PRINT " Frequency (MHz)...........";USING U2$;F
2170 PRINT " Capacitance (pF)..........";USING U2$;C
2180 PRINT " Inductance (�H)...........";USING U2$;U
2190 PRINT UL$;
2200 X=0:F=0:C=0:U=0
2210 GOTO 2940
2220 '
2230 '.....equivalant voltages
2240 CLS
2250 COLOR 15,2
2260 PRINT " RELATIVE A.C. VOLTAGES "
2270 COLOR 1,0
2280 PRINT STRING$(80,"�");
2290 COLOR 7,0
2300 PRINT " Press number in < > to select KNOWN voltage:"
2310 PRINT UL$;
2320 PRINT "   <1> Peak"
2330 PRINT "   <2> Peak-to-Peak"
2340 PRINT "   <3> RMS (Root Mean Square)"
2350 PRINT "   <4> Average"
2360 PRINT UL$;
2370 Z$=INKEY$
2380 IF Z$="1"THEN 2440
2390 IF Z$="2"THEN 2520
2400 IF Z$="3"THEN 2600
2410 IF Z$="4"THEN 2680
2420 GOTO 2370
2430 '
2440 '.....Peak known
2450 INPUT " ENTER: Known PEAK voltage";PK
2460 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
2470 PP=2*PK
2480 RMS=PK/SQR(2)
2490 AVG=2/PI*PK
2500 GOTO 2760
2510 '
2520 '.....Peak-to-Peak known
2530 INPUT " ENTER: Known PEAK-to-PEAK voltage";PP
2540 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
2550 PK=PP/2
2560 RMS=PP/2/SQR(2)
2570 AVG=PP/PI
2580 GOTO 2760
2590 '
2600 '.....RMS known
2610 INPUT " ENTER: Known RMS voltage";RMS
2620 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
2630 PK=RMS*SQR(2)
2640 PP=2*PK
2650 AVG=PP/PI
2660 GOTO 2760
2670 '
2680 '.....AVERAGE known
2690 INPUT " ENTER: Known AVERAGE voltage";AVG
2700 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
2710 PK=PI/2*AVG
2720 PP=2*PK
2730 RMS=PK/SQR(2)
2740 GOTO 2760
2750 '
2760 '.....display equivalent values
2770 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
2780 PRINT " PEAK voltage............";USING U2$;PK
2790 PRINT " PEAK-to-PEAK voltage....";USING U2$;PP
2800 PRINT " RMS voltage.............";USING U2$;RMS
2810 PRINT " AVERAGE voltage.........";USING U2$;AVG
2820 PRINT UL$;
2830 GOTO 2940
2840 '
2850 '.....error trap
2860 BEEP:CLS:LOCATE 3
2870 '
2880 PRINT " You may have entered something that is not valid or possible...."
2890 PRINT
2900 PRINT " Press any key to start over....."
2910 IF INKEY$=""THEN 2910
2920 CLS:GOTO 2960
2930 '
2940 '.....end
2950 GOSUB 2990
2960 GOTO 120   'start
2970 END
2980 '
2990 'PRT
3000 KEY OFF:GOSUB 3070:LOCATE 25,5:COLOR 0,2
3010 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
3020 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
3030 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 3030 ELSE GOSUB 3070
3040 IF Z$="3"THEN RETURN
3050 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
3060 IF Z$="2"THEN LPRINT CHR$(12) ELSE 3000
3070 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

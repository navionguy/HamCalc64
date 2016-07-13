1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$
10 'DSGNRLC - Resistor/Inductor/Capacitor Circuits  27 SEP 96 rev.11 APR 97
20 CLS:KEY OFF:COLOR 7,0,1
30 
40 '
50 '.....start
60 CLS:N=0:R=0:S=0:C=0:F=0:X=0:V=0:A=0:T=0:W=0
70 IF CKT$=""THEN RUN"dsgnmenu"
80 PRINT " RESISTOR/INDUCTOR/CAPACITOR CIRCUITS"
90 PRINT UL$;
100 '
110 '.....display diagrams
120 LOCATE 3:K=17:GOSUB 260
130 LOCATE 3:K=42:GOSUB 390
140 LOCATE 12:K=17:GOSUB 520
150 LOCATE 12:K=42:GOSUB 650
160 PRINT UL$;
170 COLOR 0,7:LOCATE ,17
180 PRINT "  Press a Fig. number to continue or 0 to EXIT  ";
190 COLOR 7,0
200 Z$=INKEY$:IF Z$=""THEN 200
210 IF ASC(Z$)<48 OR ASC(Z$)>52 THEN 200
220 IF Z$="0"THEN CHAIN"dsgnmenu"
230 FIG=VAL(Z$):GOTO 840
240 GOTO 200
250 '
260 '.....fig. 1
270 COLOR 0,7
280 LOCATE ,K:PRINT "  I-¯      R1          "
290 LOCATE ,K:PRINT "  ÚÄÄÄÄÄÄ\/\/\ÄÄÄÄÄÄ¿  "
300 LOCATE ,K:PRINT "  ³        L1       ³  "
310 LOCATE ,K:PRINT " ÄÅÄÄÄÄÄÄïïïïïÄÄÄÄÄÄÅÄ "
320 LOCATE ,K:PRINT "  ³        C1       ³  "
330 LOCATE ,K:PRINT "  ÀÄÄÄÄÄÄÄÄ×ÄÄÄÄÄÄÄÄÙ  "
340 LOCATE ,K:PRINT "                       "
350 LOCATE ,K:PRINT "        Fig. 1         "
360 COLOR 7,0
370 RETURN
380 '
390 '.....fig. 2
400 COLOR 0,7
410 LOCATE ,K:PRINT "  I-¯                  "
420 LOCATE ,K:PRINT "                       "
430 LOCATE ,K:PRINT "      R1     L1   C1   "
440 LOCATE ,K:PRINT " ÄÄÄ\/\/\ÄÄïïïïïÄÄ×ÄÄÄ "
450 LOCATE ,K:PRINT "                       "
460 LOCATE ,K:PRINT "                       "
470 LOCATE ,K:PRINT "                       "
480 LOCATE ,K:PRINT "        Fig. 2         "
490 COLOR 7,0
500 RETURN
510 '
520 '.....fig. 3
530 COLOR 0,7
540 LOCATE ,K:PRINT "  I-¯  R1      L1      "
550 LOCATE ,K:PRINT "  ÚÄÄ\/\/\ÄÄÄïïïïïÄÄ¿  "
560 LOCATE ,K:PRINT "  ³                 ³  "
570 LOCATE ,K:PRINT " Ä´                 ÃÄ "
580 LOCATE ,K:PRINT "  ³        C1       ³  "
590 LOCATE ,K:PRINT "  ÀÄÄÄÄÄÄÄÄ×ÄÄÄÄÄÄÄÄÙ  "
600 LOCATE ,K:PRINT "                       "
610 LOCATE ,K:PRINT "        Fig. 3         "
620 COLOR 7,0
630 RETURN
640 '
650 '.....fig. 4
660 COLOR 0,7
670 LOCATE ,K:PRINT "  I-¯  R1      L1      "
680 LOCATE ,K:PRINT "  ÚÄÄ\/\/\ÄÄÄïïïïïÄÄ¿  "
690 LOCATE ,K:PRINT "  ³                 ³  "
700 LOCATE ,K:PRINT " Ä´                 ÃÄ "
710 LOCATE ,K:PRINT "  ³    R2      C1   ³  "
720 LOCATE ,K:PRINT "  ÀÄÄ\/\/\ÄÄÄÄÄ×ÄÄÄÄÙ  "
730 LOCATE ,K:PRINT "                       "
740 LOCATE ,K:PRINT "        Fig. 4         "
750 COLOR 7,0
760 RETURN
770 '
780 '.....voltage indicator
790 COLOR 0,7
800 LOCATE ,K:PRINT "  ³®ÄÄÄÄÄÄ V ÄÄÄÄÄÄ¯³  "
810 COLOR 7,0
820 RETURN
830 '
840 '.....menu
850 VIEW PRINT 3 TO 24:CLS:VIEW PRINT
860 K=30:LOCATE 3
870 IF FIG=1 THEN GOSUB 260:GOTO 920
880 IF FIG=2 THEN GOSUB 390:GOTO 920
890 IF FIG=3 THEN GOSUB 520:GOTO 920
900 IF FIG=4 THEN GOSUB 650:GOTO 920
910 '
920 PRINT UL$;
930 PRINT " Do you want an (i)mpedance or (o)hm's law calculation?   (i/o)"
940 Z$=INKEY$:IF Z$=""THEN 940
950 IF Z$="i"THEN 980
960 IF Z$="o"THEN K=30:LOCATE 9:GOSUB 780:GOTO 990
970 GOTO 940
980 '
990 VIEW PRINT 12 TO 24:CLS:VIEW PRINT:LOCATE 12
1000 IF FIG=1 AND Z$="i"THEN 1090
1010 IF FIG=2 AND Z$="i"THEN 1160
1020 IF FIG=3 AND Z$="i"THEN 1230
1030 IF FIG=4 AND Z$="i"THEN 1300
1040 IF FIG=1 AND Z$="o"THEN 1380
1050 IF FIG=2 AND Z$="o"THEN 1460
1060 IF FIG=3 AND Z$="o"THEN 1540
1070 IF FIG=4 AND Z$="o"THEN 1300
1080 '
1090 '.....fig.1 impedance
1100 GOSUB 1710
1110 GOSUB 1890
1120 GOSUB 2060
1130 IF Z$="1" THEN 1090
1140 GOTO 2350
1150 '
1160 '.....fig.2 impedance
1170 GOSUB 1710
1180 GOSUB 1950
1190 GOSUB 2060
1200 IF Z$="1" THEN 1160
1210 GOTO 2350
1220 '
1230 '.....fig.3 impedance
1240 GOSUB 1710
1250 GOSUB 2000
1260 GOSUB 2060
1270 IF Z$="1" THEN 1230
1280 GOTO 2350
1290 '
1300 '.....fig.4 impedance
1310 INPUT " ENTER: Resistance R2 in ohms..................";S
1320 GOSUB 1710
1330 GOSUB 2000
1340 GOSUB 2060
1350 IF Z$="1" THEN 1300
1360 GOTO 2350
1370 '
1380 '.....fig.1 ohm's law
1390 GOSUB 1710
1400 GOSUB 1890
1410 GOSUB 1810
1420 GOSUB 2060
1430 IF Z$="1" THEN 1380
1440 GOTO 2350
1450 '
1460 '.....fig.2 ohm's law
1470 GOSUB 1710
1480 GOSUB 1950
1490 GOSUB 1810
1500 GOSUB 2060
1510 IF Z$="1" THEN 1460
1520 GOTO 2350
1530 '
1540 '.....fig.3 ohm's law
1550 GOSUB 1710
1560 GOSUB 2000
1570 GOSUB 1810
1580 GOSUB 2060
1590 IF Z$="1" THEN 1460
1600 GOTO 2350
1610 '
1620 '.....fig.4 ohm's law
1630 INPUT " ENTER: Resistance R2 in ohms..................";S
1640 GOSUB 1710
1650 GOSUB 2000
1660 GOSUB 1810
1670 GOSUB 2060
1680 IF Z$="1" THEN 1620
1690 GOTO 2350
1700 '
1710 '.....input impedance data
1720 INPUT " ENTER: Resistance R1 in ohms.....................";R
1730 INPUT " ENTER: Capacitance C1 in pF......................";C:C=C*10^-12
1740 INPUT " ENTER: Inductance L1 in æH.......................";L:L=L*10^-6
1750 INPUT " ENTER: Frequency in MHz..........................";F:F=F*10^6
1760 X=1/(2*PI*F*C)   'reactance
1770 Y=2*PI*F*L
1780 D=Y-X
1790 RETURN
1800 '
1810 '.....input ohm's law data
1820 INPUT " ENTER: Peak Voltage V in volts (0 if unknown)....";V
1830 IF V=0 THEN 1840 ELSE A=V/Z:GOTO 1860
1840 INPUT " ENTER: Peak Current I in amps. (0 if unknown)....";A
1850 IF A=0 THEN 1810 ELSE V=A*Z
1860 W=ABS(V*A*COS(T)/2)
1870 RETURN
1880 '
1890 '.....fig.1 - impedance
1900 Z=R*Y*X/SQR(Y^2*X^2+R^2*D^2)
1910 T=ATN(R*D/(Y*X))
1920 T=T*180/PI
1930 RETURN
1940 '
1950 '.....fig.2 - impedance
1960 Z=SQR(R^2+D^2)
1970 T=ATN(D/R)
1980 RETURN
1990 '
2000 '.....fig.3 & 4 - impedance
2010 Z=SQR((R^2+Y^2)*(S^2+X^2)/((R+S)^2+D^2))
2020 T=ATN((X*(R^2+Y^2)-Y*(S^2+X^2))/(R*(S^2+X^2)+S*(R^2+Y^2)))
2030 T=T*180/PI
2040 RETURN
2050 '
2060 '.....screen display
2070 VIEW PRINT 12 TO 24:CLS:VIEW PRINT:LOCATE 12
2080 PRINT " Resistance R1.......................";USING U$;R;:PRINT " ohms"
2090 IF S=0 THEN 2110
2100 PRINT " Resistance R2.......................";USING U$;S;:PRINT " ohms"
2110 K=C*10^12
2120 PRINT " Capacitance C1......................";USING U$;K;:PRINT " pF"
2130 K=L*10^6
2140 PRINT " Inductance L1.......................";USING U$;K;:PRINT " æH"
2150 PRINT " Reactance X.........................";USING U$;X;:PRINT " ohms"
2160 PRINT " Impedance Z.........................";USING U$;Z;:PRINT " ohms"
2170 IF V*A THEN 2200
2180 PRINT " Phase Angle.........................";USING U$;T;:PRINT " degrees"
2190 GOTO 2230
2200 PRINT " Voltage V...........................";USING U$;V;:PRINT " volts"
2210 PRINT " Current I...........................";USING U$;A;:PRINT " amps"
2220 PRINT " Power Consumption...................";USING U$;W;:PRINT " watts"
2230 PRINT
2240 LN=CSRLIN
2250 COLOR 0,7:LOCATE LN,17
2260 PRINT " Press 1 to repeat calculation or 0 to continue "
2270 COLOR 7,0
2280 Z$=INKEY$:IF Z$=""THEN 2280
2290 IF Z$="0"THEN J=LN:GOTO 2320
2300 IF Z$="1"THEN J=12:GOTO 2320
2310 GOTO 2280
2320 VIEW PRINT J TO 24:CLS:VIEW PRINT:LOCATE 12
2330 RETURN
2340 '
2350 '.....end
2360 GOSUB 2390
2370 GOTO 60
2380 '
2390 'PRT
2400 KEY OFF:GOSUB 2470:LOCATE 25,5:COLOR 0,2
2410 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2420 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2430 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2430 ELSE GOSUB 2470
2440 IF Z$="3"THEN RETURN
2450 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2460 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2400
2470 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

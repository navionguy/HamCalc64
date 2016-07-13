1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'LOOPXMIT - Loop Antenna Equations - 09 DEC 2002
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PI=4*ATN(1)  '3.141593
70 DIM Y(8,12)
80 UL$=STRING$(80,205)
90 '
100 '.....start
110 CLS
120 COLOR 15,2,1
130 PRINT " Small Transmitting Loop Antennas";,TAB(57);;"by George Murphy VE3ERP ";
140 COLOR 1,0:PRINT STRING$(80,223);
150 COLOR 7,0
160 GOSUB 1980  'introduction
170 PRINT UL$
180 LOCATE CSRLIN-1
190 Z$=INKEY$:IF Z$=""THEN 190
200 IF Z$="0"THEN CLS:CHAIN GO$
210 IF Z$="1"THEN 230
220 GOTO 190
230 PRINT " Want dimensioms in metric or USA units?   (m/u)"
240 Z$= INKEY$:IF Z$=""THEN 240
250 IF Z$="u"THEN D$="in.":DX=2.539999:GOTO 290
260 IF Z$="m"THEN D$="cm.":DXM=1  :GOTO 290
270 GOTO 240
280 '
290 '.....data input
300 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
310 F=0:PL=0:L=0:P=0
320 INPUT " ENTER: Frequency in MHz";F:WL=300/F
330 PP=INT(10^4*8.500000E-02*WL+5.000000E-01)/10^4
340 IF D$="in."THEN L$="feet"ELSE L$="metres"
350 IF D$="cm."THEN P=PP ELSE P=PP/3.048000E-01
360 COLOR 0,7
370 COLOR 0,7:PRINT " NOTE: The perimeter should be as large as possible but ";
380 PRINT "must be ";
390 PRINT "less than "
400 PRINT INT(P*1.000000E+03)/1.000000E+03;L$;;" "
410 COLOR 7,0,0:PRINT " ENTER: Perimeter of loop in ";L$;:INPUT PL
420 IF D$="in."THEN PL=PL*3.048000E-01
430 IF PL>PP THEN BEEP:GOTO 410
440 PRINT " ENTER: Diameter of conductor (";D$;;")";:INPUT L
450 IF D$="in."THEN L=L*2.539999
460 INPUT " ENTER: Transmitter power, in watts";P
470 '
480 CLS
490 COLOR 0,7,0:PRINT " Single Turn Loop Transmitting Antennas ";:COLOR 7,0
500 PRINT UL$;
510 R=9.959999E-01*10^-6*SQR(F*10^6)/(L/2.539999)   'AC resistance in ohms per foot
520 TR=R*PL/3.048000E-01      'total AC resistance
530 XL=2*PI*LUH*F      'inductive reactance
540 PRINT USING "Frequency............ ####.### MHz";F
550 PRINT USING "Wavelength........... ####.### m. ";WL
560 PRINT USING "Loop perimeter....... ####.### m.  = ###.### ft.";PL,PL/3.048000E-01;
570 COLOR 0,7
580 PRINT USING " = #.### wavelength ";PL/WL;
590 COLOR 7,0
600 PRINT "(max.=.085)"
610 PRINT USING "Conductor diameter... ####.### cm. = ###.### in.";L,L/2.539999
620 PRINT USING "AC resistance........ ####.### ohms";TR
630 PRINT USING "Transmitter power.... ####.### watts";P
640 PRINT "Shape of loop.......................";
650 PRINT TAB(39);"TRIANGLE";TAB(51);"SQUARE";TAB(60);"HEXAGON";TAB(70);"OCTAGON"
660 '
670 '.....triangle
680 S=PL/3*100:Y(3,1)=S          'length of side in cm.
690 E1=6.000000E-03*S
700 E2=LOG(1.154700*S/2^L)
710 E3=1.348000E-01*2^L/S
720 LUH=E1*(E2+6.553300E-01+E3):Y(3,2)=LUH
730 XL=2*PI*F*LUH:Y(3,3)=XL      'inductance
740 M=S/100:A=M/2*SQR(M^2-(M^2/(2*M))^2):Y(3,4)=A   'area mý
750 RR=3.119999*10^4*(A/WL^2)^2:Y(3,5)=RR 'radiation resistance
760 Q=XL/(2*(RR+TR)):Y(3,6)=Q    'Q
770 EF=RR/(RR+TR)*100:Y(3,7)=EF  'efficiency
780 C=2.533028E+04/F^2/LUH:Y(3,8)=C  'capacitance
790 VC=SQR(P*XL*Q):Y(3,9)=VC     'voltage across capacitor
800 BW=F/Q*10^3:Y(3,10)=BW       'bandwidth in kHz
810 CC=S/8.659999E-01/3.047999E+01:Y(3,11)=CC   'circumscribed circle
820 IL=SQR(P*Q/XL):Y(3,12)=IL    'circulating current
830 '
840 '.....square
850 S=PL/4*100:Y(4,1)=S          'length of side in cm.
860 E1=8.000001E-03*S
870 E2=LOG(1.414199*S/2^L)
880 E3=3.332999E-01*2^L/S
890 LUH=E1*(E2+3.794200E-01+E3):Y(4,2)=LUH
900 XL=2*PI*F*LUH:Y(4,3)=XL      'inductance
910 A=(S/100)^2:Y(4,4)=A         'area mý
920 RR=3.119999*10^4*(A/WL^2)^2:Y(4,5)=RR 'radiation resistance
930 Q=XL/(2*(RR+TR)):Y(4,6)=Q    'Q
940 EF=RR/(RR+TR)*100:Y(4,7)=EF  'efficiency
950 C=2.533028E+04/F^2/LUH:Y(4,8)=C  'capacitance
960 VC=SQR(P*XL*Q):Y(4,9)=VC     'voltage across capacitor
970 BW=F/Q*10^3:Y(4,10)=BW       'bandwidth in kHz
980 CC=SQR(2*S^2)/3.047999E+01:Y(4,11)=CC 'circumscribed circle
990 IL=SQR(P*Q/XL):Y(4,12)=IL    'circulating current
1000 '
1010 '.....hexagon
1020 S=PL/6*100:Y(6,1)=S          'length of side in cm.
1030 E1=1.200000E-02*S
1040 E2=LOG(2*S/2^L)
1050 E3=1.348000E-01*2^L/S
1060 LUH=E1*(E2+6.553300E-01+E3):Y(6,2)=LUH
1070 XL=2*PI*F*LUH:Y(6,3)=XL      'inductance
1080 A=2.598000*(S/100)^2:Y(6,4)=A   'area mý
1090 RR=3.119999*10^4*(A/WL^2)^2:Y(6,5)=RR 'radiation resistance
1100 Q=XL/(2*(RR+TR)):Y(6,6)=Q    'Q
1110 EF=RR/(RR+TR)*100:Y(6,7)=EF  'efficiency
1120 C=2.533028E+04/F^2/LUH:Y(6,8)=C  'capacitance
1130 VC=SQR(P*XL*Q):Y(6,9)=VC     'voltage across capacitor
1140 BW=F/Q*10^3:Y(6,10)=BW       'bandwidth in kHz
1150 CC=2*S/3.047999E+01:Y(6,11)=CC      'circumscribed circle
1160 IL=SQR(P*Q/XL):Y(6,12)=IL    'circulating current
1170 '
1180 '.....octagon
1190 S=PL/8*100:Y(8,1)=S          'length of side in cm.
1200 E1=1.600000E-02*S
1210 E2=LOG(2.612999*S/2^L)
1220 E3=7.152999E-02*2^L/S
1230 LUH=E1*(E2+7.514299E-01+E3):Y(8,2)=LUH
1240 XL=2*PI*F*LUH:Y(8,3)=XL      'inductance
1250 A=4.828000*(S/100)^2:Y(8,4)=A   'area mý
1260 RR=3.119999*10^4*(A/WL^2)^2:Y(8,5)=RR 'radiation resistance
1270 Q=XL/(2*(RR+TR)):Y(8,6)=Q
1280 EF=RR/(RR+TR)*100:Y(8,7)=EF  'efficiency
1290 C=2.533028E+04/F^2/LUH:Y(8,8)=C  'capacitance
1300 VC=SQR(P*XL*Q):Y(8,9)=VC     'voltage across capacitor
1310 BW=F/Q*10^3:Y(8,10)=BW       'bandwidth in kHz
1320 CC=2*S*1.307000/3.047999E+01:Y(8,11)=CC 'circumscribed circle
1330 IL=SQR(P*Q/XL):Y(8,12)=IL    'circulating current
1340 '
1350 '.....display
1360 X$="#####.### #####.### #####.### #####.###"
1370 Y$="Length of each side (cm.)........... "+X$
1380 PRINT USING Y$;Y(3,1),Y(4,1),Y(6,1),Y(8,1)
1390 Y$="                    (in.)........... "+X$
1400 PRINT USING Y$;Y(3,1)/2.539999,Y(4,1)/2.539999,Y(6,1)/2.539999,Y(8,1)/2.539999
1410 Y$="Dia. of circumscribed circle (ft.).. "+X$
1420 PRINT USING Y$;Y(3,11),Y(4,11),Y(6,11),Y(8,11)
1430 Y$="Area enclosed by loop (metreý)...... "+X$
1440 PRINT USING Y$;Y(3,4),Y(4,4),Y(6,4),Y(8,4)
1450 Y$="Inductance (æH)..................... "+X$
1460 PRINT USING Y$;Y(3,2),Y(4,2),Y(6,2),Y(8,2)
1470 Y$="Inductive reactance (ohms).......... "+X$
1480 PRINT USING Y$;Y(3,3),Y(4,3),Y(6,3),Y(8,3)
1490 Y$="Radiation resistance (ohms)......... "+X$
1500 PRINT USING Y$;Y(3,5),Y(4,5),Y(6,5),Y(8,5)
1510 Y$="Q................................... "+X$
1520 PRINT USING Y$;Y(3,6),Y(4,6),Y(6,6),Y(8,6)
1530 Y$="Capacitance req'd to resonate (pF).. "+X$
1540 PRINT USING Y$;Y(3,8),Y(4,8),Y(6,8),Y(8,8)
1550 Y$="Voltage across capacitor............ "+X$
1560 PRINT USING Y$;Y(3,9),Y(4,9),Y(6,9),Y(8,9)
1570 Y$="Circulating current (amperes)....... "+X$
1580 PRINT USING Y$;Y(3,12),Y(4,12),Y(6,12),Y(8,12)
1590 Y$="Bandwidth (kHz)..................... "+X$
1600 PRINT USING Y$;Y(3,10),Y(4,10),Y(6,10),Y(8,10)
1610 Y$="Efficiency (%)...................... "+X$
1620 PRINT USING Y$;Y(3,7),Y(4,7),Y(6,7),Y(8,7)
1630 COLOR 0,7
1640 '
1650 '.....change values
1660 PRINT " Change (f)requency? (p)erimeter? (c)onductor? (w)atts? ";
1670 PRINT "or (q)uit?  (f/p/c/w/q) ";:COLOR 7,0
1680 Z$=INKEY$:IF Z$=""THEN 1680
1690 IF Z$="f"OR Z$="F"THEN 1750
1700 IF Z$="p"OR Z$="P"THEN 1770
1710 IF Z$="c"OR Z$="C"THEN 1790
1720 IF Z$="w"OR Z$="W"THEN 1800
1730 IF Z$="q"OR Z$="Y"THEN 2280
1740 GOTO 1680
1750 INPUT " ENTER: Frequency in MHz";F:WL=300/F
1760 IF PL>WL*8.500000E-02 THEN A$="f":GOTO 1830 ELSE 1960
1770 INPUT " ENTER: Perimeter of loop (in metres)";PL
1780 IF PL>WL*8.500000E-02 THEN A$="p":GOTO 1830 ELSE 1960
1790 INPUT " ENTER: Diameter of conductor, in centimetres";L:GOTO 1960
1800 INPUT " ENTER: Transmitter power, in watts";P:GOTO 1960
1810 GOTO 480
1820 '
1830 '.....value out of range
1840 VIEW PRINT 5 TO 24:CLS:VIEW PRINT:LOCATE 6
1850 PRINT "PERIMETER EXCEEDS 0.085 WAVELENGTH!"
1860 IF A$="f"THEN H=300/PL*8.500000E-02:GOTO 1890
1870 IF A$="p"THEN H=300*8.500000E-02/F:GOTO 1920
1880 GOTO 1860
1890 A$=" ##.### MHz too high for ##.### m. perimeter loop. "
1900 A$=A$+"Try ##.#### MHz or lower"
1910 PRINT USING A$;F,PL,H:GOTO 1750
1920 A$=" ###.### m. too long for ###.### MHz.loop. "
1930 A$=A$+"Try ##.#### m. or less"
1940 PRINT USING A$;PL,F,H:GOTO 1770
1950 IF A$="p"THEN PRINT PL;"m. too long. Try a shorter perimeter";:GOTO 1770
1960 GOTO 480
1970 '
1980 '.....intro
1990 PRINT "  This program solves the equations for the properties of small t";
2000 PRINT "ransmitting   "
2010 PRINT " single-turn loop antennas (The ARRL Antenna book, pp. 5-2 to 5-1";
2020 PRINT "0).           "
2030 PRINT "  The components in a resonated transmission loop are subject to ";
2040 PRINT "both high     "
2050 PRINT " current and voltages as a result of the large circulating curren";
2060 PRINT "ts found in   "
2070 PRINT " the high-Q tuned circuit formed by the antenna."
2080 PRINT "  An unfortunate consequence of having a small but high efficienc";
2090 PRINT "y transmitting"
2100 PRINT " loop is high Q, and therefore limited bandwidth."
2110 PRINT "  For maximum efficiency the perimeter of the loop and the conduc";
2120 PRINT "tor diameter  "
2130 PRINT " should both be as large as practicable."
2140 PRINT "  The current around the perimeter of the loop must be in phase. ";
2150 PRINT "When the      "
2160 PRINT " perimeter of the loop is more than about 0.085 wavelength, this ";
2170 PRINT "is no longer  "
2180 PRINT " true."
2190 PRINT "  The equations in this program are valid only for small transmit";
2200 PRINT "ting loops    "
2210 PRINT " having a perimeter length of 0.085 wavelength or less.          "
2220 PRINT
2230 PRINT " (Ref.The ARRL Antenna Book, 18th edition, page 5-4)."
2240 PRINT
2250 COLOR 0,7:LOCATE ,23:PRINT " Press 1 to continue or 0 the EXIT ";:COLOR 7,0
2260 RETURN
2270 '
2280 '.....end
2290 VIEW PRINT CSRLIN-1 TO 24:CLS:VIEW PRINT
2300 GOSUB 2320:GOTO 100
2310 '
2320 '.....PRT
2330 KEY OFF:GOSUB 2400:LOCATE 25,5:COLOR 0,2
2340 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2350 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2360 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2360 ELSE GOSUB 2400
2370 IF Z$="3"THEN RETURN
2380 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2390 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2330
2400 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

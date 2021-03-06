1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'TRANSTUB - Stubs - Open-Wire Lines - 20 JUL 95 rev.13 APR 97
20 IF EX$=""THEN EX$="EXIT"
30 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
40 
50 CLS:KEY OFF:COLOR 7,0,1
60 UL$=STRING$(80,205)
70 U1$="####.###"
80 PI=3.141592
90 DIM GA(40)   'AWG wire sizes
100 '
110 '.....AWG calculator
120 K=(4.600000E-01/4.999999E-03)^(1/39)    'increment factor
130 FOR Z=1 TO 40
140 KN=Z+3
150 GA(Z)=4.600000E-01/K^KN
160 NEXT Z
170 '
180 '.....start
190 CLS
200 COLOR 15,2
210 PRINT " STUBS for OPEN-WIRE TRANSMISSION LINE";
220 PRINT TAB(61);;"by ";CHR$(34);;"LB";CHR$(34);;" Cebik W4RNL ";
230 PRINT STRING$(80,32);
240 LOCATE CSRLIN-1,20:PRINT "edited for HAMCALC by George Murphy VE3ERP"
250 COLOR 1,0:PRINT STRING$(80,223);:COLOR 7,0
260 MG=7      'margin
270 PRINT TAB(MG);
280 PRINT "Inductive (shorted) and capacitive (open) transmission line stubs"
290 PRINT TAB(MG);
300 PRINT "less than 1/4 wavelength long are very useful for introducing"
310 PRINT TAB(MG);
320 PRINT "frequency-sensitive amounts of inductance (or inductive reactance)"
330 PRINT TAB(MG);
340 PRINT "or capacitance (or capacitive reactance) into circuits. Their main"
350 PRINT TAB(MG);
360 PRINT "use is in antenna work, where they serve to load antenna elements"
370 PRINT TAB(MG);
380 PRINT "(electrically lengthen or shorten them). They also serve as"
390 PRINT TAB(MG);
400 PRINT "inductors or capacitors in matching networks."
410 PRINT
420 PRINT TAB(MG);
430 PRINT "This program allows you to calculate the length of either an"
440 PRINT TAB(MG);
450 PRINT "inductive (shorted) or capacitive (open) stub by entering the"
460 PRINT TAB(MG);
470 PRINT "frequency of interest and a reactance value, or a capacitance"
480 PRINT TAB(MG);
490 PRINT "or inductance."
500 PRINT
510 PRINT TAB(MG);
520 PRINT "You may then enter either the known line impedance (Zo) or the"
530 PRINT TAB(MG);
540 PRINT "physical dimensions (wire size and spacing) of an open-wire"
550 PRINT TAB(MG);
560 PRINT "feedline (along with its velocity factor). The program will then"
570 PRINT TAB(MG);
580 PRINT "compute the stub length."
590 PRINT
600 COLOR 0,7:LOCATE CSRLIN,22
610 PRINT " Press 1 to continue or 0 to EXIT...."
620 COLOR 7,0
630 Z$=INKEY$:IF Z$=""THEN 630
640 IF Z$="0"THEN CLS:CHAIN GO$
650 IF Z$="1"THEN 680
660 GOTO 630
670 '
680 '.....units of measure
690 VIEW PRINT 4 TO 24:CLS:VIEW PRINT:LOCATE 4
700 PRINT " Press number in < > to choose standard units of measure:"
710 PRINT UL$;
720 PRINT "  < 1 > Metric"
730 PRINT "  < 2 > U.S.A./Imperial"
740 Z$=INKEY$
750 IF Z$="1"THEN UM=2.539999E+01:GOTO 780
760 IF Z$="2"THEN UM=1:GOTO 780
770 GOTO 740
780 GOTO 850
790 '
800 '.....format input line
810 LOCATE CSRLIN-1:PRINT SPC(7);
820 LOCATE CSRLIN,47:PRINT STRING$(7,".");USING U1$;Z;
830 RETURN
840 '
850 '.....clear screen
860 VIEW PRINT 4 TO 24:CLS:VIEW PRINT:LOCATE 4
870 '
880 INPUT " ENTER: Frequency of interest...................(MHz)";F
890 Z=F:GOSUB 800:PRINT " MHz";:FB=F*10^6
900 '
910 PRINT UL$;
920 LN=CSRLIN-1
930 PRINT" Press a number in < > to select a known value of the line @";F;;"MHz:"
940 PRINT UL$;
950 PRINT "  < 1 > Inductive Reactance"
960 PRINT "  < 2 > Capacitive Reactance"
970 PRINT "  < 3 > Inductance"
980 PRINT "  < 4 > Capacitance"
990 A$=INKEY$:IF A$=""THEN 990
1000 IF A$="1"THEN GOSUB 1060:GOTO 1140
1010 IF A$="2"THEN GOSUB 1060:GOTO 1220
1020 IF A$="3"THEN GOSUB 1060:GOTO 1090
1030 IF A$="4"THEN GOSUB 1060:GOTO 1170
1040 GOTO 990
1050 '
1060 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
1070 RETURN
1080 '
1090 '.....inductance to inductive reactance
1100 INPUT " ENTER: Inductance...............................(�H)";L
1110 Z=L:GOSUB 800:PRINT " �H"
1120 LB=L*9.999999E-07:LX=((2*PI)*(FB*LB)):T=1:GOTO 1250
1130 '
1140 INPUT " ENTER: Inductive reactance....................(ohms)";LX
1150 Z=LX:GOSUB 800:PRINT " �";:T=1:GOTO 1250
1160 '
1170 '.....capacitance to capacitive reactance
1180 INPUT " ENTER: Capacitance..............................(pF)";C
1190 Z=C:GOSUB 800:PRINT " pF"
1200 CB=C*(9.999999E-13):CX=1/((2*PI)*(FB*CB)):T=0:GOTO 1250
1210 '
1220 INPUT " ENTER: Capacitive reactance...................(ohms)";CX
1230 Z=CX:GOSUB 800:PRINT " �";:T=0:GOTO 1250
1240 '
1250 '.....stub construction
1260 PRINT UL$;
1270 LN=CSRLIN-1
1280 PRINT " Press number in < > to select known characteristics of line:"
1290 PRINT UL$;
1300 PRINT "  < 1 > Feedline characteristic impedance (Zo)"
1310 PRINT "  < 2 > Wire size and spacing of open-wire feedline"
1320 B$=INKEY$
1330 IF B$="1"THEN GOSUB 1060:GOTO 1370
1340 IF B$="2"THEN GOSUB 1060:GOTO 1430
1350 GOTO 1320
1360 '
1370 INPUT " ENTER: Impedance (Zo) of the line..............(ohms)";ZO
1380 Z=ZO:GOSUB 800:PRINT " �"
1390 INPUT " ENTER: Velocity factor (decimal) of the line.........";VF
1400 Z=VF:GOSUB 800
1410 IF T>0 THEN 1720 ELSE 1830
1420 '
1430 IF UM=1 THEN V$="in."ELSE V$="mm."
1440 PRINT " ENTER: Open-wire line spacing..................(";V$;;")";:INPUT SP
1450 Z=SP:GOSUB 800:PRINT " ";V$:SP=SP/UM         'sp=spacing in inches
1460 '
1470 PRINT UL$;
1480 LN=CSRLIN-1
1490 PRINT " Press number in < > to choose method of wire size selection:"
1500 PRINT UL$;
1510 PRINT "  < 1 > Diameter (";V$;;")"
1520 PRINT "  < 2 > AWG size"
1530 Z$=INKEY$
1540 IF Z$="1"THEN GOSUB 1060:GOTO 1580
1550 IF Z$="2"THEN GOSUB 1060:GOTO 1620
1560 GOTO 1530
1570 '
1580 PRINT " ENTER: Wire diameter...........................(";V$;;")";:INPUT Y
1590 Z=Y:GOSUB 800:PRINT " ";V$:DIA=Y/UM         'sp=spacing in inches
1600 GOTO 1680
1610 '
1620 INPUT " ENTER: Wire gauge.................................AWG #";Y
1630 Z=Y:GOSUB 800:LOCATE CSRLIN,54:PRINT USING "####";Y;:PRINT " AWG"
1640 DIA=GA(Y)
1650 PRINT "        Wire diameter................................";
1660 PRINT USING U1$;DIA*UM;:PRINT " ";V$
1670 '
1680 ZO=(276*4.342899E-01)*(LOG((2*SP)/DIA))
1690 INPUT " ENTER: Velocity factor (decimal) of line..............";VF
1700 Z=VF:GOSUB 800
1710 IF T>0 THEN 1720 ELSE 1830
1720 '.....inductive stub
1730 '
1740 IF ZO=0 THEN ZO=1.000000E-07
1750 LR=ATN(LX/ZO):LDG=(180*LR)/PI
1760 IF F=0 THEN F=1.000000E-07
1770 LFT=(VF*LDG)/(3.659999E-01*F)
1780 IF UM=2.539999E+01 THEN UMM=3.048000E-01:V$=" metres"ELSE UMM=1:V$=" feet"
1790 PRINT "        Length of SHORTED (inductive) STUB...........";
1800 PRINT USING U1$;LFT*UMM;:PRINT V$
1810 GOTO 1930
1820 '
1830 '.....capacitive stub
1840 IF CX=0 THEN CX=1.000000E-07
1850 LR=ATN(ZO/CX):LDG=(180*LR)/PI
1860 IF F=0 THEN F=1.000000E-07
1870 LFT=(VF*LDG)/(3.659999E-01*F)
1880 IF UM=2.539999E+01 THEN UMM=3.048000E-01:V$=" metres"ELSE UMM=1:V$=" feet"
1890 PRINT "        Length of OPEN (capacitive) STUB.............";
1900 PRINT USING U1$;LFT*UMM;:PRINT V$
1910 GOTO 1930
1920 '
1930 M=16
1940 IF V$=" metres"THEN V$=" m."
1950 LOCATE 11
1960 COLOR 0,7
1970 LOCATE CSRLIN,M:PRINT "                                                  "
1980 LOCATE CSRLIN,M:PRINT "    ��> ����������������������������������������  "
1990 LOCATE CSRLIN,M:PRINT "    �      ������������������������� <Ŀ          "
2000 LOCATE CSRLIN,M:PRINT " Line                                Open Stub    "
2010 LOCATE CSRLIN,M:PRINT "    �      ������������������������� <��          "
2020 LOCATE CSRLIN,M:PRINT "    ��> ����������������������������������������  "
2030 LOCATE CSRLIN,M:PRINT "                                                  "
2040 LOCATE CSRLIN,M:PRINT "           �����������������������į�             "
2050 LOCATE CSRLIN-1,M+17:PRINT USING U1$;LFT*UMM;:PRINT V$;" "
2060 LOCATE CSRLIN,M:PRINT "                                                  "
2070 COLOR 7,0
2080 PRINT
2090 PRINT TAB(7);
2100 PRINT "The line should be as close to horizontal as possible with the stub"
2110 PRINT TAB(7);
2120 PRINT "extending vertically from the line."
2130 IF T=0 THEN 2200
2140 COLOR 0,7
2150 LOCATE CSRLIN-10,M+36:PRINT "�"
2160 LOCATE CSRLIN,   M+36:PRINT "�Shorted Stub"
2170 LOCATE CSRLIN,   M+36:PRINT "�"
2180 COLOR 7,0
2190 '
2200 '....end
2210 GOSUB 2250
2220 GOTO 180
2230 END
2240 '
2250 'PRT
2260 KEY OFF:GOSUB 2330:LOCATE 25,5:COLOR 0,2
2270 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2280 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2290 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2290 ELSE GOSUB 2330
2300 IF Z$="3"THEN RETURN
2310 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2320 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2260
2330 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

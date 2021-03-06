1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'NMBRSIZE - numbered drills & screws - 03 JUN 95 rev.14 FEB 1998
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,1
50 DIM D$(105,2)
60 DIM G(38)
70 UL$=STRING$(80,205)
80 '
90 '.....start
100 CLS:RESTORE
110 COLOR 15,2
120 PRINT " NUMBERED DRILLS, SCREWS, TAPS & GAUGES (USA standard)";
130 PRINT TAB(57);;"by George Murphy VE3ERP ";
140 COLOR 1,0:PRINT STRING$(80,223);
150 COLOR 7,0
160 '
170 PRINT " Press number in < > to select:"
180 PRINT UL$;
190 PRINT "   < 1 >  Drill sizes"
200 PRINT "   < 2 >  Screw sizes & tap drill sizes"
210 PRINT "   < 3 >  Sheet metal gauges"
220 PRINT UL$;
230 PRINT "   < 0 >  EXIT"
240 Z$=INKEY$:IF Z$=""THEN 240
250 IF Z$="0"THEN CLS:RUN EX$
260 IF Z$="1"THEN GOSUB 320:GOTO 450
270 IF Z$="2"THEN 910
280 IF Z$="3"THEN GOSUB 320:GOTO 1530
290 GOTO 240
300 '
310 '.....numbered drills (79)
320 PRINT UL$;
330 LN=CSRLIN-2:VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
340 PRINT " Press number in < > to choose standard units of measure:"
350 PRINT UL$;
360 PRINT "   < 4 >  Metric"
370 PRINT "   < 5 >  U.S.A./Imperial"
380 Z$=INKEY$
390 IF Z$="4"THEN UM=1:GOTO 420
400 IF Z$="5"THEN UM=2:GOTO 420
410 GOTO 380
420 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
430 RETURN
440 '
450 '.....numbered drills (79)
460 DATA .0059, .0063, .0067, .0071, .0075, .0079, .0083, .0087, .0091, .0095
470 DATA .0100, .0105, .0110, .0115, .0120, .0125, .0130, .0135, .0145, .0400
480 DATA .0410, .0420, .0430, .0465, .0520, .0550, .0595, .0635, .0670, .0700
490 DATA .0730, .0760, .0785, .0810, .0820, .0860, .0890, .0935, .0960, .0980
500 DATA .0995, .1015, .1040, .1065, .1100, .1110, .1130, .1160, .1200, .1285
510 DATA .1360, .1405, .1440, .1470, .1495, .1520, .1540, .1570, .1590, .1610
520 DATA .1660, .1695, .1730, .1770, .1800, .1820, .1850, .1890, .1910, .1935
530 DATA .1960, .1990, .2010, .2040, .2055, .2090, .2130, .2210, .2280
540 '.....lettered drills (26)
550 DATA .234, .238, .242, .246, .250, .257, .261, .266, .272, .277, .281
560 DATA .290, .295, .302, .316, .323, .332, .339, .348, .358, .368, .377
570 DATA .386, .397, .404, .413
580 '
590 N=98
600 FOR Z=1 TO 105
610 IF Z<80 THEN N=N-1 :IF N=78 THEN N=60
620 IF Z=80 THEN N=64
630 IF Z>79 THEN N=N+1
640 IF Z<80 THEN N$=RIGHT$(STR$(N),LEN(STR$(N))-1)
650 IF Z>79 THEN N$=CHR$(N)
660 IF LEN(N$)<2 THEN N$=" "+N$
670 D$(Z,1)="#"+N$
680 READ Z$
690 M=VAL(Z$)*2.539999E+01:MM=INT(M*100+5.000000E-01)/100:M$=STR$(MM)
700 IF VAL(M$)>=1 AND LEN(M$)<5 THEN M$=M$+"0":GOTO 700
710 IF VAL(M$)<1 AND LEN(M$)<4 THEN M$=M$+"0":GOTO 700
720 IF LEN(M$)>6THEN M$=LEFT$(M$,6)
730 IF VAL(M$)<1 AND LEN(M$)<5 THEN M$=" "+M$:GOTO 700
740 IF LEN(M$)>5 THEN M$=LEFT$(M$,5)
750 IF UM=1 THEN D$(Z,2)=M$+" mm"
760 IF UM=2 THEN D$(Z,2)=" "+Z$+CHR$(34)
770 NEXT Z
780 '
790 '.....screen print
800 PRINT " NUMBERED DRILL DIAMETERS:"
810 FOR Z=1 TO 21
820 PRINT TAB( 2);D$(Z,1);;" =";D$(Z,2);
830 PRINT TAB(17);;"�";D$(Z+21,1);;" =";D$(Z+21,2);
840 PRINT TAB(33);;"�";D$(Z+42,1);;" =";D$(Z+42,2);
850 PRINT TAB(49);;"�";D$(Z+63,1);;" =";D$(Z+63,2);
860 PRINT TAB(66);;"�";D$(Z+84,1);;" =";D$(Z+84,2);
870 NEXT Z
880 GOTO 2150
890 PRINT " # 0 = .060 in. = 1.52 mm     #50 = .0700 in. = 1.78 mm"
900 '
910 '.....screw sizes
920 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
930 T=6
940 PRINT TAB(T);;"�";STRING$(68,196);;"�"
950 LOCATE CSRLIN-1,T+15:PRINT "�";STRING$(15,196);;"�"
960 PRINT TAB(T);;"� MACHINE SCREW� CLEARANCE HOLE";
970 PRINT "�          TAP LEADER DRILL           �"
980 '
990 PRINT TAB(T);;"�     SIZE     �    DRILL      ";
1000 PRINT "�   ( pitch P = threads per inch )    �"
1010 '
1020 PRINT TAB(T);;"�������������������������������";
1030 PRINT "�������������������������������������Ĵ"
1040 '
1050 PRINT TAB(T);;"�    � diameter�    � diameter ";
1060 PRINT "�  coarse thread   �   fine thread    �"
1070 '
1080 PRINT TAB(T);;"� no.���������Ĵ no.�����������";
1090 PRINT "�������������������������������������Ĵ"
1100 '
1110 PRINT TAB(T);;"�    � in.� mm �    � in. � mm ";
1120 PRINT "� P� no.� in. � mm � P� no.� in. � mm �"
1130 '
1140 PRINT TAB(T);;"�������������������������������";
1150 PRINT "�������������������������������������Ĵ"
1160 '
1170 PRINT TAB(T);;"� # 0�.060�1.52� #50�.0700�1.78";
1180 PRINT "�80�3/64�.0469�1.19� -�  - �  -  �  - �"
1190 '
1200 PRINT TAB(T);;"� # 1�.073�1.85� #46�.0810�2.06";
1210 PRINT "�64� #53�.0595�1.51�72� #53�.0595�1.51�"
1220 '
1230 PRINT TAB(T);;"� # 2�.086�2.18� #41�.0960�2.44";
1240 PRINT "�56� #50�.0700�1.78�64� #50�.0700�1.78�"
1250 '
1260 PRINT TAB(T);;"� # 3�.099�2.51� #35�.1100�2.79";
1270 PRINT "�48� #47�.0785�1.99�56� #45�.0820�2.08�"
1280 '
1290 PRINT TAB(T);;"� # 4�.112�2.84� #30�.1285�3.26";
1300 PRINT "�40� #43�.0890�2.26�48� #42�.0935�2.37�"
1310 '
1320 PRINT TAB(T);;"� # 5�.125�3.18� #29�.1360�3.45";
1330 PRINT "�40� #38�.1015�2.58�44� #37�.1040�2.64�"
1340 '
1350 PRINT TAB(T);;"� # 6�.138�3.51� #25�.1495�3.80";
1360 PRINT "�32� #36�.1065�2.71�40� #33�.1130�2.87�"
1370 '
1380 PRINT TAB(T);;"� # 8�.164�4.17� #16�.1770�4.50";
1390 PRINT "�32� #29�.1360�3.54�36� #29�.1360�3.54�"
1400 '
1410 PRINT TAB(T);;"� #10�.190�4.83� # 7�.2010�5.11";
1420 PRINT "�24� #25�.1495�3.80�32� #21�.1590�4.04�"
1430 '
1440 PRINT TAB(T);;"� #12�.216�5.49� # 1�.2280�5.79";
1450 PRINT "�24� #16�.1770�4.50�28� #14�.1820�4.62�"
1460 '
1470 PRINT TAB(T);;"� 1/4�.250�6.35� # H�.2660�6.76";
1480 PRINT "�20� # 7�.2010�5.11�28� # 3�.2130�5.41�"
1490 PRINT TAB(T);;"�������������������������������";
1500 PRINT "���������������������������������������"
1510 GOTO 2150
1520 '
1530 '.....sheet steel gauges
1540 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1550 TB=7
1560 PRINT TAB(TB);
1570 PRINT"``The thickness of steel sheets now is based upon a weight of 41.82"
1580 PRINT TAB(TB);
1590 PRINT "pounds per foot� per inch thick. This is known as Manufacturers'"
1600 PRINT TAB(TB);
1610 PRINT "Standard Gage for Sheet Steel.....thicknesses of aluminum, copper,"
1620 PRINT TAB(TB);
1630 PRINT "and copper-base alloys were formerly designated by the American or"
1640 PRINT TAB(TB);
1650 PRINT "Brown & Sharpe Wire Gage but now are specified in decimals or"
1660 PRINT TAB(TB);
1670 PRINT "fractions of an inch''.....(Machinery's Handbook, Revised 21st"
1680 PRINT TAB(TB);
1690 PRINT "Edition, page 465)."
1700 PRINT
1710 PRINT TAB(TB);
1720 PRINT "The following data is according to the Manufacturers' Standard Gage"
1730 PRINT TAB(TB);
1740 PRINT "for Sheet Steel........"
1750 GOSUB 2190
1760 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1770 '
1780 '.....pounds per square foot
1790 I=5/8      'increment
1800 W=1.062500E+01   'initial lbs/ft�
1810 FOR Z=3 TO 38
1820 IF Z=15 THEN I=5/16
1830 IF Z=17 THEN I=1/4
1840 IF Z=21 THEN I=1/8
1850 IF Z=27 THEN I=1/16
1860 IF Z=32 THEN I=1/32
1870 IF Z=37 THEN I=1/64
1880 W=W-I
1890 G(Z)=W
1900 NEXT Z
1910 '
1920 '.....screen display
1930 IF UM=1 THEN T=4.882429:T$="Ga.     Kg/m�    Thick"
1940 IF UM=2 THEN T=1:       T$="Ga.    Lb/Ft�    Thick"
1950 U$=STRING$(25,196)
1960 PRINT " SHEET STEEL GAUGES:"
1970 PRINT " �������������������"
1980 PRINT TAB(10);T$;TAB(47);T$
1990 PRINT TAB(10);U$;TAB(47);U$
2000 FOR Z=3 TO 20
2010 N$=RIGHT$(STR$(Z),LEN(STR$(Z))-1)
2020 IF VAL(N$)<10 THEN N$=" "+N$
2030 PRINT TAB(10);;"#";N$;
2040 PRINT USING "#####.####";G(Z)*T;
2050 IF UM=1 THEN PRINT USING "#####.###";G(Z)/4.181999E+01*2.539999E+01;:PRINT " mm";
2060 IF UM=2 THEN PRINT USING "####.####";G(Z)/4.181999E+01;:PRINT " in.";
2070 N$=RIGHT$(STR$(Z+18),2)
2080 PRINT TAB(47);;"#";N$;
2090 PRINT USING "#####.####";G(Z+18)*T;
2100 IF UM=1 THEN PRINT USING "#####.###";G(Z+18)/4.181999E+01*2.539999E+01;:PRINT " mm";
2110 IF UM=2 THEN PRINT USING "####.####";G(Z+18)/4.181999E+01;:PRINT " in.";
2120 NEXT Z
2130 GOTO 2150
2140 '
2150 '.....end
2160 GOSUB 2190
2170 GOTO 90   'start
2180 '
2190 'PRT
2200 KEY OFF:GOSUB 2270:LOCATE 25,5:COLOR 0,2
2210 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2220 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2230 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2230 ELSE GOSUB 2270
2240 IF Z$="3"THEN RETURN
2250 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2260 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2200
2270 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

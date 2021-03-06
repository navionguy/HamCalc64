1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$
10 'DSGNRC - Resistor/Capacitor Circuits  22 NOV 95 rev.11 APR 97
20 CLS:KEY OFF:COLOR 7,0,1
30 
40 R$="\/\/\"
50 C$="�����"
60 L$="�����"
70 '
80 '.....start
90 CLS:D=0:N=0
100 IF CKT$=""THEN CLS:RUN"dsgnmenu"
110 A$=LEFT$(CKT$,1):B$=RIGHT$(CKT$,1)
120 IF CKT$="RC"THEN AA$=R$:BB$=C$
130 IF CKT$="RL"THEN AA$=R$:BB$=L$
140 IF CKT$="LC"THEN AA$=L$:BB$=C$
150 PRINT T$
160 PRINT UL$;
170 K=23:LOCATE 3:GOSUB 280
180 K=42:LOCATE 3:GOSUB 400
190 PRINT UL$;
200 GOTO 520    'start
210 '
220 '.....invalid input
230 BEEP:COLOR 0,7
240 PRINT " YOUR DATA INPUT IS NOT VALID ";
250 COLOR 7,0:PRINT "....Press any key to start over...."
260 IF INKEY$=""THEN 260 ELSE 80
270 '
280 '.....series diagram
290 COLOR 0,7
300 LOCATE ,K:PRINT "                 "
310 LOCATE ,K:PRINT "      ";A$;;"     ";B$;;"    "
320 LOCATE ,K:PRINT " ���";AA$;;"�";BB$;;"� "
330 LOCATE ,K:PRINT "       I-�       "
340 LOCATE ,K:PRINT "                 "
350 LOCATE ,K:PRINT " ������ V ���į� "
360 LOCATE ,K:PRINT "   S E R I E S   "
370 COLOR 7,0
380 RETURN
390 '
400 '.....parallel diagram
410 COLOR 0,7
420 LOCATE ,K:PRINT "        ";A$;;"        "
430 LOCATE ,K:PRINT "   ���";AA$;;"�Ŀ   "
440 LOCATE ,K:PRINT " �Ĵ   I-�   ��� "
450 LOCATE ,K:PRINT "   ���";BB$;;"���   "
460 LOCATE ,K:PRINT "        ";B$;;"        "
470 LOCATE ,K:PRINT " ������ V ���į� "
480 LOCATE ,K:PRINT " P A R A L L E L "
490 COLOR 7,0
500 RETURN
510 '
520 PRINT " Press number in ( ) for:"
530 PRINT UL$;
540 IF CKT$="LC"THEN D$="/Resonance"ELSE D$=""
550 IF CKT$="LC"THEN E$="-"ELSE E$="Series Phase Angle"
560 PRINT "  (1) Impedance";D$
570 PRINT "  (2) Ohm's Law"
580 PRINT "  (3) ";E$
590 PRINT "  (4) Series Voltage Divider"
600 PRINT
610 PRINT "  (0) QUIT"
620 Z$=INKEY$:IF Z$=""THEN 620
630 N=VAL(Z$)
640 IF CKT$="LC"AND N=3 THEN 220
650 IF N=1 THEN 720
660 IF N=2 THEN 1130
670 IF N=3 THEN 2250
680 IF N=4 THEN 2610
690 IF N=0 THEN CLS:CHAIN"dsgnmenu"
700 GOTO 620
710 '
720 CLS '.....impedance / resonance
730 IF CKT$="LC"THEN PRINT TAB(30);"IMPEDANCE / RESONANCE";:GOTO 750
740 PRINT TAB(36);"IMPEDANCE"
750 PRINT UL$;
760 LOCATE 3:K=23:GOSUB 280
770 LOCATE 3:K=42:GOSUB 400
780 PRINT UL$;
790 COLOR 0,7
800 IF CKT$="LC"THEN 820
810 PRINT " Enter Frequency, ";A$;;", and ";B$;;" ";:GOTO 830
820 PRINT " Enter Frequency, C and L for Impedance, or any two for Resonance "
830 COLOR 7,0
840 PRINT
850 GOSUB 3310  'inputs
860 IF CKT$="RC"THEN X=1/(2*PI*F*C):XC=X:GOSUB 910
870 IF CKT$="RL"THEN X=2*PI*F*L:XL=X:GOSUB 910
880 IF CKT$="LC"THEN GOSUB 1000
890 GOTO 3440   'screen print
900 '
910 '.....RC & RL
920 IF CKT$="RC"AND F*R*C=0 THEN 220
930 IF CKT$="RL"AND F*R*L=0 THEN 220
940 SIMP=SQR(R^2+X^2)      'series impedance
950 SPA=ATN(X/R)*180/PI    'series phase angle
960 PIMP=R*X/SQR(R^2+X^2)  'parallel impedance
970 PPA=ATN(R/X)*180/PI    'parallel phase angle
980 RETURN
990 '
1000 '.....LC
1010 IF F*C*L=0 THEN 1070
1020 Y=2*PI*F*L:XL=Y
1030 X=1/(2*PI*F*C):XC=X
1040 D=ABS(Y-X)
1050 SIMP=D:PIMP=Y*X/D:IF D=0THEN PIMP=0
1060 GOTO 1110
1070 IF F=0 THEN F=1/(2*PI*SQR(L*C)):GOTO 1100
1080 IF C=0 THEN C=1/(F^2*4*PI^2*L):GOTO 1100
1090 IF L=0 THEN L=1/(F^2*4*PI^2*C):GOTO 1100
1100 XC=1/(2*PI*F*C):XL=2*PI*F*L
1110 RETURN
1120 '
1130 CLS '.....ohm's law
1140 PRINT TAB(36);"OHM'S LAW"
1150 PRINT UL$;
1160 LOCATE 3:K=23:GOSUB 280
1170 LOCATE 3:K=42:GOSUB 400
1180 PRINT UL$;
1190 IF CKT$="LC"THEN 2040
1200 COLOR 0,7
1210 PRINT " Required data are Frequency, ";A$;;", ";B$;;", V and I. Enter 0 for";
1220 PRINT " "
1230 PRINT " unknown value and actual values for all the others.     "
1240 COLOR 7,0
1250 PRINT
1260 GOSUB 3310   'inputs
1270 IF F*C<>0 THEN X=1/(2*PI*F*C):XC=X
1280 IF F*L<>0 THEN X=2*PI*F*L:XL=X
1290 INPUT " ENTER: Voltage V (peak).............................(volts)";V
1300 INPUT " ENTER: Current I (peak)............................. (amps)";A
1310 COLOR 0,7
1320 '
1330 LOCATE CSRLIN,8
1340 PRINT " Press 1 for Series circuit, 2 for Parallel circuit....."
1350 COLOR 7,0
1360 Z$=INKEY$:IF Z$=""THEN 1360
1370 GOSUB 1490
1380 N=VAL(Z$)
1390 IF N=1 AND CKT$="RC"THEN GOSUB 1520:GOTO 1430
1400 IF N=2 AND CKT$="RC"THEN GOSUB 1650:GOTO 1430
1410 IF N=1 AND CKT$="RL"THEN GOSUB 1780:GOTO 1430
1420 IF N=2 AND CKT$="RL"THEN GOSUB 1910:GOTO 1430
1430 CVPA=D*180/PI
1440 W=V*A*COS(D)/2
1450 XC=1/(2*PI*F*C)
1460 XL=2*PI*F*L
1470 GOTO 3440
1480 '
1490 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1500 RETURN
1510 '
1520 '.....RC series
1530 LOCATE 3:K=32:GOSUB 280
1540 PRINT UL$;
1550 IF V=0 THEN V=A*SQR(R^2+X^2)
1560 IF A=0 THEN A=V/SQR(R^2+X^2)
1570 IF R=0 AND V<A*X THEN 2220
1580 IF R=0 THEN R=SQR(V^2-A^2*X^2)/A
1590 IF(C=0 OR F=0)AND V<=A*R THEN 2220
1600 IF C=0 THEN C=A/(2*PI*F*SQR(V^2-A^2*R^2))
1610 IF F=0 THEN F=A/(2*PI*C*SQR(V^2-A^2*R^2))
1620 D=ATN(1/(2*PI*F*C*R))
1630 RETURN
1640 '
1650 '.....RC parallel
1660 LOCATE 3:K=32:GOSUB 400
1670 PRINT UL$;
1680 IF V=0 THEN V=A*R*X/SQR(R^2+X^2)
1690 IF A=0 THEN A=V*SQR(R^2+X^2)/(R*X)
1700 IF R=0 AND A*X<=V THEN 2220
1710 IF R=0 THEN R=V*X/SQR(A^2*X^2-V^2)
1720 IF (C=0 OR F=0) AND A*R<V THEN 2220
1730 IF C=0 THEN C=SQR(A^2*R^2-V^2)/(2*PI*F*V*R)
1740 IF F=0 THEN F=SQR(A^2*R^2-V^2)/(2*PI*C*V*R)
1750 D=ATN(R*2*PI*F*C)
1760 RETURN
1770 '
1780 '.....RL series
1790 LOCATE 3:K=32:GOSUB 280
1800 PRINT UL$;
1810 IF V=0 THEN V=A*SQR(R^2+X^2)
1820 IF A=0 THEN A=V/SQR(R^2+X^2)
1830 IF R=0 AND V<A*X THEN 2220
1840 IF R=0 THEN R=SQR(V^2-A^2*X^2)/A
1850 IF(L=0 OR F=0)AND V<=A*R THEN 2220
1860 IF L=0 THEN L=SQR(V^2-A^2*R^2)/(A*2*PI*F)
1870 IF F=0 THEN F=SQR(V^2-A^2*R^2)/(A*2*PI*L)
1880 D=ATN(1/(2*PI*F*L/R))
1890 RETURN
1900 '
1910 '.....RL parallel
1920 LOCATE 3:K=32:GOSUB 400
1930 PRINT UL$;
1940 IF V=0 THEN V=A*R*X/SQR(R^2+X^2)
1950 IF A=0 THEN A=V*SQR(R^2+X^2)/(R*X)
1960 IF R=0 AND A*X<=V THEN 2220
1970 IF R=0 THEN R=V*X/SQR(A^2*X^2-V^2)
1980 IF (L=0 OR F=0) AND A*R<V THEN 2220
1990 IF L=0 THEN L=V*R/(2*PI*F*SQR(A^2*R^2-V^2))
2000 IF F=0 THEN F=V*R/(2*PI*L*SQR(A^2*R^2-V^2))
2010 D=ATN(R/(2*PI*F*L))
2020 RETURN
2030 '
2040 '.....LC series/parallel
2050 COLOR 0,7
2060 PRINT " Enter Frequency and values of C and L "
2070 COLOR 7,0:PRINT
2080 GOSUB 3310
2090 IF F*C*L=0 THEN 220
2100 Y=2*PI*F*L:XL=Y
2110 X=1/(2*PI*F*C):XC=X
2120 D=ABS(Y-X)
2130 COLOR 0,7:PRINT " Enter value for either V or I, and 0 for the other "
2140 COLOR 7,0
2150 INPUT " ENTER: Voltage V...........................................";V
2160 IF V THEN 2180
2170 INPUT " ENTER: Current I...........................................";A
2180 IF V=0THEN VS=A*D:VP=A*Y*X/D
2190 IF A=0 THEN BS=V/D:AP=V*D/(Y*X)
2200 GOTO 3440
2210 '
2220 COLOR 0,7:PRINT " You may have entered some improbable values "
2230 COLOR 7,0:PRINT :GOTO 220
2240 '
2250 CLS '.....voltage phase angle
2260 PRINT TAB(31);"VOLTAGE PHASE ANGLE"
2270 PRINT UL$;
2280 LOCATE 3:K=32:GOSUB 280
2290 PRINT UL$;
2300 COLOR 0,7
2310 PRINT " Required data are Frequency, ";A$;;", ";B$;;" & phase angle. ";
2320 PRINT "Enter 0 for the "
2330 PRINT " component you want to compute and correct values for the other 3."
2340 PRINT
2350 COLOR 7,0
2360 GOSUB 3310
2370 IF F*C<>0 THEN X=1/(2*PI*F*C)
2380 IF F*L<>0 THEN X=2*PI*F*L
2390 IF CKT$="RC"THEN 2400 ELSE 2420
2400 INPUT " ENTER: Capacitance voltage phase angle..................(�)";D
2410 GOTO 2430
2420 INPUT " ENTER: Inductance voltage phase angle...................(�)";D
2430 D=-ABS(D/(180/PI))
2440 IF D=0 THEN D=ATN(X/R)-PI/2
2450 IF CKT$="RC"THEN 2470 ELSE 2520
2460 '
2470 IF C=0 THEN C=1/(2*PI*F*R*TAN(D+PI/2))
2480 IF F=0 THEN F=1/(2*PI*C*R*TAN(D+PI/2))
2490 XC=1/(2*PI*F*C)
2500 GOTO 2560
2510 '
2520 IF L=0 THEN L=R/(2*PI*F*TAN(D+PI/2))
2530 IF F=0 THEN F=R/(2*PI*L*TAN(D+PI/2))
2540 XL=2*PI*F*L
2550 '
2560 IF R=0 THEN R=X/TAN(D+PI/2)
2570 LC=-D*180/PI
2580 RES=D*180/PI+90
2590 GOTO 3440
2600 '
2610 CLS '.....voltage divider computation
2620 PRINT TAB(33);"VOLTAGE DIVIDER"
2630 PRINT UL$;
2640 IF CKT$="LC"THEN 3050
2650 LOCATE 3:K=32:GOSUB 280
2660 PRINT UL$;
2670 COLOR 0,7
2680 PRINT " Required data are Frequency, ";A$;;", ";B$;;", V  and the voltage ";
2690 PRINT "across ";B$;". Enter 0  ";
2700 PRINT " for the component you want to compute and correct values ";
2710 PRINT "for the other 4."
2720 COLOR 7,0
2730 PRINT
2740 GOSUB 3310
2750 INPUT " ENTER: Voltage V.....................................(ohms)";V
2760 IF CKT$="RC"THEN 2780
2770 IF CKT$="RL"THEN 2910
2780 INPUT " ENTER: Voltage across capacitance C.................(volts)";N
2790 X=1/(2*PI*F*C)
2800 IF N=0 THEN N=V*X/SQR(R^2+X^2)
2810 IF (R=0 OR C=0 OR F=0) AND V<N THEN 2220
2820 IF R=0 THEN R=X*SQR(V^2-N^2)/N
2830 IF C=0 THEN C=SQR(V^2-N^2)/(2*PI*F*R*N)
2840 IF F=0 THEN F=SQR(V^2-N^2)/(2*PI*C*R*N)
2850 IF V=0 THEN V=N*SQR(R^2+X^2)/X
2860 VR=V*R/SQR(R^2+X^2)
2870 VC=N
2880 XC=1/(2*PI*F*C)
2890 GOTO 3440
2900 '
2910 INPUT " ENTER: Voltage across inductance L..................(volts)";N
2920 X=2*PI*F*L
2930 IF N=0 THEN N=V*X/SQR(R^2+X^2)
2940 IF R=0 AND V<N THEN 2220
2950 IF R=0 THEN R=X*SQR(V^2-N^2)/N
2960 IF F*L=0 AND V<=N THEN 2220
2970 IF L=0 THEN L=N*R/(2*PI*F*SQR(V^2-N^2))
2980 IF F=0 THEN F=N*R/(2*PI*L*SQR(V^2-N^2))
2990 IF V=0 THEN V=N*SQR(R^2+X^2)/X
3000 VR=V*R/SQR(R^2+X^2)
3010 VL=N
3020 XL=2*PI*F*L
3030 GOTO 3440
3040 '
3050 LOCATE 3:K=23:GOSUB 280
3060 LOCATE 3:K=42:GOSUB 400
3070 PRINT UL$;
3080 COLOR 0,7
3090 PRINT " Enter values for Frequency, ";A$;;" and ";B$;;", then             "
3100 PRINT " Enter a value for either V or I, and 0 for the other. "
3110 COLOR 7,0
3120 PRINT
3130 GOSUB 3310
3140 Y=2*PI*F*L:XL=Y
3150 X=1/(2*PI*F*C):XC=X
3160 D=ABS(Y-X)
3170 INPUT " ENTER: Voltage V (peak).............................(volts)";V
3180 IF V THEN 3200
3190 INPUT " ENTER: Current I (peak)..............................(amps)";A
3200 IF V=0 THEN VS=A*D:VP=A*Y*X/D:H$=" VOLTAGES ":GOTO 3220
3210 IF A=0 THEN BS=V/D:AP=V*D/(Y*X):H$=" CURRENTS ":GOTO 3220
3220 PRINT
3230 COLOR 12
3240 PRINT " CAUTION! VERY HIGH";H$;;"MAY BE PRODUCED AT FREQUENCIES ";
3250 PRINT "CLOSE TO RESONANCE!"
3260 COLOR 7
3270 PRINT TAB(25);"Press any key to continue...."
3280 IF INKEY$=""THEN 3280
3290 GOTO 3440
3300 '
3310 '.....inputs
3320 INPUT " ENTER: Frequency......................................(MHz)";F
3330 F=F*10^6
3340 IF CKT$="LC"THEN 3360
3350 INPUT " ENTER: Value of Resistance R.........................(ohms)";R
3360 IF CKT$="RL"THEN 3390
3370 INPUT " ENTER: Value of Capacitance C..........................(pF)";C
3380 C=C*10^-12
3390 IF CKT$="RC"THEN 3420
3400 INPUT " ENTER: Value of Inductance L...........................(�H)";L
3410 L=L*10^-6
3420 RETURN
3430 '
3440 '.....screen print
3450 VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
3460 PRINT " Frequency..............................�=";
3470 PRINT USING U$;F/10^6;:PRINT " MHz":F=0
3480 IF R=0 THEN 3510
3490 PRINT " Resistance.............................R=";
3500 PRINT USING U$;R;:PRINT " �":R=0
3510 IF C=0 THEN 3540
3520 PRINT " Capacitance............................C=";
3530 PRINT USING U$;C*10^12;:PRINT " pF":C=0
3540 IF L=0 THEN 3570
3550 PRINT " Inductance.............................L=";
3560 PRINT USING U$;L*10^6;:PRINT " �H":L=0
3570 IF XC=0 THEN 3600
3580 PRINT " Capacitive reactance..................Xc=";
3590 PRINT USING U$;XC;:PRINT " �":XC=0
3600 IF XL=0 THEN 3630
3610 PRINT " Inductive reactance...................Xl=";
3620 PRINT USING U$;XL;:PRINT " �":XL=0
3630 IF SIMP=0 THEN 3660
3640 PRINT " Series impedance......................Zs=";
3650 PRINT USING U$;SIMP;:PRINT " �":SIMP=0
3660 IF SPA=0 THEN 3690
3670 PRINT " Series phase angle....................�s=";
3680 PRINT USING U$;SPA;:PRINT "�":SPA=0
3690 IF PIMP=0 THEN 3720
3700 PRINT " Parallel impedance....................Zp=";
3710 PRINT USING U$;PIMP;:PRINT " �":PIMP=0
3720 IF PPA=0 THEN 3780
3730 PRINT " Parallel phase angle..................�p=";
3740 PRINT USING U$;PPA;:PRINT "�":PPA=0
3750 IF AV=0 THEN 3780
3760 PRINT " Applied voltage..........................";
3770 PRINT USING U$;AV;:PRINT " v.":AV=0
3780 IF V=0 THEN 3810
3790 PRINT " Voltage across circuit.................V=";
3800 PRINT USING U$;V;:PRINT " v.":V=0
3810 IF VR=0 THEN 3840
3820 PRINT " Voltage across resistor R.............Vr=";
3830 PRINT USING U$;VR;:PRINT " v.":VR=0
3840 IF VC=0 THEN 3870
3850 PRINT " Voltage across capacitance C..........Vc=";
3860 PRINT USING U$;VC;:PRINT " v.":VC=0
3870 IF VL=0 THEN 3900
3880 PRINT " Voltage across inductor L.............Vl=";
3890 PRINT USING U$;VL;:PRINT " v.":VL=0
3900 IF VS=0 THEN 3930
3910 PRINT " Voltage across series circuit..........V=";
3920 PRINT USING U$;VS;:PRINT " v.":VS=0
3930 IF VP=0 THEN 3960
3940 PRINT " Voltage across parallel circuit........V=";
3950 PRINT USING U$;VP;:PRINT " v.":VP=0
3960 IF A=0 THEN 3990
3970 PRINT " Current................................I=";
3980 PRINT USING U$;A;:PRINT " A.":A=0
3990 IF BS=0 THEN 4020
4000 PRINT " Current through series circuit.........I=";
4010 PRINT USING U$;BS;:PRINT " A.":BS=0
4020 IF AP=0 THEN 4050
4030 PRINT " Current through parallel circuit.......I=";
4040 PRINT USING U$;AP;:PRINT " A.":AP=0
4050 IF CVPA=0 THEN 4080
4060 PRINT " Current-voltage phase angle..........�cv=";
4070 PRINT USING U$;CVPA;:PRINT "�":CVPA=0
4080 IF W=0 THEN 4110
4090 PRINT " Power consumption......................P=";
4100 PRINT USING U$;W;:PRINT " watts":P=0
4110 IF LC=0 THEN 4170
4120 IF CKT$="RC"THEN 4130 ELSE 4150
4130 PRINT " Capacitance voltage lags behind V by.....";
4140 GOTO 4160
4150 PRINT " Inductance voltage leads V by............";
4160 PRINT USING U$;LC;:PRINT "�";:LC=0
4170 IF RES=0 THEN 4240
4180 IF CKT$="RC"THEN 4190 ELSE 4210
4190 PRINT " Resistance voltage leads V by............";
4200 GOTO 4220
4210 PRINT " Resistance voltage lags behind V by......";
4220 PRINT USING U$;RES;:PRINT "�";:RES=0
4230 '
4240 '.....end
4250 GOSUB 4280
4260 GOTO 80
4270 '
4280 'PRT
4290 KEY OFF:GOSUB 4360:LOCATE 25,5:COLOR 0,2
4300 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
4310 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
4320 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 4320 ELSE GOSUB 4360
4330 IF Z$="3"THEN RETURN
4340 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
4350 IF Z$="2"THEN LPRINT CHR$(12) ELSE 4290
4360 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

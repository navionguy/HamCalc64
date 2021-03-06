1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'ATTENPAD - TEE & PI attenuator pads - 05 JUN 95 rev. 08 APR 97
20 IF EX$=""THEN EX$="EXIT"
30 KEY OFF
40 DEFDBL P
50 COLOR 7,0,0
60 UL$=STRING$(80,205)
70 DIM STD(25)    'standard resistor base values
80 DIM RES(25)
90 DATA 10,11,12,13,15,16,18,20,22,24,27,30
100 DATA 33,36,39,43,47,51,56,62,68,75,82,91,100
110 FOR Z=1 TO 25:READ STD(Z):NEXT Z
120 CLS:GOTO 330
130 '
140 '.....nearest standard resistor
150 REM: REX = exact resistor value
160 REM: NSR = nearest standard resistor
170 MX=9.999999E-03      'multiplier
180 FOR Z=1 TO 25:RES(Z)=STD(Z)*MX:NEXT Z
190 '
200 FOR Y=1 TO 24:MID=(RES(Y)+RES(Y+1))/2
210 IF REX>=RES(Y)AND REX<=MID THEN NSR=RES(Y):GOTO 260
220 IF REX<=RES(Y+1)AND REX>MID THEN NSR=RES(Y+1):GOTO 260
230 NEXT Y
240 MX=MX*10:IF MX>=10^7 THEN NSR=22*10^6:GOTO 260
250 GOTO 180
260 RETURN
270 '
280 '.....format input line
290 U$="####.###":LOCATE CSRLIN-1:PRINT SPC(7);
300 LOCATE CSRLIN,47:PRINT STRING$(7,".");USING U$;Z;
310 RETURN
320 '
330 '.....start
340 CLS
350 COLOR 15,2
360 PRINT" ATTENUATORS: T-Pad and Pi-Pad";TAB(62);"by Bob Stein W6NBI ";
370 PRINT STRING$(80,32);
380 LOCATE CSRLIN-1,20:PRINT "edited for HAMCALC by George Murphy VE3ERP"
390 COLOR 1,0:PRINT STRING$(80,223);
400 COLOR 7,0
410 PRINT TAB(8);
420 PRINT "This program computes resistance values and power dissipation in"
430 PRINT TAB(8);
440 PRINT "constant-impedance TEE and PI attenuator pads."
450 PRINT
460 PRINT TAB(8);
470 PRINT "For minimum loss: R2 in the T-pad approaches zero, and R4 in the"
480 PRINT TAB(8);
490 PRINT "Pi-pad approaches infinity, effectively making them L-pads."
500 PRINT UL$;
510 LN=CSRLIN
520 TM= 8:LOCATE LN:GOSUB 2810    'diagrams
530 TM=44:LOCATE LN:GOSUB 2930
540 PRINT UL$;
550 COLOR 0,7:LOCATE CSRLIN,22
560 PRINT " Press 1 to continue or 0 to EXIT....."
570 COLOR 7,0
580 Z$=INKEY$:IF Z$=""THEN 580
590 IF Z$="0"THEN CLS:RUN EX$
600 IF Z$="1"THEN 630
610 GOTO 580
620 '
630 CLS
640 TM= 8:LOCATE 1:GOSUB 2810     'diagrams
650 TM=44:LOCATE 1:GOSUB 2930
660 PRINT UL$;
670 INPUT " ENTER: Terminating Resistance Z(Hi).......... (ohms)";ZH
680 IF ZH<=0 THEN BEEP:GOTO 630
690 Z=ZH:GOSUB 280:PRINT " �"
700 INPUT " ENTER: Terminating Resistance Z(Lo).......... (ohms)";ZL
710 IF ZL<=ZH THEN 760
720 BEEP:PRINT " Z(Lo) MUST BE EQUAL TO OR LESS THAN Z(Hi), BUT NOT ZERO"
730 PRINT " Press any key..........."
740 IF INKEY$=""THEN 740
750 CSR=CSRLIN-3:VIEW PRINT CSR TO 24:CLS:VIEW PRINT:LOCATE CSR:GOTO 630
760 Z=ZL:GOSUB 280:PRINT " �"
770 A=((SQR(ZH/ZL)+SQR((ZH/ZL)-1))^2)
780 ML=10*(LOG(A)/LOG(10)) : ML=INT(100*(ML+5.000000E-01/100))/100
790 PRINT "        Minimum Loss.................................";USING U$;ML;
800 PRINT " dB"
810 IF ML=0 GOTO 880
820 P=A
830 GOSUB 2550   'formulae
840 PRINT
850 PRINT TAB(28);"MINIMUM LOSS PADS  (effectively, L-pads)"
860 PRINT TAB(27);"�������������������"
870 GOSUB 2690   'print values for minimum loss
880 PRINT
890 INPUT " ENTER: Required Loss............................(dB)";L
900 IF L>ML THEN 940
910 BEEP:PRINT" REQUIRED LOSS MUST BE GREATER THAN MIN. LOSS OF";ML;"dB"
920 GOTO 880
930 '
940 '.....calculation
950 CLS:PRINT
960 P=EXP((L/10)*LOG(10))
970 GOSUB 2550   'resistance formulas
980 TM= 8:LOCATE 1:GOSUB 2810   'diagrams
990 TM=44:LOCATE 1:GOSUB 2930   'diagrams
1000 PRINT UL$;
1010 GOSUB 2640  'calculate and print
1020 PRINT UL$;
1030 CSR=CSRLIN:GOSUB 4690
1040 LOCATE 25,1:PRINT STRING$(80,32);:LOCATE CSR
1050 '
1060 PRINT " If option <4> is run before option <3>, then option <3> will also";
1070 PRINT " calculate"
1080 PRINT " recommended resistor wattages."
1090 PRINT UL$;
1100 PRINT "        Press number in < > to:"
1110 PRINT UL$;
1120 PRINT "  < 1 > Change specified loss"
1130 PRINT "  < 2 > Change Impedances (Rerun)"
1140 PRINT "  < 3 > Determine exact Attenuation & Impedances using standard ";
1150 PRINT "value resistors"
1160 PRINT "  < 4 > Determine power dissipated in each resistor"
1170 PRINT "  < 5 > QUIT";
1180 Z$=INKEY$:IF Z$=""THEN 1180
1190 IF Z$="1"THEN LOCATE 24,8:GOTO 890
1200 IF Z$="2"THEN 120
1210 IF Z$="3"THEN LOCATE 24,8:GOTO 1260
1220 IF Z$="4"THEN LOCATE 24,8:GOTO 3050
1230 IF Z$="5"THEN 330   'start
1240 GOTO 1180
1250 '
1260 '.....exact attenuation & impedances for standard value resistors
1270 COLOR 0,7
1280 PRINT " For T-pad or Pi-pad?  (t/p) ";
1290 COLOR 7,0
1300 Z$=INKEY$:IF Z$=""THEN 1300
1310 IF Z$="T"OR Z$="t"THEN 1350
1320 IF Z$="P"OR Z$="p"THEN 1950
1330 GOTO 1300
1340 '
1350 CLS:LOCATE 1:TM=26:GOSUB 2810    'T-pad diagram
1360 PRINT UL$;
1370 IF I THEN GOSUB 3910
1380 GOSUB 4390:LOCATE 10
1390 OTHER$="Pi-pad"
1400 U$="####,###.#"
1410 T=42
1420 '
1430 REX=R1:GOSUB 140
1440 PRINT " R1 calculated value is";R1;"�";TAB(T);"Use: R1=";USING U$;NSR;
1450 PRINT " �";:W=W1:GOSUB 4440:PRINT W$:R11=NSR
1460 '
1470 REX=R2:GOSUB 140
1480 PRINT " R2 calculated value is";R2;"�";TAB(T);"Use: R2=";USING U$;NSR;
1490 PRINT " �";:W=W2:GOSUB 4440:PRINT W$:R22=NSR
1500 '
1510 REX=R3:GOSUB 140
1520 PRINT " R3 calculated value is";R3;"�";TAB(T);"Use: R3=";USING U$;NSR;
1530 PRINT " �";:W=W3:GOSUB 4440:PRINT W$:R33=NSR
1540 '
1550 RA=R22+ZL
1560 RB=RA*R33/(RA+R33)
1570 RC=R11+RB
1580 RD=R11+ZH
1590 RE=RD*R33/(RD+R33)
1600 RF=R22+RE
1610 PA=1/RC
1620 PB=((ZL/RA)*(RB/RC))^2/ZL
1630 ATI=10*LOG(PA/PB)/LOG(10)
1640 PC=1/RF
1650 PD=((ZH/RD)*(RE/RF))^2/ZH
1660 ATO=10*LOG(PC/PD)/LOG(10)
1670 SC=RC/ZH
1680 IF SC<1 THEN SC=1/SC
1690 SF=RF/ZL
1700 IF SF<1 THEN SF=1/SF
1710 PRINT
1720 PRINT " For";L;"dB T-pad using standard resistors shown above and ";
1730 PRINT "design values of:"
1740 PRINT
1750 PRINT TAB(25);"Z(Hi) =";ZH;"� and Z(Lo) =";ZL;"�"
1760 PRINT
1770 U$="####.##"
1780 PRINT "   Impedance looking into Z(Hi) =   ";USING U$;RC;
1790 PRINT " �.....VSWR =";USING "##.##";SC
1800 PRINT "   Impedance looking into Z(Lo) =   ";USING U$;RF;
1810 PRINT " �.....VSWR =";USING "##.##";SF
1820 PRINT
1830 PRINT "   Attenuation from Z(Hi) to Z(Lo) =";USING U$;ATI;:PRINT " dB"
1840 PRINT "   Attenuation from Z(Lo) to Z(Hi) =";USING U$;ATO;:PRINT " dB"
1850 CSR=CSRLIN
1860 PRINT
1870 COLOR 0,7
1880 PRINT " Do you want to see the ";OTHER$;" configuration?  (y/n) ";
1890 Z$=INKEY$:IF Z$=""THEN 1890
1900 COLOR 7,0
1910 IF Z$="N"OR Z$="n"THEN 4570
1920 IF Z$="Y"OR Z$="y"THEN 1950
1930 GOTO 1890
1940 '
1950 CLS:LOCATE 1:TM=26:GOSUB 2930    'Pi-pad diagram
1960 PRINT UL$;
1970 IF I THEN GOSUB 3910
1980 GOSUB 4390:LOCATE 10
1990 OTHER$="T-pad"
2000 U$="####,###.#"
2010 T=42
2020 '
2030 REX=R4:GOSUB 140
2040 PRINT " R4 calculated value is";R4;"�";TAB(T);"Use: R4=";USING U$;NSR;
2050 PRINT " �";:W=W4:GOSUB 4440:PRINT W$:R44=NSR
2060 '
2070 REX=R5:GOSUB 140
2080 PRINT " R5 calculated value is";R5;"�";TAB(T);"Use: R5=";USING U$;NSR;
2090 PRINT " �";:W=W5:GOSUB 4440:PRINT W$:R55=NSR
2100 '
2110 REX=R6:GOSUB 140
2120 PRINT " R6 calculated value is";R6;"�";TAB(T);"Use: R6=";USING U$;NSR;
2130 PRINT " �";:W=W6:GOSUB 4440:PRINT W$:R66=NSR
2140 '
2150 RG=R55*ZL/(R55+ZL)
2160 RH=RG+R66
2170 RI=R44*RH/(R44+RH)
2180 RJ=R44*ZH/(R44+ZH)
2190 RK=RJ+R66
2200 RL=R55*RK/(R55+RK)
2210 PE=1/RI
2220 PF=(RG/RH)^2/ZL
2230 API=10*LOG(PE/PF)/LOG(10)
2240 PG=1/RL
2250 PH=(RJ/RK)^2/ZH
2260 APO=10*LOG(PG/PH)/LOG(10)
2270 SI=RI/ZH
2280 IF SI<1 THEN SI=1/SI
2290 SL=RL/ZL
2300 IF SL<1 THEN SL=1/SL
2310 PRINT
2320 PRINT " For";L;"dB Pi-pad using standard resistors shown above and ";
2330 PRINT "design values of:"
2340 PRINT
2350 PRINT TAB(25);"Z(Hi) =";ZH;"� and Z(Lo) =";ZL;"�"
2360 PRINT
2370 U$="####.##"
2380 PRINT "   Impedance looking into Z(Hi) =   ";USING U$;RI;
2390 PRINT " �.....VSWR =";USING "##.##";SI
2400 PRINT "   Impedance looking into Z(Lo) =   ";USING U$;RL;
2410 PRINT " �.....VSWR =";USING "##.##";SL
2420 PRINT
2430 PRINT "   Attenuation from Z(Hi) to Z(Lo) =";USING U$;API;:PRINT " dB"
2440 PRINT "   Attenuation from Z(Lo) to Z(Hi) =";USING U$;APO;:PRINT " dB"
2450 CSR=CSRLIN
2460 PRINT
2470 COLOR 0,7
2480 PRINT " Do you want to see the ";OTHER$;" configuration?  (y/n) ";
2490 Z$=INKEY$:IF Z$=""THEN 2490
2500 COLOR 7,0
2510 IF Z$="N"OR Z$="n"THEN 4570
2520 IF Z$="Y"OR Z$="y"THEN 1350
2530 GOTO 2490
2540 '
2550 '.....Formulas for resistance values from ITT Handbook
2560 R3=2*SQR(P*ZH*ZL)/(P-1)
2570 R1=ZH*((P+1)/(P-1))-R3
2580 R2=ZL*((P+1)/(P-1))-R3
2590 R6=((P-1)*SQR(ZH*ZL/P))/2
2600 R4=1/((1/ZH)*(P+1)/(P-1)-(1/R6))
2610 R5=1/((1/ZL)*(P+1)/(P-1)-(1/R6))
2620 RETURN
2630 '
2640 'ABS variables in print statements eliminate negative values of unused
2650 'resistors in min. loss L-pads
2660 PRINT"       ";L;"dB T-Pad";TAB(33);"Z(Hi) =";ZH;"�";TAB(59);L;"dB Pi-Pad"
2670 PRINT TAB(33);"Z(Lo) =";ZL;"�"
2680 '
2690 U$="###,###.##"
2700 PRINT "        R1 =";USING U$;R1;:PRINT " �";TAB(53);"R4 =";
2710 IF ABS(R4)>10^7 THEN PRINT " (near infinity)":GOTO 2730
2720 PRINT USING U$;ABS(R4);:PRINT " �"
2730 PRINT "        R2 =";'USING U$;ABS(R2);:PRINT " �";TAB(53);"R5 =";
2740 IF ABS(R2)<1.000000E-03 THEN PRINT " (near zero)";:GOTO 2760
2750 PRINT USING U$;ABS(R2);:PRINT " �";
2760 PRINT TAB(53);"R5 =";USING U$;ABS(R5);:PRINT " �"
2770 PRINT "        R3 =";USING U$;R3;:PRINT " �";TAB(53);"R6 =";
2780 PRINT USING U$;R6;:PRINT " �"
2790 RETURN
2800 '
2810 '.....T-pad diagram
2820 COLOR 0,7
2830 LOCATE CSRLIN,TM:PRINT "                             "
2840 LOCATE CSRLIN,TM:PRINT "   ���� R1 ������� R2 ����   "
2850 LOCATE CSRLIN,TM:PRINT "              �              "
2860 LOCATE CSRLIN,TM:PRINT " Z(Hi)į      R3     ��Z(Lo) "
2870 LOCATE CSRLIN,TM:PRINT "              �              "
2880 LOCATE CSRLIN,TM:PRINT "   �����������������������   "
2890 LOCATE CSRLIN,TM:PRINT "            T-Pad            "
2900 COLOR 7,0
2910 RETURN
2920 '
2930 '.....Pi-pad diagram
2940 COLOR 0,7
2950 LOCATE CSRLIN,TM:PRINT "                              "
2960 LOCATE CSRLIN,TM:PRINT "   ���������� R6 ����������   "
2970 LOCATE CSRLIN,TM:PRINT "         �          �         "
2980 LOCATE CSRLIN,TM:PRINT " Z(Hi)į R4        R5 ��Z(Lo) "
2990 LOCATE CSRLIN,TM:PRINT "         �          �         "
3000 LOCATE CSRLIN,TM:PRINT "   ������������������������   "
3010 LOCATE CSRLIN,TM:PRINT "            Pi-Pad            "
3020 COLOR 7,0
3030 RETURN
3040 '
3050 '.....Power dissipation calculations
3060 CLS
3070 PRINT
3080 PRINT "        Will input to pad be entered in (v)olts or (w)atts?  (v/w)"
3090 Z$=INKEY$:IF Z$=""THEN 3090
3100 IF Z$="V"OR Z$="v"THEN IP$="volts":GOTO 3150
3110 IF Z$="W"OR Z$="w"THEN IP$="watts":GOTO 3150
3120 GOTO 3090
3130 '
3140 GOTO 4120
3150 PRINT " ENTER: Input signal level applied to pad.....(";IP$;")";:INPUT I
3160 Z=I:GOSUB 280:PRINT " ";IP$
3170 PRINT "        Is input applied to (h)i or (l)o impedance port?  (h/l)"
3180 IZ$=INKEY$:IF IZ$=""THEN 3180
3190 IF IZ$="H" OR IZ$="h" THEN IZ$="Hi":GOTO 3220
3200 IF IZ$="L" OR IZ$="l" THEN IZ$="Lo":GOTO 3220
3210 GOTO 3180
3220 IF IZ$="Lo" THEN 3560 ELSE 3250
3230 GOTO 3180
3240 '
3250 '.....select pad for HI port input
3260 IF IP$="watts" THEN V=SQR(I*ZH) ELSE V=I
3270 COLOR 0,7:LOCATE CSRLIN,8
3280 PRINT " For T-pad or Pi-pad?  (t/p) ";
3290 COLOR 7,0
3300 ZP$=INKEY$:IF ZP$=""THEN 3300
3310 IF ZP$="T"OR ZP$="t"THEN 3350
3320 IF ZP$="P"OR ZP$="p"THEN 3450
3330 GOTO 1300
3340 '
3350 RE=R3*(R2+ZL)/(R3+R2+ZL)
3360 V3=V*RE/(RE+R1)
3370 V1=V-V3
3380 V2=V3*R2/(R2+ZL)
3390 GOSUB 3860
3400 CLS:LOCATE 1:TM=26:GOSUB 2810  'T-pad diagram
3410 GOSUB 4390
3420 GOSUB 3910
3430 GOTO 3970
3440 '
3450 RE=R5*ZL/(R5+ZL)
3460 V5=V*RE/(RE+R6)
3470 V6=V-V5
3480 W4=V*V/R4
3490 W5=V5*V5/R5
3500 W6=V6*V6/R6
3510 CLS:LOCATE 1:TM=26:GOSUB 2930  'Pi-pad diagram
3520 GOSUB 4390        'print Z(Hi) & Z(Lo)
3530 GOSUB 3910        'print results
3540 GOTO 4190         'print Pi-pad results
3550 '
3560 '.....select pad for LO port input
3570 IF IP$="watts" THEN V=SQR(I*ZL) ELSE V=I
3580 COLOR 0,7:LOCATE CSRLIN,8
3590 PRINT " For T-pad or Pi-pad?  (t/p) ";
3600 COLOR 7,0
3610 ZP$=INKEY$:IF ZP$=""THEN 3610
3620 IF ZP$="T"OR ZP$="t"THEN 3660
3630 IF ZP$="P"OR ZP$="p"THEN 3760
3640 GOTO 1300
3650 '
3660 RE=R3*(R1+ZH)/(R3+R1+ZH)
3670 V3=V*RE/(RE+R2)
3680 V2=V-V3
3690 V1=V3*R1/(R1+ZH)
3700 GOSUB 3860
3710 CLS:LOCATE 1:TM=26:GOSUB 2810  'T-pad diagram
3720 GOSUB 4390
3730 GOSUB 3910
3740 GOTO 3970
3750 '
3760 RE=R4*ZH/(R4+ZH)
3770 V4=V*RE/(RE+R6)
3780 V6=V-V4
3790 W4=V4*V4/R4
3800 W5=V*V/R5
3810 W6=V6*V6/R6
3820 CLS:LOCATE 1:TM=26:GOSUB 2930    'Pi-pad diagram
3830 GOSUB 4390
3840 GOSUB 3910
3850 GOTO 4190
3860 W1=V1*V1/R1
3870 W2=V2*V2/R2
3880 W3=V3*V3/R3
3890 RETURN
3900 '
3910 '.....text
3920 LOCATE 9,18
3930 PRINT "With";I;IP$;" applied to ";IZ$;"-Z port of";L;"dB pad:"
3940 PRINT
3950 RETURN
3960 '
3970 '.....T-pad
3980 T=16:U$="##,###.###"
3990 PRINT TAB(T);
4000 PRINT "R1 =";USING U$;ABS(R1);:PRINT " � @";USING U$;SQR(W1/R1);
4010 PRINT " amps =";USING U$;W1;:PRINT " watts"
4020 PRINT TAB(T);
4030 PRINT "R2 =";USING U$;ABS(R2);:PRINT " � @";USING U$;SQR(W2/R2);
4040 PRINT " amps =";USING U$;W2;:PRINT " watts"
4050 PRINT TAB(T);
4060 PRINT "R3 =";USING U$;ABS(R3);:PRINT " � @";USING U$;SQR(W3/R3);
4070 PRINT " amps =";USING U$;W3;:PRINT " watts"
4080 PRINT
4090 COLOR 0,7
4100 PRINT " Do you want to see the Pi-pad configuration?   (y/n) "
4110 COLOR 7,0
4120 Z$=INKEY$:IF Z$=""THEN 4120
4130 IF Z$="Y"OR Z$="y"THEN ZP$="p":GOTO 4170
4140 IF Z$="N"OR Z$="n"THEN 4570
4150 GOTO 4120
4160 '
4170 IF IZ$="Hi"THEN 3310 ELSE 3620
4180 '
4190 '.....Pi-pad
4200 T=16:U$="##,###.###"
4210 PRINT TAB(T);
4220 PRINT "R4 =";USING U$;ABS(R4);:PRINT " � @";USING U$;SQR(W4/R4);
4230 PRINT " amps =";USING U$;W4;:PRINT " watts"
4240 PRINT TAB(T);
4250 PRINT "R5 =";USING U$;ABS(R5);:PRINT " � @";USING U$;SQR(W5/R5);
4260 PRINT " amps =";USING U$;W5;:PRINT " watts"
4270 PRINT TAB(T);
4280 PRINT "R6 =";USING U$;ABS(R6);:PRINT " � @";USING U$;SQR(W6/R6);
4290 PRINT " amps =";USING U$;W6;:PRINT " watts"
4300 PRINT
4310 COLOR 0,7
4320 PRINT " Do you want to see the T-pad configuration?   (y/n) "
4330 COLOR 7,0
4340 Z$=INKEY$:IF Z$=""THEN 4340
4350 IF Z$="Y"OR Z$="y"THEN ZP$="t":GOTO 4170
4360 IF Z$="N"OR Z$="n"THEN 4570
4370 GOTO 4340
4380 '
4390 '.....display impedances in diagram
4400 LOCATE 4,12 : PRINT ZH;"� �į "
4410 LOCATE 4,58 : PRINT " ���";ZL;"� "
4420 RETURN
4430 '
4440 '.....resistor rating
4450 W=W*1.500000
4460 IF W>10 THEN W$=STR$(INT(W)+1)+" w."
4470 IF W<=10 THEN W$="10 w."
4480 IF W<=5 THEN W$="5 w."
4490 IF W<=2 THEN W$="2 w."
4500 IF W<=1 THEN W$="1 w."
4510 IF W<=5.000000E-01 THEN W$="� w."
4520 IF W<=2.500000E-01 THEN W$="� w."
4530 IF LEN(W$)<7 THEN W$=" "+W$:GOTO 4530
4540 IF W=0 THEN W$=""
4550 RETURN
4560 '
4570 '.....return to sub-menu
4580 LOCATE CSRLIN-1:PRINT STRING$(80,32);:GOSUB 4690:CLS
4590 LOCATE 1:TM= 8:GOSUB 2810   'T-pad diagram
4600 LOCATE 1:TM=44:GOSUB 2930   'Pi-pad diagram
4610 PRINT UL$;
4620 GOTO 1060
4630 '
4640 '.....end
4650 GOSUB 4690
4660 GOTO 330
4670 END
4680 '
4690 'PRT
4700 KEY OFF:GOSUB 4770:LOCATE 25,5:COLOR 0,2
4710 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
4720 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
4730 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 4730 ELSE GOSUB 4770
4740 IF Z$="3"THEN RETURN
4750 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
4760 IF Z$="2"THEN LPRINT CHR$(12) ELSE 4700
4770 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

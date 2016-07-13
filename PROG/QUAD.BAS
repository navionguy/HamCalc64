1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$,DIMN$,A,B,C
10 'QUAD - Quad Antenna Dimensions - 28 APR 2000 rev. 08 MAR 2009
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 PROG$="quad"
50 
60 COLOR 7,0,1
70 LGF=1/LOG(10)   'factor to convert log to base 10
80 UL$=STRING$(80,205)
90 SL=2.997925E+05  'speed of light in metres/sec
100 '.....AWG calculator
110 DIM GA(50)
120 K=(4.600000E-01/4.999999E-03)^(1/39)    'increment multiplier
130 FOR Z=1 TO 50
140 N=Z+3
150 GA(Z)=4.600000E-01/K^N
160 NEXT Z
170 '
180 '.....start
190 CLS
200 COLOR 15,2
210 PRINT " QUAD ANTENNA DIMENSIONS";TAB(57);;"by George Murphy VE3ERP ";
220 COLOR 1,0:PRINT STRING$(80,223);
230 LOCATE CSRLIN-1,24:COLOR 15,2
240 PRINT " Algorithms by L.B.Cebik, W4RNL "
250 COLOR 7,0
260 PRINT
270 GOSUB 5150   'print text
280 COLOR 0,7
290 LOCATE ,26:PRINT " Press 1 for 1-element quad "
300 LOCATE ,26:PRINT " Press 2 for 2-element quad "
310 LOCATE ,26:PRINT " Press 3 for 3-element quad "
320 LOCATE ,26:PRINT " Press 4 for 4-element quad "
330 LOCATE ,26:PRINT " Press 0 to EXIT            ";
340 COLOR 7,0
350 E$=INKEY$:IF E$=""THEN 350
360 IF E$="0"THEN CLS:RUN EX$
370 IF E$="1"THEN 4630
380 IF E$="2"OR E$="3"OR E$="4"THEN 410
390 GOTO 350
400 '
410 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 4
420 GOSUB 5510  'diagram
430 PRINT
440 INPUT " ENTER: Centre frequency of band (MHz).....";F
450 CLS
460 PRINT " Press number in < > to choose units for wire diameter:"
470 PRINT UL$;
480 PRINT "  < 1 >  inches"
490 PRINT "  < 2 >  millimetres"
500 PRINT "  < 3 >  wavelengths"
510 PRINT "  < 4 >  AWG size"
520 PRINT UL$;
530 Z$=INKEY$
540 IF Z$="1"THEN Y$="(inches)":GOTO 590
550 IF Z$="2"THEN Y$="(mm.)":GOTO 590
560 IF Z$="3"THEN Y$="(wavelengths)":GOTO 590
570 IF Z$="4"THEN Y$="(AWG size)":GOTO 590
580 GOTO 530
590 PRINT " ENTER: Wire diameter ";Y$;;" ";:INPUT WD
600 IF Z$="1"THEN WLI=SL/2.539999E+01/F:D=WD/WLI  'WLI=freq. in kHz
610 IF Z$="2"THEN WLI=SL/F:D=WD/WLI
620 IF Z$="3"THEN D=WD
630 IF Z$="4"THEN D=(GA(WD)*2.539999E+01)/(SL/F)
640 '
650 CLS:X$=" "+E$+"-ELEMENT QUAD ANTENNA DIMENSIONS  (wL = wavelengths) "
660 LOCATE 1,(80-LEN(X$))/2:COLOR 15,2:PRINT X$:COLOR 7,0
670 PRINT USING " Operating frequency:          #####.### MHz";F;
680 WL=SL/F/10^3:WF=WL/3.048000E-01
690 PRINT USING " = ##.### metres";WL:GOTO 700
700 Z$="  Wire diameter in wavelengths...............= ##.######^^^^ wL"
710 PRINT USING Z$;D
720 A=SL/F*D:B=A/2.539999E+01:C=(LOG(4.600000E-01/B))/LOG(1.122900)-3
730 Z$="  Wire diameter.................. ##.### mm. = ##.### in.= AWG ##.#"
740 PRINT USING Z$;A,B,C
750 IF E$="2"THEN 790
760 IF E$="3"THEN 1110
770 IF E$="4"THEN 1930
780 '
790 '.....2-element perimeter calculation - algorithm by L.B.Cebik, W4RNL
800 L=LGF*LOG(D*10^5):LL=L^2:LM=LL*1.279999E-02:LN=LM+1.041300:D1=LGF*LOG(D)
810 COLOR 14,4
820 IF D1<-4.500000 THEN 830 ELSE 840
830 PRINT "Wire diameter less than 3E-5 wL:  results uncertain."
840 IF D1>-2 THEN 850 ELSE 860
850 PRINT "Wire diameter greater than 1E-2 wL:  results uncertain."
860 COLOR 7,0
870 AD=3.359999999999999E-03:BD=4.966518518999999E-02:CD=2.731955556000000E-01:DD=6.716364021000000E-01:ED=1.644147937000000
880 DE=(AD*(D1^4))+(BD*(D1^3))+(CD*(D1^2))+(DD*D1)+ED
890 AR=3.173333332999999E-03:BR=5.082370370000000E-02:CR=3.081977778000000E-01:DR=8.663851852000000E-01
900 ER=2.040064443999999
910 RE=(AR*(D1^4))+(BR*(D1^3))+(CR*(D1^2))+(DR*D1)+ER
920 BS=-3.000000000000000E-03:BS=-3.551851852000000E-02:CS=-1.553055555999999E-01:DS=-2.902116402000000E-01
930 ES=-2.540079365000000E-02
940 SP=(BS*(D1^4))+(BS*(D1^3))+(CS*(D1^2))+(DS*D1)+ES
950 AZ=1.976333332999999:BZ=3.084751851999999E+01:CZ=1.724909722000000E+02:DZ=4.195162831000000E+02
960 EZ=5.198747579000000E+02
970 ZR=(AZ*(D1^4))+(BZ*(D1^3))+(CZ*(D1^2))+(DZ*D1)+EZ
980 AG=-6.333333332999999E-02:BG=-7.203703704000000E-01:CG=-3.010277778000000:DG=-5.381375660999999
990 EG=3.738769840999999
1000 GN=(AG*(D1^4))+(BG*(D1^3))+(CG*(D1^2))+(DG*D1)+EG
1010 AW=1.688666667000000:BW=2.376837036999999E+01:CW=1.249339443999999E+02:DW=2.958872327999999E+02
1020 EW=2.812755159000000E+02
1030 SW=(AW*(D1^4))+(BW*(D1^3))+(CW*(D1^2))+(DW*D1)+EW
1040 AF=-2.666666670000000E-03:BF=3.879999999999999E-01:CF=4.790666666999999:DF=1.955485714000000E+01:EF=2.876628571000000E+01
1050 FB=(AF*(D1^4))+(BF*(D1^3))+(CF*(D1^2))+(DF*D1)+EF
1060 AN=-8.333333333000000E-02:BN=-9.462962963000000E-01:CN=-3.943055556000000:DN=-7.582671958000000
1070 EN=-5.232341270000000
1080 DG=(AN*(D1^4))+(BN*(D1^3))+(CN*(D1^2))+(DN*D1)+EN
1090 GOTO 3120
1100 '
1110 '.....3-element perimeter calculation - algorithm by L.B.Cebik, W4RNL
1120 COLOR 0,7
1130 PRINT " Want best design for (w)ide-band or (h)igh gain?    (w/h) "
1140 COLOR 7,0
1150 Z$=INKEY$:IF Z$=""THEN 1150
1160 IF Z$="w"THEN LOCATE CSRLIN-1:GOTO 1200
1170 IF Z$="h"THEN LOCATE CSRLIN-1:GOTO 1580
1180 GOTO 1150
1190 '
1200 '.....wide band
1210 L=LGF*LOG(D*10^5):LL=L^2:LM=LL*1.279999E-02:LN=LM+1.041300:D1=LGF*LOG(D)
1220 COLOR 14,4
1230 IF D1<-4.500000 THEN 1240 ELSE 1250
1240 PRINT "Wire diameter less than 3E-5 wavelengths:  results uncertain."
1250 IF D1>-2 THEN 1260 ELSE 1270
1260 PRINT "Wire diameter greater than 1E-2 wavelengths:  results uncertain."
1270 COLOR 7,0
1280 AD=6.399999E-04:BD=1.044148148000000E-02:CD=6.484444443999999E-02:DD=1.886626455000000E-01:ED=1.232080635000000
1290 DE=(AD*(D1^4))+(BD*(D1^3))+(CD*(D1^2))+(DD*D1)+ED
1300 AR=9.333333333000000E-04:BR=1.915555555999999E-02:CR=1.398333333300000E-01:DR=4.587492062999999E-01
1310 ER=1.640423810000000
1320 RE=(AR*(D1^4))+(BR*(D1^3))+(CR*(D1^2))+(DR*D1)+ER
1330 AI=-1.200000000000000E-03:BI=-2.090370369999999E-02:CI=-1.302111111099999E-01:DI=-3.498137565999999E-01
1340 EI=5.941126983999999E-01
1350 IR=(AI*(D1^4))+(BI*(D1^3))+(CI*(D1^2))+(DI*D1)+EI
1360 BS=-3.299999999999999E-03:BS=-3.927777778000000E-02:CS=-1.724583332999999E-01:DS=-3.239603174999999E-01
1370 ES=-4.951547619000000E-02
1380 SP=(BS*(D1^4))+(BS*(D1^3))+(CS*(D1^2))+(DS*D1)+ES
1390 AP=-4.866666667000000E-03:BP=-6.262962962999999E-02:CP=-2.934722222200000E-01:DP=-6.174457671999999E-01
1400 EP=-2.289269841000000E-01
1410 IP=(AP*(D1^4))+(BP*(D1^3))+(CP*(D1^2))+(DP*D1)+EP
1420 AZ=-2.227066666999999:BZ=-2.675247406999999E+01:CZ=-1.159142556000000E+02:DZ=-2.178183323000000E+02
1430 EZ=-7.959203175000000E+01
1440 ZR=(AZ*(D1^4))+(BZ*(D1^3))+(CZ*(D1^2))+(DZ*D1)+EZ
1450 AG=-6.999999999999999E-02:BG=-7.877777777999999E-01:CG=-3.350833333000000:DG=-6.143888888999999:EG=5.104166666999999
1460 GN=(AG*(D1^4))+(BG*(D1^3))+(CG*(D1^2))+(DG*D1)+EG
1470 AW=-5.847333333000000E-02:BW=-5.028392592999999E-01:CW=-4.586494444000000E-01:DW=6.080227037000000
1480 EW=1.761091388999999E+01
1490 SW=(AW*(D1^4))+(BW*(D1^3))+(CW*(D1^2))+(DW*D1)+EW
1500 AF=1.169566666700000E-01:BF=1.717985555999999:CF=9.651092500000000:DF=2.523848992000000E+01
1510 EF=2.778167987999999E+01
1520 FB=(AF*(D1^4))+(BF*(D1^3))+(CF*(D1^2))+(DF*D1)+EF
1530 AN=-4.666666666999999E-02:BN=-5.414814815000000E-01:CN=-2.302777777999999:DN=-4.364074074000000
1540 EN=-3.092777777999999
1550 DG=(AN*(D1^4))+(BN*(D1^3))+(CN*(D1^2))+(DN*D1)+EN
1560 GOTO 3120
1570 '
1580 '.....high gain
1590 L=LGF*LOG(D*10^5):LL=L^2:LM=LL*1.279999E-02:LN=LM+1.041300:D1=LGF*LOG(D)
1600 COLOR 14,4
1610 IF D1<-4.500000 THEN 1620 ELSE 1630
1620 PRINT "Wire diameter less than 3E-5 wavelengths:  results uncertain."
1630 IF D1>-2 THEN 1640 ELSE 1650
1640 PRINT "Wire diameter greater than 1E-2 wavelengths:  results uncertain."
1650 COLOR 7,0
1660 AD=2.666666670000000E-04:BD=5.066666669999999E-03:CD=3.633333333000000E-02:DD=1.221904762000000E-01
1670 ED=1.183285713999999
1680 DE=(AD*(D1^4))+(BD*(D1^3))+(CD*(D1^2))+(DD*D1)+ED
1690 AR=3.733333333299999E-03:BR=5.362962962999999E-02:CR=2.927555555599999E-01:DR=7.424529101000000E-01
1700 ER=1.814412697999999
1710 RE=(AR*(D1^4))+(BR*(D1^3))+(CR*(D1^2))+(DR*D1)+ER
1720 AI=-2.666666670000000E-03:BI=-3.324444444399999E-02:CI=-1.550666667000000E-01:DI=-3.222793650999999E-01
1730 EI=7.283809524000000E-01
1740 IR=(AI*(D1^4))+(BI*(D1^3))+(CI*(D1^2))+(DI*D1)+EI
1750 BS=3.333333299999999E-04:BS=4.837037037000000E-03:CS=2.552777778000000E-02:DS=5.643756614000000E-02
1760 ES=2.191230158999999E-01
1770 SP=(BS*(D1^4))+(BS*(D1^3))+(CS*(D1^2))+(DS*D1)+ES
1780 AP=-2.333333333000000E-03:BP=-3.128148147999999E-02:CP=-1.558611111099999E-01:DP=-3.417669311999999E-01
1790 EP=-5.499206348999999E-02
1800 IP=(AP*(D1^4))+(BP*(D1^3))+(CP*(D1^2))+(DP*D1)+EP
1810 AZ=4.402900000000000:BZ=5.343954443999999E+01:CZ=2.392408582999999E+02:DZ=4.623614437000000E+02:EZ=3.733035654999999E+02
1820 ZR=(AZ*(D1^4))+(BZ*(D1^3))+(CZ*(D1^2))+(DZ*D1)+EZ
1830 AG=-1.500000000000000E-01:BG=-1.768518519000000:CG=-7.763055555999999:DG=-1.478592593000000E+01:EG=-6.097222219999999E-01
1840 GN=(AG*(D1^4))+(BG*(D1^3))+(CG*(D1^2))+(DG*D1)+EG
1850 AW=1.666666666700000E-01:BW=2.265925925999999:CW=1.170611111100000E+01:DW=2.793058201000000E+01
1860 EW=2.888753968000000E+01
1870 SW=(AW*(D1^4))+(BW*(D1^3))+(CW*(D1^2))+(DW*D1)+EW
1880 AF=1.193333333299999E-01:BF=1.671777777999999:CF=8.988499999999999:DF=2.245931745999999E+01:EF=2.368797619000000E+01
1890 FB=(AF*(D1^4))+(BF*(D1^3))+(CF*(D1^2))+(DF*D1)+EF
1900 DG=0
1910 GOTO 3120
1920 '
1930 '.....4-element calculation
1940 BL=0    'boom length
1950 L=LGF*LOG(D*10^5):LL=L^2:LM=LL*1.279999E-02:LN=LM+1.041300:D1=LGF*LOG(D)
1960 COLOR 14,4
1970 IF D1<-4.500000 THEN 1980 ELSE 1990
1980 PRINT "Wire diameter less than 3E-5 wavelengths:  results uncertain."
1990 IF D1>-2 THEN 2000 ELSE 2010
2000 PRINT "Wire diameter greater than 1E-2 wavelengths:  results uncertain."
2010 COLOR 7,0
2020 AD=-1.800000E-04:BD=-2.359259258999999E-03:CD=-1.090277777999999E-02
2030 DD=-1.971296296000000E-02:ED=1.174938888999999E-01
2040 DE=(AD*(D1^4))+(BD*(D1^3))+(CD*(D1^2))+(DD*D1)+ED:DE=DE*8
2050 AR=2.666666667000000E-04:BR=4.237037036999999E-03:CR=2.554444443999999E-02
2060 DR=7.158756613999999E-02:ER=2.119230159000000E-01
2070 RE=(AR*(D1^4))+(BR*(D1^3))+(CR*(D1^2))+(DR*D1)+ER:RE=RE*8
2080 AI=-1.999999999999999E-04:BI=-2.525925925999999E-03:CI=-1.182777777999999E-02
2090 DI=-2.473915343999999E-02:EI=1.008246031999999E-01
2100 IR=(AI*(D1^4))+(BI*(D1^3))+(CI*(D1^2))+(DI*D1)+EI:IR=IR*8
2110 AT=-6.000000000000000E-04:BT=-9.059259258999999E-03:CT=-4.912777778000000E-02
2120 DT=-1.152343914999999E-01:ET=1.678174603000000E-02
2130 TT=(AT*(D1^4))+(BT*(D1^3))+(CT*(D1^2))+(DT*D1)+ET:TT=TT*8
2140 SP=1.634999E-01:IP=4.810000E-01
2150 ATT=2.666666666699999E-03:BTT=3.688888888900000E-02:CTT=1.770000000000000E-01
2160 DTT=3.386587302000000E-01:ETT=1.046738095000000
2170 TTP=(ATT*(D1^4))+(BTT*(D1^3))+(CTT*(D1^2))+(DTT*D1)+ETT
2180 AZ=1.200000000000000:BZ=1.392592593000000E+01:CZ=6.077777777799999E+01:DZ=1.139177248999999E+02:EZ=1.326182540000000E+02
2190 ZR=(AZ*(D1^4))+(BZ*(D1^3))+(CZ*(D1^2))+(DZ*D1)+EZ
2200 AG=-1.000000000000000E-01:BG=-1.184444444000000:CG=-5.228333333000000:DG=-9.831507937000000:EG=4.045238094999999
2210 GN=(AG*(D1^4))+(BG*(D1^3))+(CG*(D1^2))+(DG*D1)+EG
2220 AW=-6.663333333000000E-02:BW=-6.539148147999999E-01:CW=-1.677836110999999
2230 DW=1.361137830999999:EW=9.502790078999999
2240 SW=(AW*(D1^4))+(BW*(D1^3))+(CW*(D1^2))+(DW*D1)+EW
2250 AF=-3.000000000000000E-02:BF=-2.766666700000000E-01:CF=-4.475000000000000E-01:DF=2.348809523999999:EF=7.853214286000000
2260 FB=(AF*(D1^4))+(BF*(D1^3))+(CF*(D1^2))+(DF*D1)+EF
2270 WL=2.997925E+02/F
2280 WF=9.835592E+02/F
2290 '
2300 A=RE/4:B=A*WL:C=A*WF
2310 Z$=" Reflector sides................. ##.#### wL = ##.### m. = ###.### ft."
2320 COLOR 0,7:PRINT USING Z$;A,B,C:COLOR 7,0
2330 '
2340 A=RE:B=RE*WL:C=RE*WF
2350 Z$=" Reflector perimeter............. ##.#### wL = ##.### m. = ###.### ft."
2360 PRINT USING Z$;A,B,C
2370 '
2380 A=SQR(2*(RE/4)^2):B=A*WL:C=A*WF
2390 Z$=" Reflector diagonal arms......... ##.#### wL = ##.### m. = ###.### ft."
2400 PRINT USING Z$;A,B,C
2410 '
2420 A=SP:B=SP*WL:C=SP*WF
2430 Z$="  Reflector/Driver spacing....... ##.#### wL = ##.### m. = ###.### ft."
2440 COLOR 15:PRINT USING Z$;A,B,C:COLOR 7
2450 '
2460 A=DE/4:B=A*WL:C=A*WF
2470 Z$=" Driver sides.................... ##.#### wL = ##.### m. = ###.### ft."
2480 COLOR 0,7:PRINT USING Z$;A,B,C:COLOR 7,0
2490 '
2500 A=DE:B=A*WL:C=A*WF
2510 Z$=" Driver perimeter................ ##.#### wL = ##.### m. = ###.### ft."
2520 PRINT USING Z$;A,B,C
2530 '
2540 A=SQR(2*(DE/4)^2):B=A*WL:C=A*WF
2550 Z$=" Driver diagonal arms............ ##.#### wL = ##.### m. = ###.### ft."
2560 PRINT USING Z$;A,B,C
2570 '
2580 A=IP-SP:B=A*WL:C=A*WF
2590 Z$="  Driver/Director-1 spacing...... ##.#### wL = ##.### m. = ###.### ft."
2600 COLOR 15:PRINT USING Z$;A,B,C:COLOR 7
2610 '
2620 A=IR/4:B=A*WL:C=A*WF
2630 Z$=" Director-1 sides................ ##.#### wL = ##.### m. = ###.### ft."
2640 COLOR 0,7:PRINT USING Z$;A,B,C:COLOR 7,0
2650 '
2660 A=IR:B=A*WL:C=A*WF
2670 Z$=" Director-1 perimeter............ ##.#### wL = ##.### m. = ###.### ft."
2680 PRINT USING Z$;A,B,C
2690 '
2700 A=SQR(2*(IR/4)^2):B=A*WL:C=A*WF
2710 Z$=" Director-1 diagonal arms........ ##.#### wL = ##.### m. = ###.### ft."
2720 PRINT USING Z$;A,B,C
2730 '
2740 A=TTP-IP:B=A*WL:C=A*WF
2750 Z$="  Director-1/Director-2 spacing.. ##.#### wL = ##.### m. = ###.### ft."
2760 COLOR 15:PRINT USING Z$;A,B,C:COLOR 7
2770 '
2780 BL=TTP  'boom length
2790 '
2800 A=TT/4:B=A*WL:C=A*WF
2810 Z$=" Director-2 sides................ ##.#### wL = ##.### m. = ###.### ft."
2820 COLOR 0,7:PRINT USING Z$;A,B,C:COLOR 7,0
2830 '
2840 A=TT:B=A*WL:C=A*WF
2850 Z$=" Director-2 perimeter............ ##.#### wL = ##.### m. = ###.### ft."
2860 PRINT USING Z$;A,B,C
2870 '
2880 A=SQR(2*(TT/4)^2):B=A*WL:C=A*WF
2890 Z$=" Director-2 diagonal arms........ ##.#### wL = ##.### m. = ###.### ft."
2900 PRINT USING Z$;A,B,C
2910 '
2920 A=BL:B=A*WL:C=A*WF
2930 Z$="  Boom length.................... ##.#### wL = ##.### m. = ###.### ft."
2940 COLOR 15:PRINT USING Z$;A,B,C:COLOR 7
2950 '
2960 PRINT USING " Feedpoint Impedance...........÷ ###.### ohms";ZR
2970 Z=SW/100*F
2980 PRINT USING " 2:1 VSWR bandwidth............÷ ###.### MHz";Z
2990 Z=FB/100*F
3000 PRINT USING " >20 dB F/B ratio bandwidth....÷ ###.### MHz";Z;
3010 PRINT USING ", Free-space gain ÷ ##.### dBi";GN;
3020 GOSUB 5740
3030 CLS:LOCATE 12:PRINT " Want to find minimum turning radius?   (y/n) "
3040 Z$=INKEY$:IF Z$=""THEN 3040
3050 IF Z$="n"THEN 3090
3060 IF Z$="y"THEN 3080
3070 GOTO 3040
3080 PROG$="quad":DIMN$="feet":A=BL*WF:B=RE/4/2*WF:C=TT/4/2*WF:CHAIN"turnrad"
3090 GOTO 180
3100 '
3110 PRINT TAB(T);
3120 '.....screen print (2-el & 3-el)
3130 BL=0    'boom length
3140 A=DE/4:B=A*WL:C=A*WF
3150 Z$=" Driver sides.................... ##.#### wL = ##.### m. = ###.### ft."
3160 COLOR 0,7:PRINT USING Z$;A,B,C:COLOR 7,0
3170 '
3180 A=DE:B=A*WL:C=A*WF
3190 Z$=" Driver perimeter................ ##.#### wL = ##.### m. = ###.### ft."
3200 PRINT USING Z$;A,B,C
3210 '
3220 A=SQR(2*(DE/4)^2):B=A*WL:C=A*WF
3230 Z$=" Driver diagonal arms............ ##.#### wL = ##.### m. = ###.### ft."
3240 PRINT USING Z$;A,B,C
3250 '
3260 A=SP:B=A*WL:C=A*WF
3270 Z$="  Driver/Reflector spacing....... ##.#### wL = ##.### m. = ###.### ft."
3280 COLOR 15:PRINT USING Z$;A,B,C:COLOR 7
3290 '
3300 BL=BL+SP
3310 '
3320 A=RE/4:B=A*WL:C=A*WF
3330 Z$=" Reflector sides................. ##.#### wL = ##.### m. = ###.### ft."
3340 COLOR 0,7:PRINT USING Z$;A,B,C:COLOR 7,0
3350 '
3360 A=RE:B=A*WL:C=A*WF
3370 Z$=" Reflector perimeter............. ##.#### wL = ##.### m. = ###.### ft."
3380 PRINT USING Z$;A,B,C
3390 '
3400 A=SQR(2*(RE/4)^2):B=A*WL:C=A*WF
3410 Z$=" Reflector diagonal arms......... ##.#### wL = ##.### m. = ###.### ft."
3420 PRINT USING Z$;A,B,C
3430 '
3440 IF E$="2"THEN 3640
3450 '
3460 A=IP:B=A*WL:C=A*WF
3470 Z$="  Driver/Director spacing........ ##.#### wL = ##.### m. = ###.### ft."
3480 COLOR 15:PRINT USING Z$;A,B,C:COLOR 7
3490 '
3500 BL=BL+IP
3510 '
3520 A=IR/4:B=A*WL:C=A*WF
3530 Z$=" Director sides.................. ##.#### wL = ##.### m. = ###.### ft."
3540 COLOR 0,7:PRINT USING Z$;A,B,C:COLOR 7,0
3550 '
3560 A=IR:B=A*WL:C=A*WF
3570 Z$=" Director perimeter.............. ##.#### wL = ##.### m. = ###.### ft."
3580 PRINT USING Z$;A,B,C
3590 '
3600 A=SQR(2*(IR/4)^2):B=A*WL:C=A*WF
3610 Z$=" Director diagonal arms.......... ##.#### wL = ##.### m. = ###.### ft."
3620 PRINT USING Z$;A,B,C
3630 '
3640 A=BL:B=BL*WL:C=A*WF
3650 Z$="  Boom length.................... ##.#### wL = ##.### m. = ###.### ft."
3660 COLOR 15:PRINT USING Z$;A,B,C:COLOR 7
3670 '
3680 '.....other data
3690 PRINT USING " Resonant feedpoint impedance..... ÷ ####.### ê";ZR
3700 PRINT USING " Free-space gain.................. ÷ ####.### dBi";GN
3710 PRINT USING " 2:1 VSWR bandwidth............... ÷ ####.### MHz";SW/100*F
3720 PRINT USING " >20 dB front/back ratio bandwidth ÷ ####.### MHz";FB/100*F
3730 IF DG=0 THEN 3760
3740 Z$=" Rate of gain change.............. ÷ ####.### dB per ##.### MHz"
3750 PRINT USING Z$;DG,9.999999E-03*F
3760 GOSUB 5740
3770 CLS:LOCATE 12:PRINT " Want to find minimum turning radius?   (y/n) "
3780 Z$=INKEY$:IF Z$=""THEN 3780
3790 IF Z$="n"THEN 3850
3800 IF Z$="y"THEN 3820
3810 GOTO 3780
3820 IF E$="2"THEN C=DE/4/2*WF
3830 IF E$="3"THEN C=IR/4/2*WF
3840 PROG$="quad":DIMN$="feet":A=BL*WF:B=RE/4/2*WF:CHAIN"turnrad"
3850 IF E$="2"THEN 4080
3860 IF E$="3"THEN 5710
3870 END
3880 '
3890 '.....diagram 1
3900 LOCATE 1,(80-LEN(P$))/2:PRINT P$
3910 COLOR 0,7:J=12
3920 LOCATE ,J:PRINT"                                                          "
3930 LOCATE ,J:PRINT"  ³®ÄÄÄÄ R ÄÄÄÄ¯³   ³®ÄÄÄÄ D ÄÄÄÄ¯³                       "
3940 'IF E$="3"THEN C=IR/4
3950 LOCATE ,J:PRINT"  ³  each side  ³   ³  each side  ³                       "
3960 LOCATE ,J:PRINT"  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄ¿r  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄ¿d                      "
3970 LOCATE ,J:PRINT"  ³             ³   ³             ³   ÇÄÄìÄÄÄÄDÄÄÄÄìÄÄ¶   "
3980 LOCATE ,J:PRINT"  ³   Reflector ³   ³   Driver    ³   º®Ä masts (4) Ä¯º   "
3990 LOCATE ,J:PRINT"  ³             ³   ³             ³   ÇÄìÄÄÄÄÄRÄÄÄÄÄìÄ¶   "
4000 LOCATE ,J:PRINT"  ³   Element   ³   ³   Element   ³  -½-----earth-----½-- "
4010 LOCATE ,J:PRINT"  ³             ³   ³             ³  ÚÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄ¿ "
4020 LOCATE ,J:PRINT" rÀÄÄÄÄÄÄÄÄÄÄÄÄÄÙ  dÀÄÄÄÄÄ x ÄÄÄÄÄÙ  Locate R as close to "
4030 LOCATE ,J:PRINT"                     x = feed point    earth as possible  "
4040 LOCATE ,J:PRINT"                                                          "
4050 COLOR 7,0
4060 RETURN
4070 '
4080 '.....continue diagram
4090 IF F<12 THEN 4200
4100 LOCATE CSRLIN-1
4110 PRINT " A 2 element quad can also be configured as a Near Vertical Incide";
4120 PRINT "nce Skywave "
4130 PRINT " (NVIS) antenna but NVIS antennas are not effective at frequencies";
4140 PRINT " higher than"
4150 PRINT " 12 MHz. Your design is for a frequency of";F;;"MHz."
4160 PRINT
4170 PRINT " .....press any key....."
4180 Z$=INKEY$:IF Z$=""THEN 4180 ELSE CLS:GOTO 180
4190  GOTO 4180
4200 COLOR 0,7
4210 PRINT " Want to configure for a NVIS (Near Vertical Incidence Skywave) an";
4220 PRINT "tenna?  (y/n) ";:COLOR 7,0
4230 Z$=INKEY$:IF Z$=""THEN 4230
4240 IF Z$="n"THEN 180
4250 IF Z$="y"THEN 4280
4260 GOTO 4230
4270 VIEW PRINT LN TO 24:CLS:VIEW PRINT:GOTO 5710
4280 CLS:P$=STR$(F)+" MHz NEAR VERTICAL INCIDENCE SKYWAVE QUAD"
4290 '
4300 GOSUB 3890  'diagram
4310 PRINT
4320 PRINT " R = ";
4330 A=RE/4:B=A*WL:C=A*WF
4340 Z$="  Reflector sides............... ##.#### wL = ##.### m. = ##.### ft."
4350 PRINT USING Z$;A,B,C
4360 '
4370 PRINT " rr= ";
4380 A=SQR(2*(RE/4)^2):B=A*WL:C=A*WF
4390 Z$="  Reflector diagonals........... ##.#### wL = ##.### m. = ##.### ft."
4400 PRINT USING Z$;A,B,C
4410 '
4420 PRINT "     ";
4430 A=SP:B=A*WL:C=A*WF
4440 Z$="  Reflector to Driver spacing... ##.#### wL = ##.### m. = ##.### ft."
4450 PRINT USING Z$;A,B,C
4460 PRINT " D = ";
4470 A=DE/4:B=A*WL:C=A*WF
4480 Z$="  Driver sides.................. ##.#### wL = ##.### m. = ##.### ft."
4490 PRINT USING Z$;A,B,C
4500 '
4510 PRINT " dd= ";
4520 A=SQR(2*(DE/4)^2):B=A*WL:C=A*WF
4530 Z$="  Driver diagonals.............. ##.#### wL = ##.### m. = ##.### ft."
4540 PRINT USING Z$;A,B,C
4550 IF E$="2"THEN 4610
4560 '
4570 PRINT "     ";
4580 A=IP:B=A*WL:C=A*WF
4590 Z$="  Driver to Director spacing.... ##.#### wL = ##.### m. = ##.### ft."
4600 PRINT USING Z$;A,B,C
4610 GOSUB 5740:GOTO 180
4620 '
4630 '.....single element quad
4640 CLS
4650 INPUT " ENTER: Centre frequency of band (MHz).....";F
4660 PRINT " Press number in < > to choose units for wire diameter:"
4670 PRINT UL$;
4680 PRINT "  < 1 >  inches"
4690 PRINT "  < 2 >  millimetres"
4700 PRINT "  < 3 >  wavelengths"
4710 PRINT "  < 4 >  AWG size"
4720 PRINT UL$;
4730 Z$=INKEY$
4740 IF Z$="1"THEN Y$="(inches)":GOTO 4790
4750 IF Z$="2"THEN Y$="(mm.)":GOTO 4790
4760 IF Z$="3"THEN Y$="(wavelengths)":GOTO 4790
4770 IF Z$="4"THEN Y$="(AWG size)":GOTO 4790
4780 GOTO 4730
4790 PRINT " ENTER: Wire diameter ";Y$;;" ";:INPUT WD
4800 IF Z$="1"THEN WLI=SL/2.539999E+01/F:D=WD/WLI  'WLI=freq. in kHz
4810 IF Z$="2"THEN WLI=SL/F:D=WD/WLI
4820 IF Z$="3"THEN D=WD
4830 IF Z$="4"THEN D=(GA(WD)*2.539999E+01)/(SL/F)
4840 '
4850 CLS
4860 PRINT " SINGLE ELEMENT SQUARE QUAD"
4870 PRINT "  (wL = wavelength)"
4880 PRINT UL$;
4890 PRINT USING " Operating frequency........... ###.### MHz";F
4900 WL=SL/F/10^3:WF=WL/3.048000E-01
4910 Z$=" Operating wavelength.......... ###.### m.  = ###.### ft."
4920 PRINT USING Z$;WL,WF
4930 Z$=" Wire diameter in wavelengths..  ##.###^^^^ wL"
4940 PRINT USING Z$;D
4950 A=SL/F*D:B=A/2.539999E+01:C=(LOG(4.600000E-01/B))/LOG(1.122900)-3
4960 Z$=" Wire diameter................. ###.### mm. = ##.### in.= AWG ##.#"
4970 PRINT USING Z$;A,B,C
4980 PRINT UL$;
4990 LF=1/LOG(10)
5000 L=LF*LOG(D*10^5):LL=L^2:LM=LL*1.279999E-02:LN=LM+1.041300
5010 PM=LN*WL:PF=LN*WF
5020 PRINT " ELEMENT DIMENSIONS"
5030 PRINT UL$;
5040 Z$=" Perimeter..................... ##.##### wL = ###.### m. = ###.### ft."
5050 PRINT USING Z$;LN,PM,PF
5060 Z$=" Each side..................... ##.##### wL = ###.### m. = ###.### ft."
5070 PRINT USING Z$;LN/4,PM/4,PF/4
5080 A=SQR(2*(LN/4)^2):B=A*WL:C=A*WF
5090 Z$=" Diagonal...................... ##.##### wL = ###.### m. = ###.### ft."
5100 PRINT USING Z$;A,B,C
5110 PRINT UL$;
5120 GOSUB 5740:GOTO 180
5130 END
5140 '
5150 '.....text
5160 T=(8)   'tab
5170 PRINT TAB(T);
5180 PRINT "  Most published design data for quad antennas determine the sizes"
5190 PRINT TAB(T);
5200 PRINT "of the elements by dividing an empirical number (usually in the"
5210 PRINT TAB(T);
5220 PRINT "order of 1005 or thereabouts) by the frequency in MHz to arrive at"
5230 PRINT TAB(T);
5240 PRINT "the length of the perimeter in feet. Actually, the factor 1005"
5250 PRINT TAB(T);
5260 PRINT "applies only at some single arbitrary frequency with some single"
5270 PRINT TAB(T);
5280 PRINT "arbitrary conductor diameter. In reality, this empirical factor "
5290 PRINT TAB(T);
5300 PRINT "varies considerably with frequency and conductor diameter."
5310 PRINT TAB(T);
5320 PRINT "(ref. ARRL Publication #36, Antenna Anthology, 1978, p.56).";
5330 PRINT TAB(T);
5340 PRINT "  Dimensions are not critical, as the quad is relatively broad in"
5350 PRINT TAB(T);
5360 PRINT "frequency response, therefore element lengths calculated by that"
5370 PRINT TAB(T);
5380 PRINT "method can usually be made to work to some degree of satisfaction."
5390 PRINT TAB(T);
5400 PRINT "  More precise dimensions can be calculated but seldom are, due to"
5410 PRINT TAB(T);
5420 PRINT "the complex mathematics involved. This program utilizes the"
5430 PRINT TAB(T);
5440 PRINT "mathematical, rather than the empirical, method of calculation to"
5450 PRINT TAB(T);
5460 PRINT "design square quad beams as modelled by L.B.Cebik, W4RNL, using"
5470 PRINT TAB(T);
5480 PRINT "NEC antenna software."
5490 RETURN
5500 '
5510 '.....anatomy diagram
5520 COLOR 0,7:C=21
5530 LOCATE ,C:PRINT "         TYPICAL QUAD ANTENNA          "
5540 LOCATE ,C:PRINT "                                       "
5550 LOCATE ,C:PRINT "         ³®Ä reflector                 "
5560 LOCATE ,C:PRINT "         ³   ³®Ä driver                "
5570 LOCATE ,C:PRINT "         ³   ³   ³®Ä director          "
5580 LOCATE ,C:PRINT "         ³   ³   ³   ³®Ä director      "
5590 LOCATE ,C:PRINT "  boom Ä¯ÅÄÄÄÅÄ×ÄÅÄÄÄÅ    ¯Ä gain Ä¯   "
5600 LOCATE ,C:PRINT "         ³   ³ º ³   ³                 "
5610 LOCATE ,C:PRINT "         ³   ³ º ³                     "
5620 LOCATE ,C:PRINT "         ³ Ú¯³ º                       "
5630 LOCATE ,C:PRINT "         ³ ³   º®Ä mast                "
5640 LOCATE ,C:PRINT " ÚÄÄÄÄÄÄÄÄÄÙ   º                       "
5650 LOCATE ,C:PRINT " À Feed at centre of a driver side;    "
5660 LOCATE ,C:PRINT " -bottom side for horizontal polarity, "
5670 LOCATE ,C:PRINT " -vertical side for vertical polarity. "
5680 COLOR 7,0
5690 RETURN
5700 '
5710 '.....end
5720 GOSUB 5740:GOTO 180
5730 '
5740 'PRT
5750 KEY OFF:GOSUB 5820:LOCATE 25,5:COLOR 0,2
5760 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
5770 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
5780 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 5780 ELSE GOSUB 5820
5790 IF Z$="3"THEN RETURN
5800 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
5810 IF Z$="2"THEN LPRINT CHR$(12) ELSE 5750
5820 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

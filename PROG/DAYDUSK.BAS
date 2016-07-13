1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 '\hamcalc\prog\DAYDUSK - Daylight, Dawn & Dusk - 24 MAY 01
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 
50 COLOR 7,0,0
60 PI=4*ATN(1)  '3.141593
70 A=PI/2
80 B=PI
90 C=1.500000*PI
100 D=2*PI
110 U1$=STRING$(80,205)
120 U2$=STRING$(40,196)
130 '
140 '.....title page
150 CLS
160 COLOR 15,2
170 PRINT " DAYLIGHT DUSK & DAWN CALCULATOR";TAB(66);"Author Unknown ";
180 COLOR 1,0:PRINT STRING$(80,223);
190 COLOR 15,2:LOCATE CSRLIN-1,18
200 PRINT " Edited for HAMCALC by George Murphy, VE3ERP "
210 COLOR 7,0
220 PRINT:GOSUB 2740   'proloque
230 PRINT U1$;
240 PRINT " Press number in < > to:"
250 PRINT U1$;
260 PRINT "   < 1 > run this program"
270 PRINT "   < 2 > run Sunrise/Sunset program"
280 PRINT "   < 0 > EXIT";
290 Z$=INKEY$:IF Z$=""THEN 290
300 IF Z$="0"THEN CLS:CHAIN EX$
310 IF Z$="1"THEN 360
320 IF Z$="2"THEN CLS:CHAIN"\hamcalc\prog\riseset"
330 GOTO 290
340 END
350 '
360 '.....data input
370 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 4
380 INPUT " ENTER: Latitude, in decimal degrees (minus if south)...";Z1
390 IF SGN(Z1)=-1 THEN LAT$="S"ELSE LAT$="N"
400 LAT=Z1*1.745329E-02
410 INPUT " ENTER: Longitude, in decimal degrees (minus if west)...";Z2
420 IF SGN(Z2)=-1 THEN LNG$="W"ELSE LNG$="E"
430 LNG=-Z2*1.745329E-02
440 UT=FIX(Z2/15)   'UTC time differential
450 VIEW PRINT 4 TO 24:CLS:VIEW PRINT:LOCATE 4
460 Z$=" Location..............  ##.#ø"+LAT$+" ###.#ø"+LNG$
470 Z$=Z$+".   Local UTC Zone: UTC +## hours"
480 PRINT USING Z$;ABS(Z1),ABS(Z2),UT
490 PRINT U1$;
500 '
510 INPUT " ENTER: Year...........";H
520 INPUT " ENTER: Month no. .....";I
530 INPUT " ENTER: Day no. .......";J
540 VIEW PRINT 5 TO 24:CLS:VIEW PRINT:LOCATE 5
550 PRINT USING " Date (y/m/d).......... ####/##/##";H,I,J
560 '
570 REM UT=FIX(Z2/15)   'UTC time differential
580 '
590 GOSUB 1370
600 ZONE=-UT*2.617990E-01  'UTC time zone
610 '
620 '.....day of year
630 K=INT((I+9)/12)
640 X=H/4
650 Y=INT(X)
660 Z=X-Y
670 IF Z=0 THEN 690
680 K=K*2
690 H=INT(275*I/9)
700 H=H+J-K-30
710 '
720 '.....rising phenomena
730 I=0
740 J=A
750 GOSUB 1500
760 '
770 PRINT U1$;
780 '
790 R=-3.090170E-01
800 PRINT TAB(22);"local solar"
810 PRINT TAB(25);"time"
820 GOSUB 2030
830 PRINT " Astronomical Dawn..... ";V$;;" = ";UTC$;;" UTC";
840 PRINT "   Sun 18ø below horizon"
850 '
860 R=-2.079119E-01
870 GOSUB 2030
880 PRINT " Nautical Dawn......... ";V$;;" = ";UTC$;;" UTC";
890 PRINT "   Sun 12ø below horizon"
900 '
910 R=-1.045280E-01
920 GOSUB 2030
930 PRINT " Civil Dawn............ ";V$;;" = ";UTC$;;" UTC";
940 PRINT "   Sun  6ø below horizon"
950 '
960 R=-1.454390E-02
970 GOSUB 2030
980 PRINT " Sunrise............... ";V$;;" = ";UTC$;;" UTC";
990 PRINT "   Top of sun at the horizon"
1000 '
1010 PRINT U2$
1020 '
1030 '.....setting phenomena
1040 I=1
1050 J=C
1060 GOSUB 1500
1070 '
1080 R=-1.454390E-02
1090 GOSUB 2030
1100 PRINT " Sunset................ ";V$;;" = ";UTC$;;" UTC";
1110 PRINT "   Top of sun at the horizon"
1120 '
1130 R=-1.045280E-01
1140 GOSUB 2030
1150 PRINT " Civil Dusk............ ";V$;;" = ";UTC$;;" UTC";
1160 PRINT "   Sun  6ø below horizon"
1170 '
1180 R=-2.079119E-01
1190 GOSUB 2030
1200 PRINT " Nautical Dusk......... ";V$;;" = ";UTC$;;" UTC";
1210 PRINT "   Sun 12ø below horizon"
1220 '
1230 R=-3.090170E-01
1240 GOSUB 2030
1250 PRINT " Astronomical Dusk..... ";V$;;" = ";UTC$;;" UTC";
1260 PRINT "   Sun 18ø below horizon"
1270 '
1280 PRINT U1$;
1290 PRINT " UTC zone local SOLAR times shown. Times vary with longitude acro";
1300 PRINT "ss the UTC  "
1310 PRINT " time zone at the rate of 4 minutes per degree of longitude.     "
1320 PRINT U1$;
1330 GOTO 3000
1340 '
1350 '.....sexagesimal to decimal
1360 '
1370 W=1
1380 IF Z>=0 THEN 1460
1390 W=-1
1400 Z=ABS(Z)
1410 X=INT(Z)
1420 Z=(Z-X)*100
1430 Y=INT(Z)
1440 Z=(Z-Y)*100
1450 Z=(X+Y/60+Z/3600)*W
1460 RETURN
1470 '
1480 '.....approximate time
1490 '
1500 K=H+((J+LNG)/D)
1510 '
1520 '.....solar mean anomoly
1530 '
1540 L=K*1.720199E-02
1550 L=L-5.740389E-02
1560 '
1570 '.....solar true longitutde
1580 '
1590 Z=SIN(L)
1600 M=L+3.344050E-02*Z
1610 Z=SIN(2*L)
1620 M=M+3.490659E-04*Z
1630 M=M+4.932889
1640 '
1650 '.....quadrant determination
1660 '
1670 Z=M
1680 GOSUB 2660
1690 M=Z
1700 X=M/A
1710 Y=INT(X)
1720 Z=X-Y
1730 IF Z<>0 THEN 1750
1740 M=M+4.848140E-06
1750 N=2
1760 IF M>C THEN 1830
1770 N=1
1780 IF M>A THEN 1830
1790 N=0
1800 '
1810 '.....solar right ascension
1820 '
1830 P=9.174600E-01*TAN(M)
1840 P=ATN(P)
1850 '
1860 '.....quadrant adjustment
1870 '
1880 IF N=0 THEN 1960
1890 IF N=2 THEN 1920
1900 P=P+B
1910 GOTO 1960
1920 P=P+D
1930 '
1940 '.....solar declination
1950 '
1960 Q=3.978199E-01*SIN(M)
1970 Q=Q/SQR(-Q*Q+1)
1980 Q=ATN(Q)
1990 RETURN
2000 '
2010 '.....coordinate conversion
2020 '
2030 S=R-(SIN(Q)*SIN(LAT))
2040 S=S/(COS(Q)*COS(LAT))
2050 '
2060 '.....null phenomenon
2070 '
2080 Z=ABS(S)
2090 IF Z<=1 THEN 2150
2100 V=0
2110 RETURN
2120 '
2130 '.....adjustment
2140 '
2150 S=S/SQR(-S*S+1)
2160 S=-ATN(S)+A
2170 IF I=1 THEN 2220
2180 S=D-S
2190 '
2200 '.....local apparent time
2210 '
2220 Z=1.720280E-02*K
2230 T=S+P-Z-1.733639
2240 '
2250 '.....universal time
2260 '
2270 U=T+LNG
2280 '
2290 '.....wall clock time
2300 '
2310 V=U-ZONE
2320 '
2330 '.....decimal to sexigesimal
2340 '
2350 Z=V
2360 GOSUB 2660
2370 Z=Z*3.819720
2380 V=INT(Z)
2390 W=(Z-V)*60
2400 X=INT(W)
2410 Y=W-X
2420 IF Y<5.000000E-01 THEN 2440
2430 X=X+1
2440 IF X<60 THEN 2500
2450 V=V+1
2460 X=0
2470 '
2480 '.....conventional format
2490 '
2500 Z$="00"
2510 HR$=MID$(STR$(V),2)
2520 HR$=RIGHT$(Z$+HR$,2)
2530 MI$=MID$(STR$(X),2)
2540 MI$=RIGHT$(Z$+MI$,2)
2550 V$=HR$+":"+MI$
2560 UTC=VAL(HR$)-UT
2570 IF UTC<0 THEN UTC=UTC+24:GOTO 2590
2580 IF UTC>24 THEN UTC=UTC-24
2590 UTC$=STR$(UTC):UTC$=RIGHT$(UTC$,LEN(UTC$)-1)
2600 IF LEN(UTC$)<2 THEN UTC$="0"+UTC$:GOTO 2600
2610 UTC$=UTC$+MI$
2620 RETURN
2630 '
2640 '.....normalization
2650 '
2660 IF Z>=0 THEN 2690
2670 Z=Z+D
2680 GOTO 2660
2690 IF Z<D THEN 2720
2700 Z=Z-D
2710 GOTO 2690
2720 RETURN
2730 '
2740 '.....prologue
2750 PRINT " This program computes times of sunrise, sunset, dawn and dusk at";
2760 PRINT " any location."
2770 PRINT
2780 PRINT " Enter the latitude and longitude of the location in decimal degr";
2790 PRINT "ees. If the   "
2800 PRINT " latitude is south of the equator enter the latitude as a minus (";
2810 PRINT "-) value or as"
2820 PRINT " a positive value if north of the equator. If the longitude is we";
2830 PRINT "st of the     "
2840 PRINT " prime meridian (0ø - Greenwich) enter the longitude as a minus (";
2850 PRINT "-) value or as"
2860 PRINT " a positive value if west of the prime meridian.                 "
2870 PRINT
2880 PRINT " Times calculated are local sidereal (SOLAR) times. Forget about ";
2890 PRINT "Standard Time,"
2900 PRINT " Daylight Saving Time, local political time, UTC, or any other ma";
2910 PRINT "n-made time   "
2920 PRINT " system. Sidereal time is time referenced to the stars. It is the";
2930 PRINT " time shown   "
2940 PRINT " for millenia on properly installed sun dials.                   "
2950 PRINT
2960 PRINT " Related data can also be calculated using Hamcalc's `Sunrise/Sun";
2970 PRINT "set' program. "
2980 RETURN
2990 '
3000 '.....end
3010 GOSUB 3030:GOTO 140
3020 '
3030 '.....PRT
3040 KEY OFF:GOSUB 3110:LOCATE 25,5:COLOR 0,2
3050 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
3060 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
3070 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 3070 ELSE GOSUB 3110
3080 IF Z$="3"THEN RETURN
3090 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
3100 IF Z$="2"THEN LPRINT CHR$(12) ELSE 3040
3110 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

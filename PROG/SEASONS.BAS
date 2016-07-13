1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'SEASONS  21 DEC 03
20 CLS:KEY OFF
30 '
40 COLOR 7,0,0
50 IF EX$=""THEN EX$="EXIT"
60 DIM M$(12),D(12),D$(12)
70 AM$="##:## am":AZ$=" ###.#ø":PM$="##:## pm":EL$=" ##.#ø"
80 P$="##.##":P1$="####":P2$="##.#
90 UL$=STRING$(80,205)
100 PI=4*ATN(1)
110 DR=PI/180:RD=180/PI         'DEG to RAD, RAD to DEG
120 '
130 CLS:VIEW PRINT:COLOR 0,7
140 LOCATE 1,31:PRINT " SEASONS ";:COLOR 7,0
150 PRINT UL$;
160 PRINT "Press number in ( ) to:"
170 PRINT " (1)  Run program"
180 PRINT " (0)  Exit"
190 Z$=INKEY$:IF Z$=""THEN 190
200 IF Z$="0"THEN CLS:CHAIN EX$
210 IF Z$="1"THEN 230 ELSE 190
220 GOTO 190
230 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
240 LINE INPUT "Name of your location.......? ";N$
242 PRINT
245 PRINT "Note: Daylight hours cannot be calculated accurately at latitudes";
247 PRINT " over 66.6ø"
248 PRINT
250 INPUT "ENTER: Your latitude  (XX.X degrees, minus if SOUTH).....";LX
260 OO=LX:GOSUB 1700
270 INPUT "ENTER: Your longitude (XX.X degrees, minus if EAST)......";LO
280 OO=LO:GOSUB 1700
290 LA=LX*DR     'Latitude in Radians
300 LN=CSRLIN:PRINT "Press number in ( ) to indicate your Time Zone:"
310 PRINT " (1) Atlantic";
320 PRINT " (2) Eastern";
330 PRINT " (3) Central";
340 PRINT " (4) Mountain";
350 PRINT " (5) Pacific";
360 PRINT " (6) Other";
370 Z$=INKEY$:IF VAL(Z$)>=1 AND VAL(Z$)<=6 THEN 380 ELSE 370
380 IF Z$="1"THEN SX=60:SX$="AST"
390 IF Z$="2"THEN SX=75:SX$="EST"
400 IF Z$="3"THEN SX=90:SX$="CST"
410 IF Z$="4"THEN SX=105:SX$="MST"
420 IF Z$="5"THEN SX=120:SX$="PST"
430 IF Z$="6"THEN SX=INT(LO/15)*15:SX$="Local"
440 MX=LO-SX                                'Deg. correction for std. meridian
450 MC=MX/15                                'Hrs. correction for std. meridian
460 MR=MX*DR                                'Rad equiv. of meridian correction
470 INPUT "ENTER: Year to be used in calculations (yyyy)............";YR
480 COLOR 0,7:LOCATE 1,40:PRINT " - ";YR;;" ";:COLOR 7,0
490 OO=YR:GOSUB 1700:GOSUB 1700:Z=(YR/100-INT(YR/100))*10
500 Y=INT(Z):X=(Z-Y)*10:W=(Y+X)/4:W=INT(W*1.000000E+05+5.000000E-01)/1.000000E+05
510 IF INT(W)=W THEN Q1=29:DA=366 ELSE Q1=28:DA=3.652423E+02
520 DATA JAN,31,FEB,0,MAR,31,APR,30,MAY,31,JUN,30
530 DATA JUL,31,AUG,31,SEP,30,OCT,31,NOV,30,DEC,31
540 FOR Z=1 TO 12:READ M$(Z),D$(Z):D(Z)=VAL(D$(Z)):NEXT Z:RESTORE:D(2)=Q1
550 K=360/DA                                'Days to degrees
560 '
570 N1=0:N2=0:N3=0:N4=0:YZ=1
580 M1=1:D1=1:GOSUB 1670:MO1=M1
590 N1=1
600 M1=12:D1=31:GOSUB 1670:MO2=M1
610 N2=365
620 '
630 '.....sunrise/sunset
640 X1=5.000000E-02  'increment in days
650 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
660 PRINT TAB(4);"Date";TAB(14);"Daylight";TAB(24);"Sunrise (";SX$;;")";
670 PRINT TAB(45);"Sunset (";SX$;;")";TAB(64);;"ÚÄÄÄÄÄ Noon ÄÄÄÄ¿";
680 PRINT TAB(14);"hours";
690 PRINT TAB(24);"and Azimuth";TAB(45);"and Azimuth";TAB(65);"Sun  Earth Axis"
700 PRINT UL$;
710 GOSUB 1760
720 LN=4
730 FOR N=N1 TO N2 STEP X1
740 LN=LN+1
750 GOSUB 1370
760 CC=PI/2                 '90 degrees at sunrise, sunset
770 IF LA-Z<0 THEN T=(ED-MG+MX+5.000000E-01)ELSE  T=(180-ED-MG-MX-5.000000E-01)
780 IF T<0 THEN T=360+T
790 AA=CC-Z
800 BB=CC-LA
810 SS=(AA+BB+CC)/2
820 TT=(SIN(SS-AA)*SIN(SS-BB)*SIN(SS-CC))/SIN(SS)
830 IF TT<0 THEN TT=0
840 TR=SQR(TT)
850 C1=TR/SIN(SS-CC)
860 C=2*ATN(C1)*RD                'Angle in degrees
870 AZ=2*RD*ATN(TR/SIN(SS-AA))    'Azimuth in degrees
880 CH=C/15                       'Hours vs. C
890 '.....sunrise
900 R=12-5.600000E-02+ET+MC-CH:H1    =R   'Sunrise
910 R$=STR$(INT(R)):RL=LEN(R$)-1
920 RH$=RIGHT$((R$),RL)           'sunrise decimal hour
930 IF VAL(RH$)<10 THEN RH$="0"+RH$
940 M=INT((R-INT(R))*60)
950 RM$=STR$(M):ML=LEN(RM$)-1
960 RM$=RIGHT$((RM$),ML)          'sunrise decimal minute
970 IF VAL(RM$)<10 THEN RM$="0"+RM$
980 RT$=RH$+":"+RM$               'sunrise time
990 '.....sunset
1000 S=12+5.600000E-02+ET+MC+CH:H2=S  'Sunset
1010 S$=STR$(INT(S)):SL=LEN(S$)-1
1020 SH$=RIGHT$((S$),SL)           'sunset decimal hour
1030 IF VAL(SH$)<10 THEN SH$="0"+SH$
1040 M=INT((S-INT(S))*60)
1050 SM$=STR$(M):ML=LEN(SM$)-1
1060 SM$=RIGHT$((SM$),ML)          'sunset decimal minute
1070 IF VAL(SM$)<10 THEN SM$="0"+SM$
1080 ST$=SH$+":"+SM$               'sunset time
1090 IF LA-Z<0 THEN AL=FIX(90-(Z-LA)*RD-5.000000E-01)ELSE AL=(90-(LA-Z)*RD-5.000000E-01)
1100 RISE=AZ-MG-5.000000E-01     'sunrise
1110 SET=360-AZ-MG-5.000000E-01  'sunset
1120 DECL=Z*180/PI     'declination of earth's axis
1130 'P=BB/2*180/PI     'maximum DECL
1140 P=2.344260E+01         ' earth's orbital tilt  23ø27'
1150 '.....equinoxes
1160 J=DECL-BB/2/PI
1170 IF J>X1 THEN 1190
1180 IF J<X1 AND DECL>0 THEN H=J:GOTO 1220
1190 IF (P-DECL)<X1 THEN H=J:GOTO 1220
1200 IF (DECL+P)<=X1 THEN H=J:GOTO 1220
1210 GOTO 1330    'next n
1220 '.....print
1230 GOSUB 1510
1240 PRINT TAB(15);USING "##.#";S-R; 'SD-SU;
1250 PRINT TAB(24);RT$; ;" @";        ' USING AM$;SU,MU;:PRINT " @";
1260 PRINT USING AZ$;RISE;
1270 PRINT TAB(45);ST$;;" @";
1280 PRINT USING AZ$;SET;
1290 PRINT TAB(65);USING "##.#ø";AL;
1300 PRINT TAB(73);USING  "##.#ø";ABS(H)
1310 IF LN<24 THEN PRINT "";:GOTO 1330
1320 N=N+10
1330 NEXT N
1340 PRINT :PRINT UL$;
1350 GOTO 1830   'end
1360 '
1370 '.....compute long.of GP SUN & EQ TIME
1380 NN=N:IF N>365 THEN NN=N-365
1390 L1=(2.795750E+02+(K*NN))*DR
1400 G1=(3.569670E+02+(K*NN))*DR
1410 LD=L1+(1.916000*SIN(G1)+1.999999E-02*SIN(2*G1))*DR
1420 DL=3.978199E-01*SIN(LD)
1430 Z=ATN(DL/SQR(-DL*DL+1))
1440 EL=-1.046999E+02*SIN(L1)+5.962000E+02*SIN(2*L1)+4.300000*SIN(3*L1)-1.269999E+01
1450 EL=EL*SIN(4*L1)-4.292999E+02*COS(L1)-2*COS(2*L1)+1.929999E+01*COS(3*L1)
1460 ET=-EL/3600
1470 ED=ET*15
1480 ER=ED*DR
1490 RETURN
1500 '
1510 '.....print month and day
1520 NN=N:IF N>365 THEN NN=N-365
1530 Q1=0:FOR Q=1 TO 12:Q2=Q1:Q1=Q1+D(Q):IF NN<=Q1 THEN 1560
1540 NEXT Q
1560 IF Q=3 THEN PRINT:PRINT;"Spring Equinox: Equal hours of daylight and darkness "
1570 IF Q=6 THEN PRINT:PRINT;"Summer Solstice: Longest day of the year "
1580 IF Q=9 THEN PRINT:PRINT;"Autumn Equinox: Equal hours of daylight and darkness "
1590 IF Q=12 THEN PRINT:PRINT;"Winter Solstice: Shortest day of the year "
1600 PRINT M$(Q);;" ";
1610 A=CINT(NN-Q2):A$=STR$(A):M=LEN(A$)-1
1620 A$=RIGHT$((A$),M)
1630 IF VAL(A$)<10 THEN A$="0"+A$
1640 PRINT A$;;"/";YR$(YZ);
1650 RETURN
1660 '
1670 '.....convert date to day of year
1680 Q1=0:FOR Q=1 TO M1:Q2=Q1:Q1=Q1+D(Q):NEXT Q:NQ=Q2+D1:RETURN
1690 '
1700 '......subsequent years
1710 Y$(1)=STR$(YR):Y$(2)=STR$(YR+1)
1720 IF MO1=MO2 THEN Y$(2)=Y$(1)
1730 FOR Q=1 TO 2:YR$(Q)=RIGHT$(Y$(Q),2):NEXT Q
1740 RETURN
1750 '
1760 IF LX>0 THEN LX$="øN."ELSE LX$="øS."
1770 IF LO>0 THEN LO$="øW."ELSE LO$="øE."
1780 LOCATE 5,13:PRINT "µat ";N$;" (";STR$(ABS(LX));LX$;STR$(ABS(LO));LO$;")ÆÍ"
1790 LOCATE CSRLIN-1,65:PRINT " EL "
1800 LOCATE CSRLIN-1,72:PRINT " DECL "
1810 RETURN
1820 '
1830 '.....end
1840 GOSUB 1860:GOTO 130
1850 '
1860 '.....PRT
1870 KEY OFF:GOSUB 1940:LOCATE 25,5:COLOR 0,2
1880 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1890 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1900 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1900 ELSE GOSUB 1940
1910 IF Z$="3"THEN RETURN
1920 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1930 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1870
1940 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

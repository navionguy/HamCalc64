1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'ANTMATCH - Antenna Matching Networks - 06 FEB 2002
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 
50 DIM L(20),C(20),G(20)
60 DIM A(8,9), B(8,9), X(8), NPIVROW(8,2), NPIVCOL(8,2)
70 PI=4*ATN(1)  '3.141593
80 UL$=STRING$(80,205)
90 '
100 '.....title page
110 CLS
120 COLOR 15,2,0
130 PRINT " ANTENNA MATCHING NETWORKS";TAB(66);"by R.J.Dehoney ";
140 COLOR 1,0:PRINT STRING$(80,223);
150 COLOR 7,0
160 '
170 PRINT " This program solves for the element values of a matching network ";
180 PRINT "for a dipole "
190 PRINT " or monopole antenna. After you enter the highest passband frequen";
200 PRINT "cy and the   "
210 PRINT " antenna diamter, the program calculates the lower band edge and Z";
220 PRINT "o for trial  "
230 PRINT " values of length, number of poles and SWR. The value of all these";
240 PRINT " elements can"
250 PRINT " be changed to arrive at a suitable design."
260 PRINT
270 PRINT " The procedure uses an RLC network to simulate the imput impedance";
280 PRINT " of dipole   "
290 PRINT " and monopole antennas. The RLC values are functions of the antenn";
300 PRINT "a length and "
310 PRINT " diameter. As an option, measured Zin data can be entered from whi";
320 PRINT "ch the RLC   "
330 PRINT " values are calculated. The representation is good over a restrict";
340 PRINT "ed range of  "
350 PRINT " L, D and SWR. Within this range, impressive bandwidths can be ach";
360 PRINT "ieved.       "
370 PRINT " Outside this range, you must use some other program. e.g., Hamcal";
380 PRINT "c's MATCHBOX "
390 PRINT " IMPEDANCE TRANSFORMER."
400 LOCATE ,32:COLOR 0,7:PRINT " Program Hints: ":COLOR 7,0
410 PRINT " As you 'TUNE' the various parameters, you may hear a 'BEEP' and s";
420 PRINT "ee no further"
430 PRINT " change in values. This means you have reached one of the program ";
440 PRINT "limits, for  "
450 PRINT " instance negative matching elements, or SWRmin>SWRmax or too grea";
460 PRINT "t a diameter "
470 PRINT " or length."
480 PRINT:LOCATE ,23
490 COLOR 0,7:PRINT " Press 1 to continue or 0 to EXIT ":COLOR 7,0
500 A$=INKEY$:IF A$=""THEN 500
510 IF A$="0"THEN CLS:CHAIN EX$
520 IF A$="1"THEN 540
530 GOTO 500
540 COLOR 7,0,0:CLS
550 INPUT "Is antenna a dipole or a monopole (d/m)? ",A$
560 IF A$="d" THEN B$="dipole": KA=1 :GOTO 580
570 IF A$="m" THEN B$="monopole" :KA=2 ELSE CLS :GOTO 550
580 NA=3 :SWRMAX=1.049999 :SWRMIN=1
590 PRINT :GOSUB 2570 ::REM'Calculate G(n)'s
600 INPUT "ENTER: highest operating frequency in MHz ";F0 :FH=F0*10^6
610 IF F0=0 THEN GOTO 600 ELSE LMAX=CINT(1.800000E+09/FH/KA)/10 :DMAX=LMAX*KA*2
620 PRINT :PRINT " Dimension limits: Length...";8.399999E-01*LMAX;"to";LMAX;"meters,"
630 PRINT "         Maximum Diameter...";DMAX;"cm
640 PRINT :PRINT "Do you have 4 sets of Zin data between";7.400000E-01*F0;"and";F0;"MHz"
650 PRINT "for such a ";B$;"? (y/n)";:INPUT " ",A$
660 IF A$="n" THEN GOTO 690 ELSE IF A$<>"y" GOTO 640
670 FLAG2=1 :GOSUB 3310 :GOSUB 810 :IF FLAG=1 THEN GOSUB 4380
680 CLS :GOSUB 1080 :GOTO 1550
690 PRINT '
700 PRINT "ENTER: ";B$;" element diameter (";DMAX;"cm or less)";:INPUT " ";D
710 IF D>DMAX OR D=<0 THEN GOTO  700 ELSE D=D/100
720 L0=LMAX :CLS :GOSUB 730 :GOSUB 990 :GOTO 1260
730 '.....subroutine to calc RA-CB
740 L=L0*KA
750 R=L/D :FLAG=0 :LX=LOG(2*L/D)/LOG(10)
760 CB=1.206739E+01*L/2/(LX-7.245000E-01)*9.999999E-13
770 CA=L*(8.907499E-01/(LX^8.005999E-01-8.610000E-01)-2.541000E-02)*9.999999E-13
780 LA=1.000000E-01*L*(1.481299*LX^1.011999-6.187999E-01)*9.999999E-07
790 RA=(4.128800E-01*LX^2+7.407539*(2*L/D)^-2.388999E-02-7.274079)*1000
800 '......subroutine to calc L's & C's
810 FL=FH/(G(1)*LA/RA*2*PI*FH+1)
820 BW=FH-FL :BWR=2*PI*BW :F0=SQR(FH*FL) :W0=2*PI*F0
830 CP=G(1)/BWR/RA
840 M=CB/(CA+CB-CP)
850 CC=CB*(M-1) :CS=CB/M
860 IF (-1)^NA<0 THEN Z0=RA/M/M/SWRMIN ELSE Z0=RA/M/M/SWRMAX
870 L(2)=G(2)*RA/BWR :C2=1/W0/W0/L(2) :C(2)=CS*C2/(CS-C2)
880 FOR I=3 TO NA STEP 2
890 C(I)=G(I)/RA/BWR*M*M
900 L(I)=1/C(I)/W0/W0
910 NEXT I
920 FOR I=4 TO NA STEP 2
930 L(I)=G(I)*RA/BWR/M/M
940 C(I)=1/L(I)/W0/W0
950 NEXT I
960 L(2)=L(2)/M/M :C(2)=C(2)*M*M
970 FOR M=1 TO 10 :IF C(M)<0 OR CC<0 THEN FLAG=1
980 NEXT M :RETURN
990 '
1000 '.....subroutine to print values
1010 CLS
1020 P$="Lmax= ###.# m.   ³N= ###        ³L/Dmin= ###.##"
1030 PRINT USING P$;LMAX,NA,50/KA
1040 P$="L=    ###.### m. ³D= ###.### cm.³L/D=  #####.##"';L0,D*100,L/D/KA
1050 PRINT USING P$;L0,D*100,L/D/KA
1060 P$="FL=  ####.### MHz³FH=###.### MHz³BW=   #####.##%"
1070 PRINT USING P$;FL/10^6,FH/10^6,BW/F0*100
1080 P$="Zo= #####.### ê  ³SQRmax= ###.##³SWRmin= ###.##"
1090 PRINT USING P$;Z0/KA,SWRMAX,SWRMIN
1100 PRINT :IF KA=1 THEN GOSUB 2420 ELSE GOSUB 2500
1110 LOCATE 1,53 :PRINT "ÍÍÍÍ ANTENNA ELEMENTS ÍÍÍÍ"
1120 LOCATE 2,53
1130 IF FLAG2=1 THEN PRINT "  (calculated from data)" ELSE PRINT "(calculated from dimensions)"
1140 LOCATE 3,60 :PRINT "RA=";USING "####.###";RA/KA;:PRINT " ohms"
1150 LOCATE 4,60 :PRINT "LA=";USING "####.###";LA/KA*10^6;:PRINT " uH"
1160 LOCATE 5,60 :PRINT "CA=";USING "####.###";CA*KA*9.999999E+11;:PRINT " pF"
1170 LOCATE 6,60 :PRINT "CB=";USING "####.###";2*CB*9.999999E+11;:PRINT " pF"
1180 LOCATE 7,53 :PRINT "ÍÍÍÍ NETWORK ELEMENTS ÍÍÍÍ"
1190 LOCATE 8,60 :PRINT "CC="USING "####.###" ;CC*KA*9.999999E+11;:PRINT " pF"
1200 FOR I=2 TO NA :N$=MID$(STR$(I),2) :IF I MOD(2)=0 THEN PA=2 ELSE PA=KA
1210 LOCATE 2*I+5,60:PRINT "C";N$;"=";USING "####.###";C(I)*PA*9.999999E+11;:PRINT " pF"
1220 LOCATE 2*I+6,60:PRINT"L";N$;"=";USING"####.###";L(I)/PA*10^6;:PRINT " uH"
1230 NEXT I
1240 'Y=CSRLIN :LOCATE Y+1,60 :PRINT "Z0=";USING "####.##";Z0/KA;:PRINT " ohms
1250 RETURN
1260 '                   menu when using dimensions
1270 LOCATE 15,6 :PRINT "To change values, press letter as shown below:"
1280 PRINT "         Length  Diameter  SWRmax   SWRmin  # Poles"
1290 C$="ÄÄÄÄÄÄ" :PRINT TAB(10);C$;TAB(18);C$;TAB(28);C$;TAB(37);C$;TAB(45);C$
1300 PRINT "Increase  <L>      <D>      <X>      <S>     <N>"
1310 PRINT "Decrease  <l>      <d>      <x>      <s>     <n>"
1320 PRINT "For Zin of ";B$;" without matching, press <a>
1330 PRINT "For Zin of ";B$;" and matching network, press <v>
1340 PRINT "To restart, press <r>.  To quit, press <q> "
1350 COLOR 0,7:LOCATE ,6
1360 PRINT " Press one of the above letters in < > ":COLOR 7,0
1370 A$=INKEY$ :IF A$="" THEN GOTO 1370
1380 IF A$="L" THEN L0=L0*1.009999 :GOSUB 2800
1390 IF A$="l" THEN L0=L0/1.009999 :GOSUB 2830
1400 IF A$="D" THEN D=D*1.009999 :GOSUB 2860
1410 IF A$="d" THEN D=D/1.009999 :GOSUB 2890
1420 IF A$="X" THEN SWRMAX=SWRMAX+5.000000E-02 :GOSUB 2910
1430 IF A$="x" THEN SWRMAX=SWRMAX-5.000000E-02 :GOSUB 2930
1440 IF A$="S" THEN SWRMIN=SWRMIN+5.000000E-02 :GOSUB 2960
1450 IF A$="s" THEN SWRMIN=SWRMIN-5.000000E-02 :GOSUB 2990
1460 IF A$="N" THEN NA=NA+1 :GOSUB 3020
1470 IF A$="n" THEN NA=NA-1 :GOSUB 3050
1480 IF A$="v" THEN GOSUB 1770 :GOSUB 990 :GOTO 1260
1490 IF A$="a" THEN GOSUB 2110 :GOSUB 990 :GOTO 1260
1500 IF A$="r" GOTO 540
1510 IF A$="q" GOTO 2400 ' program end routine
1520 GOSUB 990 :GOTO 1260 'repeats screen for any other entry.
1530 '
1540 '                menu when using data
1550 LOCATE 15,7 :PRINT "To change values, press letter as shown below:"
1560 PRINT "         SWRMAX        SWRMIN       # Poles"
1570 C$="ÄÄÄÄÄÄ":PRINT TAB(10);C$;TAB(23);C$;TAB(38);C$
1580 PRINT "Increase  <X>           <S>          <N>"
1590 PRINT "Decrease  <x>           <s>          <n>"
1600 PRINT "For Zin of the ";B$;" without matching, enter <a>
1610 PRINT "For Zin of the ";B$;" and matching network, enter <v>
1620 PRINT "To restart program, enter <r>.  To end program, enter <q> "
1630 PRINT "Enter letter....";
1640 A$=INKEY$ :IF A$="" THEN GOTO 1640
1650 IF A$="X" THEN SWRMAX=SWRMAX+5.000000E-02 :GOSUB 3110
1660 IF A$="x" THEN SWRMAX=SWRMAX-5.000000E-02 :GOSUB 3130
1670 IF A$="S" THEN SWRMIN=SWRMIN+5.000000E-02 :GOSUB 3160
1680 IF A$="s" THEN SWRMIN=SWRMIN-5.000000E-02 :GOSUB 3190
1690 IF A$="N" THEN NA=NA+1 :GOSUB 3220
1700 IF A$="n" THEN NA=NA-1 :GOSUB 3250
1710 IF A$="v" THEN GOSUB 1770 :GOTO 680
1720 IF A$="a" THEN GOSUB 2110 :GOTO 680
1730 IF A$="r" GOTO 540
1740 IF A$="q" GOTO 2400 ' program end routine
1750 GOTO 680 'repeats screen for any other entry.
1760 '
1770 '.....subroutine to find Zin
1780 CLS :IF FLAG2=1 THEN GOSUB 1080 ELSE GOSUB 990
1790  PRINT "This segment will analyze the matched antenna system                                  from a lower to an upper frequency."
1800 IF FLAG1=1 THEN PRINT :INPUT"Use same frequencies <y/n>? ",A$:IF A$="y"         GOTO 1840
1810 PRINT :INPUT "Enter the lower frequency in MHz ",F1 :IF F1=0 GOTO 1810
1820 INPUT "Enter the upper frequency in MHz ",F2 :IF F2<F1 THEN BEEP :GOTO 1810
1830 FS=(F2-F1)/12  :FLAG1=1
1840 CLS :LOCATE 2,1
1850 PRINT "Xin and VSWR for matched antenna with Z0=";INT(10*Z0/KA)/10;"ohms"
1860 PRINT " Frequency        Rin           Xin           VSWR"
1870 FOR FM=F1 TO F2*1.001000 STEP FS
1880 F=FM*10^6:W=2*PI*F
1890 RP=RA :BP=W*CA-1/W/LA :XP=-1/BP
1900 DN=RP*RP+XP*XP :RS=RP*XP*XP/DN :XS=XP*RP*RP/DN-1/W/CB
1910 RP=RS+XS*XS/RS :BP=-XS/(XS*XS+RS*RS)+W*CC :XP=-1/BP
1920 FOR P=2 TO NA STEP 2
1930 DN=RP*RP+XP*XP :RS=RP*XP*XP/DN :XS=XP*RP*RP/DN-1/W/C(P)+W*L(P)
1940 IF P+1>NA THEN GOTO 1980
1950 RP=RS+XS*XS/RS :BP=-XS/(XS*XS+RS*RS)+W*C(P+1)-1/W/L(P+1) :XP=-1/BP
1960 NEXT P
1970 DN=RP*RP+XP*XP :RS=RP*XP*XP/DN :XS=XP*RP*RP/DN
1980 REFLCO=SQR(((RS-Z0)^2+XS^2)/((RS+Z0)^2+XS^2))
1990 SWR=(1+REFLCO)/(1-REFLCO)
2000 PRINT USING "#####.##      ";F/10^6;RS/KA;XS/KA;SWR
2010 NEXT FM :PRINT
2020 PRINT "To change frequencies, press <f>"
2030 PRINT "To review antenna/network values, press <a>"
2040 PRINT "To end, press <q>."
2050 A$=INKEY$ :IF A$="" GOTO 2050
2060 IF A$="f" GOTO 1810
2070 IF A$="a" THEN RETURN
2080 IF A$="q" THEN RETURN 2400
2090 GOTO 2050
2100 '.....subroutine to find ZA
2110 CLS :IF FLAG2=1 THEN GOSUB 1080 ELSE GOSUB 990 'print display
2120 PRINT "This segment will analyze the antenna equivalent                                circuit from a lower to an upper frequency."
2130 IF FLAG1=1 THEN PRINT :INPUT "Use same frequencies? <y/n) ",A$:IF A$="y"        GOTO 2170
2140 PRINT :INPUT "Enter the lower frequency in MHz ",F1 :IF F1=0 GOTO 2140
2150 INPUT "Enter the upper frequency in MHz ",F2 :IF F2<F1 THEN BEEP :GOTO 2140
2160 FS=(F2-F1)/12  :FLAG1=1
2170 IF ZA=0 THEN INPUT "Enter trial ZA ",ZA :ZO=ZA*KA
2180 'IF ZA=0 THEN ZO=KA
2190 CLS :PRINT :PRINT "Zin and VSWR for antenna equivalent circuit with ZA=";INT(10*ZO/KA)/10;"ohms"
2200 PRINT "Frequency         Rin          Xin          VSWR "
2210 FOR FM=F1 TO F2*1.001000 STEP FS
2220 F=FM*10^6 :W=2*PI*F
2230 RP=RA :BP=W*CA-1/W/LA :XP=-1/BP
2240 DN=RP*RP+XP*XP :RS=RP*XP*XP/DN :XS=XP*RP*RP/DN-1/W/CB
2250 RP=RS+XS*XS/RS :XP=XS+RS*RS/XS
2260 REFLCO=SQR(((RS-ZO)^2+XS^2)/((RS+ZO)^2+XS^2))
2270 SWR=(1+REFLCO)/(1-REFLCO)
2280 PRINT USING "####.##       ";F/10^6;RS/KA;XS/KA;SWR
2290 NEXT FM
2300 PRINT "To change ZA, press <z>"
2310 PRINT "To change frequencies, press <f>"
2320 PRINT "To review antenna/network values, press <a>"
2330 PRINT "To end, press <q>."
2340 A$=INKEY$ :IF A$="" GOTO 2340
2350 IF A$="f" GOTO 2140
2360 IF A$="a" THEN RETURN
2370 IF A$="z" THEN INPUT "Enter new ZA ",ZA :ZO=ZA*KA :GOTO 2180
2380 IF A$="q" THEN RETURN 2400
2390 GOTO 2340
2400 '.....end
2410 GOSUB 4520:GOTO 100
2420 '.....subroutine to print dipole circuit
2430 PRINT "  ÚÄÄÂÄÄÂÄÄCBÄÄ*ÂÄÄC2ÄÄL2Ä* * * C4 * L4 *
2440 PRINT "  ³  ³  ³       ³         *   *
2450 PRINT "  RA LA CA      CC        C3  L3       INPUT
2460 PRINT "  ³  ³  ³       ³         *   *
2470 PRINT "  ÀÄÄÁÄÄÁÄÄCBÄÄ*ÁÄÄC2ÄÄL2Ä* * * C4 * L4 *
2480 PRINT "  ³®ÄÄÄdipoleÄÄ¯³®ÄÄÄmatching networkÄÄÄ¯³" :RETURN
2490 '
2500 '.....subroutine to print monopole circuit
2510 PRINT "  ÚÄÄÂÄÄÂÄÄCBÄ*ÂÄC2ÄÄL2Ä* * * * C4 * L4 *
2520 PRINT "  ³  ³  ³      ³        *   *
2530 PRINT "  RA LA CA     CC       C3  L3        INPUT
2540 PRINT "  ³  ³  ³      ³        *   *
2550 PRINT "  ÀÄÄÁÄÄÁÄÄÄÄÄ*ÁÄÄÄÄÄÄÄÄ* * * * * * * * *
2560 PRINT "  ³®Ämonopole¯³®ÄÄÄmatching networkÄÄÄÄ¯´" :RETURN
2570 '.....subroutine to calculate G(n)'s
2580 SX=SWRMAX :SM=SWRMIN :IF SX=1 THEN SX=1.001000
2590 IF (SX-SM)=0 THEN SX=SX*1.001000
2600 PMAX=(SX-1)/(SX+1) :ILMAX=1-PMAX^2
2610 PMIN=(SM-1)/(SM+1) :ILMIN=1-PMIN^2
2620 E1=SQR(ILMIN-ILMAX)
2630 U1=SQR((1-PMAX*PMAX)/(PMAX*PMAX-PMIN*PMIN))
2640 U2=PMIN*U1
2650 NA1=LOG(U1+SQR(U1*U1+1))
2660 NB1=LOG(U2+SQR(U2*U2+1))
2670 A=NA1/NA
2680 B=NB1/NA
2690 HSA=5.000000E-01*(EXP(A)-EXP(-A))
2700 HSB=5.000000E-01*(EXP(B)-EXP(-B))
2710 G(1)=2*SIN(PI/2/NA)/(HSA-HSB)
2720 FOR I=1 TO NA-1
2730 U3=4*SIN((2*I-1)*PI/2/NA)*SIN((2*I+1)*PI/2/NA)
2740 U4=HSA*HSA+HSB*HSB+SIN(I*PI/NA)^2-2*HSA*HSB*COS(I*PI/NA)
2750 G(I+1)=U3/U4/G(I)
2760 NEXT I
2770 GIN=2*SIN(PI/2/NA)/(HSA+HSB)/G(NA)
2780 RETURN
2790 '.....logic subroutines for dimensions
2800 '.....subroutine to check L
2810 IF L0>LMAX THEN BEEP :L0=L0/1.009999 :GOTO 3090
2820 GOSUB 730 :IF FLAG=1 THEN BEEP :L0=L0/1.009999 :GOTO 3080 ELSE GOTO 3090
2830 '.....subroutine to check l
2840 IF KA*L0/D<50 THEN BEEP :L0=L0*1.009999 :GOTO 3090
2850 GOSUB 730 :IF FLAG=1 THEN BEEP :L0=L0*1.009999 :GOTO 3080 ELSE GOTO 3090
2860 '.....subroutine to check D
2870 IF L/D<50 THEN BEEP :D=D/1.009999 :GOTO 3090
2880 GOSUB 730 :IF FLAG=1 THEN BEEP :D=D/1.009999 :GOTO 3080 ELSE GOTO 3090
2890 '.....subroutine to check d
2900 GOSUB 730 :IF FLAG=1 THEN BEEP :D=D*1.009999 :GOTO 3080 ELSE GOTO 3090
2910 '.....subroutine to check X
2920 GOSUB 2570 :GOSUB 730:IF FLAG=1 THEN BEEP:SWRMAX=SWRMAX-5.000000E-02 :GOSUB 2570 :       GOTO 3080 ELSE GOTO 3090
2930 '.....subroutine to check x
2940 IF SWRMAX< SWRMIN*9.990000E-01 THEN BEEP :SWRMAX=SWRMAX+5.000000E-02 :GOTO 3090
2950 GOSUB 2570 :GOSUB 730:IF FLAG=1 THEN BEEP:SWRMAX=SWRMAX+5.000000E-02 :GOSUB 2570 :       GOTO 3080 ELSE GOTO 3090
2960 '.....subroutine to check S
2970 IF SWRMIN>SWRMAX*1.001000 THEN BEEP :SWRMIN=SWRMIN-5.000000E-02 :GOTO 3090
2980 GOSUB 2570 :GOSUB 730:IF FLAG=1 THEN BEEP:SWRMIN=SWRMIN-5.000000E-02 :GOSUB 2570 :       GOTO 3080 ELSE GOTO 3090
2990 '......subroutine to check s
3000 IF SWRMIN<9.990000E-01 THEN BEEP :SWRMIN=SWRMIN+5.000000E-02 :GOTO 3090
3010 GOSUB 2570 :GOSUB 730:IF FLAG=1 THEN BEEP:SWRMIN=SWRMIN+5.000000E-02 :GOSUB 2570 :       GOTO 3080 ELSE GOTO 3090
3020 '          subroutine to check N
3030 IF NA>6 THEN BEEP :NA=NA-1 :GOTO 3090
3040 GOSUB 2570 :GOSUB 730:IF FLAG=1 THEN BEEP: NA=NA-1:GOSUB 2570 :GOTO 3080       ELSE GOTO 3090
3050 '.....subroutine to check n
3060 IF NA<2 THEN BEEP :NA=NA+1 :GOTO 3090
3070 GOSUB 2570 :GOSUB 730:IF FLAG=1 THEN BEEP: NA=NA+1 :GOSUB 2570 :GOTO 3080      ELSE GOTO 3090
3080 GOSUB 730
3090 CLS :GOSUB 990 :RETURN 1260
3100 '.....logic subroutines for data.......................
3110 '.....subroutine to check X
3120 GOSUB 2570 :GOSUB 800:IF FLAG=1 THEN BEEP:SWRMAX=SWRMAX-5.000000E-02 :GOTO 3280 ELSE     GOTO 3290
3130 '.....subroutine to check x
3140 IF SWRMAX< SWRMIN*9.990000E-01 THEN BEEP :SWRMAX=SWRMAX+5.000000E-02 :GOTO 3290
3150 GOSUB 2570 :GOSUB 800:IF FLAG=1 THEN BEEP:SWRMAX=SWRMAX+5.000000E-02 :GOTO 3290 ELSE     GOTO 3290
3160 '.....subroutine to check S
3170 IF SWRMIN>SWRMAX*1.001000 THEN BEEP :SWRMIN=SWRMIN-5.000000E-02 :GOTO 3290
3180 GOSUB 2570 :GOSUB 800:IF FLAG=1 THEN BEEP:SWRMIN=SWRMIN-5.000000E-02 :GOTO 3280 ELSE     GOTO 3290
3190 '.....subroutine to check s
3200 IF SWRMIN<9.990000E-01 THEN BEEP :SWRMIN=SWRMIN+5.000000E-02 :GOTO 3290
3210 GOSUB 2570 :GOSUB 800:IF FLAG=1 THEN BEEP:SWRMIN=SWRMIN*1.009999:GOTO 3280 ELSE     GOTO 3290
3220 '.....subroutine to check N
3230 IF NA>6 THEN BEEP :NA=NA-1 :GOTO 3290
3240 GOSUB 2570 :GOSUB 800:IF FLAG=1 THEN BEEP: NA=NA-1:GOTO 3280 ELSE GOTO 3290
3250 '.....subroutine to check n
3260 IF NA<2 THEN BEEP :NA=NA+1 :GOTO 3290
3270 GOSUB 2570:GOSUB 800:IF FLAG=1 THEN BEEP: NA=NA+1 :GOTO 3280 ELSE GOTO 3290
3280 GOSUB 2570 :GOSUB 800
3290 CLS :GOSUB 1080 :RETURN 1540
3300 '
3310 '.....subroutine to get data and calculate RA,LA,CA,and CB
3320 CLS :KEY OFF :N=4 :TP=2*3.141597199999999
3330 'DIM A(8,9), B(8,9), X(8), NPIVROW(8,2),NPIVCOL(8,2)
3340 CLS :PRINT ':GOSUB 3250 ' unasterisk to load preloaded data
3350 PRINT "The program requires 4 sets of <Frequency, Rin, Xin> data. Associate
3360 PRINT "each set of data with a number from 1 to 4. Enter the data by first
3370 PRINT "entering the data point number, then entering the F, Rin, and Xin"
3380 PRINT "values. Data will automatically be ordered by frequency, so it can "
3390 PRINT "be corrected. Change data by entering the data number, then entering
3400 PRINT "the correct data. When all the data is correct, press <CR>
3410 PRINT
3420 FOR P=9 TO 17:LOCATE P,1:PRINT "                                   ":NEXT P
3430 LOCATE 9,1 :INPUT "Enter Data Number ";A$
3440 IF A$<"1" OR A$>"4" THEN BEEP :GOTO 3420 ELSE N=VAL(A$)
3450 INPUT "Enter Frequency (MHz)...";F(N)
3460 INPUT "Enter R (ohms)..........";R(N)
3470 INPUT "Enter X (ohms)..........";X1(N) :PRINT
3480 LOCATE 18,1 :FOR N=1 TO 4 :PRINT N USING "#####.###  ";F(N);R(N);X1(N)          :NEXT N
3490 FOR N=1 TO 4 :IF F(N)=0 THEN GOTO 3420
3495 NEXT N
3500 '.....sort frequency data
3510 FOR N=1 TO 3
3520 FOR M=1 TO 3
3530 IF F(M)>F(M+1) THEN SWAP F(M),F(M+1) :SWAP R(M),R(M+1) :SWAP X1(M),X1(M+1)
3540 NEXT M :NEXT N :LOCATE 13,1
3550 LOCATE 18,1
3560 LOCATE 18,1 :FOR N=1 TO 4 :PRINT N USING "#####.###  ";F(N);R(N);X1(N)          :NEXT N
3570 PRINT :INPUT "Is data is OK? <y/n>. ", A$
3580 LOCATE 23,1 :PRINT "                                     "
3590 IF A$<>"y" GOTO 3420
3600 '                 load matrix values
3610 FOR N=1 TO 4
3620 W(N)=F(N)*TP :GA(N)=R(N)/(R(N)^2+X1(N)^2) :B1(N)=-X1(N)/(R(N)^2+X1(N)^2)
3630 B(N,1)=-W(N)^3 :B(N,2)=W(N) :B(N,3)=W(N)^2*B1(N) :B(N,4)=-W(N)*GA(N)             :B(N,5)=B1(N)
3640 NEXT N :N=4
3650 '.....solve matrix
3660 NC=N+1 :EPS=9.999999E-13 :DET=1
3670 FOR K = 1 TO N
3680 FOR J = 1 TO NC
3690 A(K,J)=B(K,J)
3700 NEXT J : NEXT K
3710 FOR K = 1 TO N
3720 '      Apply complete pivoting strategy
3730 MAXPIVOT = ABS(A(K,K))
3740 NPIVROW(K,1)=K: NPIVROW(K,2)=K
3750 NPIVCOL(K,1)=K: NPIVCOL(K,2)=K
3760 FOR I = K TO N
3770 FOR J = K TO N
3780 IF MAXPIVOT >= ABS(A(I,J)) GOTO 3820
3790 MAXPIVOT=ABS(A(I,J))
3800 NPIVROW(K,1)=K: NPIVROW(K,2)=I
3810 NPIVCOL(K,1)=K: NPIVCOL(K,2)=J
3820 NEXT J:NEXT I
3830 IF MAXPIVOT >= EPS GOTO 3850
3840 GOTO 4210
3850 IF NPIVROW(K,2)=K GOTO 3900
3860 FOR J = K TO NC
3870 SWAP  A(NPIVROW(K,2),J),A(K,J)
3880 NEXT J
3890 DET=DET*(-1)
3900 IF NPIVCOL(K,2)=K GOTO 3950
3910 FOR I = 1 TO N
3920 SWAP  A(I,NPIVCOL(K,2)),A(I,K)
3930 NEXT I
3940 DET=DET*(-1)
3950 IF K=N THEN GOTO 4020
3960 FOR I = K+1 TO N
3970 MULT = - A(I,K)/A(K,K)
3980 FOR J = NC TO K STEP -1
3990 A(I,J)=A(I,J)+MULT*A(K,J)
4000 NEXT J
4010 NEXT I
4020 NEXT K
4030 '.....Apply the back-substitution formulas
4040 RANK=K-1 :NMR=N-RANK
4050 IF RANK=N THEN X(N) = A(N,N+1) / A(N,N) :NCOUNT=N-1: GOTO 4090
4060 IF ABS(A(N,N+1)) > EPS THEN GOTO 4210
4070 FOR JJ=1 TO NMR : X(N+1-JJ) = 1: NEXT JJ
4080 NCOUNT=RANK
4090 FOR I = NCOUNT TO 1 STEP -1
4100 SUM = 0
4110 FOR J = I+1 TO N
4120 SUM = SUM + A(I,J) * X(J)
4130 NEXT J
4140 X(I) = (A(I,N+1) - SUM) / A(I,I)
4150 NEXT I
4160 '.....interchange order of unknowns to correct for column pivoting
4170 FOR K=N TO 1 STEP -1
4180 SWAP X(NPIVCOL(K,2)), X(NPIVCOL(K,1))
4190 NEXT K :GOSUB 4240 :RETURN '  solve for RA,LA,CA,CB and return
4200 '
4210 PRINT "Either there's an error in the data or this antenna cannot be "
4220 PRINT "analyzed by this program. " :END
4230 '-------------------------------------------------------------------
4240 '.....subroutine to get RA ,LA ,CA, and CB values
4250 AA=X(1) :CB=X(2) :DD=X(3): EE=X(4)
4260 'AA=CA*LA*GA, DD=LA*(CA+CB), EE=LA*GA
4270 CA=AA/DD*CB/(CB-AA/DD) :LA=DD/(CA+CB) :GA=EE/LA :RA=KA/GA
4280 CA=CA*9.999999E-07/KA : LA=KA*LA*9.999999E-07 :CB=CB*9.999999E-07/KA
4290 IF CA<0 OR CB<0 OR RA<0 OR LA<0 THEN GOTO 4210
4300 RETURN
4310 '----------------------------------------------------------------------
4320 '.....subroutine to preload data (to activate, add "GOTO xxxx" after intro)
4330 F(1)=73 :R(1)=3.191620E+01 :X1(1)=2.889199
4340 F(2)=7.725000E+01 :R(2)=3.741090E+01 :X1(2)=2.237910E+01
4350 F(3)=8.433329E+01 :R(3)=4.847579E+01 :X1(3)=5.434519E+01
4360 F(4)=90 :R(4)=5.945080E+01 :X1(4)=7.984970E+01
4370 RETURN 3600
4380 IF FLAG=1 THEN CLS :BEEP :INPUT "Reqires negative elements. This program not useful for this antenna. Press <CR>.",A$ :CLS :END
4390 '
4400 '                         Program Notes
4410 'After the intro, the program loads starting values of SWR & L, then calcs
4420 'G(N)'s. It then goes into either a data or dimension mode. If data, it
4430 'goes to a SR to load data and calc RA..CB then returns to calc L's & C's.
4440 'It then prints the data screen and goes to the data menu. If dimension
4450 'was selected, the program asks for D, then calc RA...C(n), prints the
4460 'dimension screen, then goes to the dimension menu. The menus are similar.
4470 'They allow parameters to be "tuned" within limits, and provide ways to
4480 'calc Zin or ZA. Because of all the subroutines, a CLEAR statement had to
4490 'be used to increase stack space to avoid an "OUT OF MEMORY" error.
4500 'Extensive "tuning" might still cause the error.
4510 '
4520 '.....PRT
4530 KEY OFF:GOSUB 4600:LOCATE 25,5:COLOR 0,2
4540 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
4550 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
4560 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 4560 ELSE GOSUB 4600
4570 IF Z$="3"THEN RETURN
4580 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
4590 IF Z$="2"THEN LPRINT CHR$(12) ELSE 4530
4600 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'TFORM3 - EQIVALENT L/C NETWORKS - 17 JUN 2010
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 '.....title page
70 COLOR 15,2
80 PRINT " L/C NETWORKS with NO TRANSFORMERS";TAB(61);"by R.J.Dehoney IEEE ";
90 COLOR 7,0
100 '.....start
110 PRINT
120 PRINT "We often need to change impedance levels when dealing with filters or with";
130 PRINT "matching or interstage networks. A handy trick which sometimes lets you avoid";
140 PRINT "winding an actual transformer is to take advantage of a circuit equivalence.";
150 PRINT "It turns out that a tee or pi network of C's or L's is exactly the same as an";
160 PRINT "L network followed by an ideal transformer. Figure 1 shows an example.";
170 GOSUB 180:GOTO 250
180 '.....Figure 1
190 PRINT "              N:1";
200 PRINT "      *ддCsдбд)(дд*д©         *ддCaдбдCcдд*дд©";
210 PRINT "    Zin1   Cp )(    ZL      Zin2    Cb       ZL";
220 PRINT "      *дддддад)(дд*ды         *дддддаддддд*дды";
230 PRINT "              T1    Figure 1";
240 RETURN
250 PRINT
260 PRINT "With proper values for the capacitors, Zin1 will equal Zin2 for all values";
270 PRINT "of ZL. There are some restrictions, of course. In this configuration, to avoid";
280 PRINT "negative values for Ca or Cc, N must be less than 1 and Cp must be greater than";
290 PRINT "Cs*(1-N)/N. If Cp is on the other side of Cs, then N must be greater than 1";
300 PRINT "and Cp must be greater than Cs*(N-1). There are similar restrictions on the pi";
310 PRINT "networks and the networks using inductors.";
320 PRINT :PRINT "Press <1> to run program or <0> to EXIT"
330 A$=INKEY$ :IF A$="" THEN 330
340 IF A$="1"THEN CLS:GOTO 370
350 IF A$="0"THEN CLS:RUN GO$
360 GOTO 330
370 PRINT "This program is intended to let you bypass all these complications. To use it,";
380 PRINT "you first enter your present impedance level, then your desired impedance";
390 PRINT "level. The program then shows you both L and C configurations. You should now";
400 PRINT "redraw your circuit to identify elements corresponding to Cs and Cp or Ls and";
410 PRINT "Lp. You are then asked to choose a configuration and enter the element values";
420 PRINT "corresponding to Cs and Cp or Ls and Lp. The program then calculates the";
430 PRINT "values of the tee and pi equivalent networks and, if all are positive, displays";
440 PRINT "them. You then choose which set of values is most realizable and insert the";
450 PRINT "tee or pi network in place of the elements you identified as Cs and Cp, or Ls";
460 PRINT "and Lp. All inductances from the circuit input to the tee or pi must be";
470 PRINT "multiplied by N^2 and all capacitors divided by N^2.";
480 PRINT :PRINT "If you find this explanation convoluted, the examples should help make
490 PRINT "everything clear.";
500 PRINT :PRINT "Press <e> to see examples or <c> to continue.
510 A$=INKEY$ :IF A$="" THEN 510 ELSE CLS
520 IF A$="e" THEN 1210
530 IF A$="c"THEN GOSUB 180:GOTO 550
540 GOTO 520
550 INPUT "Enter present Z0 ",Z01
560 INPUT "Enter desired Z0 ",Z02 :N=SQR(Z01/Z02)
570 INPUT "Enter Cs, pF ",CS
580 IF N<1 THEN PRINT "Enter Cp, greater than";CS*(1-N)/N;"pF";:INPUT CP
590 IF N>1 THEN PRINT "Enter Cp, greater than";CS*(N-1);"pF";:INPUT CP
600 INPUT "Enter Ls, ФH ",LS
610 IF N<1 THEN PRINT "Enter Lp, less than";LS*N/(1-N);"uH";:INPUT LP
620 IF N>1 THEN PRINT "Enter Lp, less than";LS/(N-1);"ФH";:INPUT LP
630 IF N>1 THEN 980
640 CA=CP*CS/(CP-CS*(1-N)/N) :IF CA<0 THEN FLAGC=1
650 CB=CP*N :CC=CP*N^2/(1-N)
660 CX=CS*(1-N) :CY=CS*N :CZ=N^2*CP-CS*N*(1-N)
670 LA=LS-LP*(1-N)/N :IF LA<0 THEN FLAGL=1
680 LB=LP/N :LC=LP*(1-N)/N^2
690 LX=LS/(1-N) :LY=LS/N :LZ=LS*LP/(N^2*LS-N*LP*(1-N))
700 IF FLAGC THEN 870
710 CLS
720 PRINT " EQUIVALENT L/C NETWORKS";
730 PRINT " ммммммммммммммммммммммм";
740 GOSUB 180
750 PRINT " Present Z0=";Z01;"Й"
760 PRINT " Desired Z0=";Z02;"Й"
770 PRINT " Cp=";CP;"pF";
780 PRINT ";
790 CA=CINT(CA*10)/10:CB=CINT(CB*10)/10:CC=CINT(CC*10)/10
800 CX=CINT(CX*10)/10:CY=CINT(CY*10)/10:CZ=CINT(CZ*10)/10
810 LA=CINT(LA*10)/10:LB=CINT(LB*10)/10:LC=CINT(LC*10)/10
820 LX=CINT(LX*10)/10:LY=CINT(LY*10)/10:LZ=CINT(LZ*10)/10
830 PRINT "       *ддCaдбдCcдд* Ca=";CA;TAB(42);;"*дбддCyддбд* Cx=";CX
840 PRINT "             Cb      Cb=";CB;TAB(42);;" Cx     Cz   Cy=";CY
850 PRINT "       *дддддаддддд* Cc=";CC;TAB(42);;"*даддддддад* Cz=";CZ
860 PRINT "                     Capacitors only (in pF)";
870 PRINT :IF FLAGL*FLAGC THEN 950 ELSE IF FLAGL THEN 950
880 PRINT ";
890 PRINT "       *ддLaдбдLcдд* La=";LA;TAB(42);;"*дбддLyддбд* Lx=";LX
900 PRINT "             Lb      Lb=";LB;TAB(42);;" Lx     Lz   Ly=";LY
910 PRINT "       *дддддаддддд* Lc=";LC;TAB(42);;"*даддддддад* Lz=";LZ
920 PRINT "                     Inductors only (in ФH)";
930 GOSUB 2690
940 RUN 'resets Flags to zero
950 PRINT "Cannot use this technique with these values. N too small.";
960 GOSUB 2690:CLS:RUN
970 RUN 'resets Flags to zero
980 CC=CP*CS*N^2/(CP-CS*(N-1)) :IF CC<0 THEN FLAGC=1
990 CB=CP*N :CA=CP*N/(N-1)
1000 CZ=CS*N*(N-1) :CY=CS*N :CX=CP-CS*(N-1)
1010 LC=(LS-LP*(N-1))/N^2 :IF LC<0 THEN FLAGL=1
1020 LB=LP/N :LA=LP*(N-1)/N
1030 LZ=LS/N/(N-1) :LY=LS/N :LX=LS*LP/(LS-LP*(N-1))
1040 IF FLAGC THEN 1090
1050 PRINT ";
1060 PRINT "       *ддCaдбдCcдд* Ca=";CA;TAB(42);;"*дбддCyддбд* Cx=";CX
1070 PRINT "             Cb      Cb=";CB;TAB(42);;" Cx     Cz   Cy=";CY
1080 PRINT "       *дддддаддддд* Cc=";CC;TAB(42);;"*даддддддад* Cz=";CZ
1090 PRINT :IF FLAGL*FLAGC THEN 1170 ELSE IF FLAGL THEN 1140
1100 PRINT ";
1110 PRINT "       *ддLaдбдLcдд* La=";LA;TAB(42);;"*дбддLyддбд* Lx=";LX
1120 PRINT "             Lb      Lb=";LB;TAB(42);;" Lx     Lz   Ly=";LY
1130 PRINT "       *дддддаддддд* Lc=";LC;TAB(42);;"*даддддддад* Lz=";LZ
1140 PRINT :PRINT "Press any key to continue
1150 A$=INKEY$ :IF A$="" THEN 1150 ELSE CLS
1160 RUN 'rests Flags to zero
1170 PRINT "Cannot use this technique with these values. N too small.";
1180 PRINT :PRINT "Press any key to continue
1190 A$=INKEY$ :IF A$="" THEN 1190 ELSE CLS
1200 RUN 'rests Flags to zero
1210 PRINT "               ---------------Example 1-----------------";
1220 PRINT "We have a design for a 5O ohm bandpass filter we wish to drive from a 75 ohm";
1230 PRINT "source. The filter is a .1 dB ripple Chebyshev with the following schematic:";
1240 PRINT "                                   C1=C3=3300 pF";
1250 PRINT "       зддбдддбдC2ддL2ддбдддбддд*  L1=L3=639.5 nH";
1260 PRINT "      50  C1  L1        C3  L3     C2=231 pF";
1270 PRINT "       юддадддадддддддддадддаддд*  L2=9.14 uH";
1280 PRINT ";
1290 PRINT "We need a step up transformation, so our configuration is:";
1300 PRINT "                       N:1                 N:1";
1310 PRINT "             *ддCsддбдд)(дд*     *ддLsддбдд))дд*  where N=sqr(50/75)";
1320 PRINT "                   Cp  )(     or       Lp  )(";
1330 PRINT "             *ддддддадд)(дд*     *ддддддадд)(дд*";
1340 PRINT ";
1350 PRINT "We redraw our filter schematic to identify elements corresponding to Cs and Cp,";
1360 PRINT "and Ls and Lp, and see that C2 and C3 correspond to Cs and Cp, and L2 and L3";
1370 PRINT "correspond to Ls and Lp.";
1380 PRINT "       зддбддбдL2ддC2дбдддбддд*      зддбддбдC2ддL2дбдддбдд*";
1390 PRINT "      50  C1 L1       C3  L3    or  50  C1 L1       L3  C3";
1400 PRINT "       юддаддаддддддддадддаддд*      юддаддаддддддддадддадд*";
1410 PRINT ";
1420 PRINT "Press <c> to continue"; :PRINT
1430 A$=INKEY$  :IF A$="" THEN 1430 ELSE CLS
1440 PRINT "We imagine we have an ideal 50:75 ohm transformer connected to our filter.";
1450 PRINT ";
1460 PRINT "   зддбддбдL2ддC2дбдддбдд)(д*         зддбддбдC2ддL2дбдддбдд)(д*";
1470 PRINT "  50  C1 L1       C3  L3 )(      or  50  C1 L1       L3  C3 )(";
1480 PRINT "   юддаддаддддддддадддадд)(д*         юддаддаддддддддадддадд)(д*";
1490 PRINT ";
1500 PRINT " We move the nearest element across the transformer:";
1510 PRINT ";
1520 PRINT "   зддбддбдL2ддC2дбдд)(ддбдд*         зддбддбдC2ддL2дбдд)(ддбдд*";
1530 PRINT "  50  C1 L1       C3 )(  L3a     or  50  C1 L1       L3 )(  C3a";
1540 PRINT "   юддаддаддддддддадд)(ддадд*         юддаддаддддддддадд)(ддадд*";
1550 PRINT " where L3a=L3/(50/75) and C3a=C3*(50/75).";
1560 PRINT ";
1570 PRINT "We can now see if C2, C3, and the transformer, or L2, L3, and the transformer";
1580 PRINT "can be replaced with a tee or pi network with real values. When the program";
1590 PRINT "asks for Cs, we enter 231, then, for Cp, we enter 3300 and so on. The";
1600 PRINT "program prints out the schematics and values for both the capacitor and";
1610 PRINT "inductor version of the equivalent networks.";
1620 PRINT :PRINT "Press <c> to continue or <b> to see previous screen" :PRINT
1630 A$=INKEY$  :IF A$="" THEN 1630 ELSE CLS
1640 IF A$="b" THEN 1210
1650 PRINT "This is what we see:";
1660 CA=2.346922099999999E+02 :CB=2.694438E+03 :CC=1.198887E+04 :CX=4.238927E+01 :CY=1.886107E+02
1670 CZ=2.165388E+03 :LA=8.996275 :LB=7.832244E-01 :LC=1.760255E-01 :LX=4.980830E+01
1680 LY=1.119416E+01 :LZ=9.745174900000000E-01
1690 PRINT "       *ддCaдбдCcдд* Ca=";CA;TAB(42);;"*дбддCyддбд* Cx=";CX
1700 PRINT "             Cb      Cb=";CB;TAB(42);;" Cx     Cz   Cy=";CY
1710 PRINT "       *дддддаддддд* Cc=";CC;TAB(42);;"*даддддддад* Cz=";CZ
1720 PRINT ";
1730 PRINT "       *ддLaдбдLcдд* La=";LA;TAB(42);;"*дбддLyддбд* Lx=";LX
1740 PRINT "             Lb      Lb=";LB;TAB(42);;" Lx     Lz   Ly=";LY
1750 PRINT "       *дддддаддддд* Lc=";LC;TAB(42);;"*даддддддад* Lz=";LZ
1760 PRINT
1770 PRINT "The pi network values seem reasonable, so we decide on that circuit. This is";
1780 PRINT "our final filter schematic:";
1790 PRINT ";
1800 PRINT "       зддбдддбдL2дбдCYдбдддбддд*  C1=3300 pF    L1=639.5 nH";
1810 PRINT "      50  C1  L1   CX   CZ  L3a    L2=9.14 uH    CX=42.389 pF";
1820 PRINT "       юддадддаддддаддддадддаддд*  CY=188.61 uH  CZ=2165.39 pF";
1830 PRINT "                                   L3a=0.97452 uH";
1840 PRINT
1850 PRINT "Using a ladder analysis program, we compare the response of our new filter,";
1860 PRINT "using a 75 ohm source to the response of the original filter using a 50 ohm";
1870 PRINT "source, and find the responses to be the same.";
1880 PRINT :PRINT "Press <c> to continue or <b> to see previous screen" :PRINT
1890 A$=INKEY$  :IF A$="" THEN 1890 ELSE CLS
1900 IF A$="b" THEN 1440
1910 PRINT "Out of curiosity, we decide to see if our 50 ohm filter can be converted into";
1920 PRINT "into a 500 ohm filter. We run the program and enter 50 as our present Z0 and";
1930 PRINT "500 as our desired Z0. We enter our C and L values as before and get:";
1940 '
1950 CA=2.722000E+02 :CB=1.043550E+03 :CC=4.826170E+02 :CX=1.579514E+02 :CY=7.304685E+01
1960 CZ=2.800513E+02 :LA=7.775199 :LB=2.022277 :LC=4.372724 :LX=1.336703E+01
1970 LY=2.890321E+01 :LZ=7.534952
1980 PRINT "       *ддCaдбдCcдд* Ca=";CA;TAB(42);;"*дбддCyддбд* Cx=";CX
1990 PRINT "             Cb      Cb=";CB;TAB(42);;" Cx     Cz   Cy=";CY
2000 PRINT "       *дддддаддддд* Cc=";CC;TAB(42);;"*даддддддад* Cz=";CZ
2010 PRINT ";
2020 PRINT "       *ддLaдбдLcдд* La=";LA;TAB(42);;"*дбддLyддбд* Lx=";LX
2030 PRINT "             Lb      Lb=";LB;TAB(42);;" Lx     Lz   Ly=";LY
2040 PRINT "       *дддддаддддд* Lc=";LC;TAB(42);;"*даддддддад* Lz=";LZ
2050 PRINT
2060 PRINT "We decide to try the tee network of inductors which gives us this schematic:";
2070 PRINT ";
2080 PRINT "       зддбдддбдC2ддLXдбдLZдбддд*  C1=3300 pF     L1=639.5 nH";
2090 PRINT "      50  C1  L1       LY   C3a    C2=231 pF      LA=7.7752 uH";
2100 PRINT "       юддадддаддддддддаддддаддд*  LB=2.02228 uH  LC=4.37272 uH";
2110 PRINT "                                   C3a=330 pF";
2120 PRINT
2130 PRINT "To our delight, the response when driven from a 500 ohm source is identical";
2140 PRINT "to the response of our original 50 ohm filter.";
2150 PRINT :PRINT "Press <e> to end or <b> to see previous screen" :PRINT
2160 A$=INKEY$  :IF A$="" THEN 2160
2170 IF A$="b" THEN 1650
2180 IF A$="e"THEN CLS:RUN
2190 GOTO 2160
2200 PRINT "We decide to try an example with N>1. We use our same filter, but this time";
2210 PRINT "we want it to have a 12.5 ohm input Z0. We draw the schematic, adding an ideal";
2220 PRINT "50:12.5 ohm transformer at the input. We see that C1 and C2, and L1 and L2 are";
2230 PRINT "candidates for a step down equivlent circuit.";
2240 PRINT "                        2:1                                2:1";
2250 PRINT "   зддбддбдC2ддL2дбдддбдд)(д*         зддбддбдL2ддC2дбдддбдд)(д*";
2260 PRINT "  50  L1 C1       C3  L3 )(      or  50  C1 L1       L3  C3 )(";
2270 PRINT "   юддаддаддддддддадддадд)(д*         юддаддаддддддддадддадд)(д*";
2280 PRINT ";
2290 PRINT "We move the transformer to its required location, changing the values of the";
2300 PRINT "intervening elements.";
2310 PRINT "              2:1                              2:1";
2320 PRINT "   зддб*дбдC2д)(д*дL2aдбдддбддд*     зддб*дбдL2д)(д*дC2aдбдддбд*";
2330 PRINT "   50 L1 C1   )(       C3a L3a   or  50 C1 L1   )(       L3a C3a";
2340 PRINT "   юдда*дадддд)(д*дддддадддаддд*     юдда*дадддд)(д*дддддадддад*";
2350 PRINT "where L2a=L2/4, C3a=C3*4, L3a=L3/4 and so on."; :PRINT
2360 PRINT "We run the program, entering 50 for our present Z0, and 12.5 for our desired";
2370 PRINT "Z0. We enter 231 for Cs, 3300 for Cp, 9.14 for Ls, and .6395 for Lp.";
2380 PRINT :PRINT "Press <c> to continue, or <b> to see previous screen
2390 A$=INKEY$ :IF A$="" THEN 2390 ELSE CLS
2400 IF A$="b" THEN 1910
2410 CA=6600  :CB=6600 :CC=9.935479E+02 :CX=3069 :CY=462
2420 CZ=462 :LA=3.197520E-01 :LB=3.197500E-01 :LC=2.125124 :LX=6.876100E-01
2430 LY=4.750000 :LZ=4.750000
2440 PRINT "We get these values.";
2450 PRINT ";
2460 PRINT "       *ддCaдбдCcдд* Ca=";CA;TAB(42);;"*дбддCyддбд* Cx=";CX
2470 PRINT "             Cb      Cb=";CB;TAB(42);;" Cx     Cz   Cy=";CY
2480 PRINT "       *дддддаддддд* Cc=";CC;TAB(42);;"*даддддддад* Cz=";CZ
2490 PRINT ";
2500 PRINT "       *ддLaдбдLcдд* La=";LA;TAB(42);;"*дбддLyддбд* Lx=";LX
2510 PRINT "             Lb      Lb=";LB;TAB(42);;" Lx     Lz   Ly=";LY
2520 PRINT "       *дддддаддддд* Lc=";LC;TAB(42);;"*даддддддад* Lz=";LZ
2530 PRINT
2540 PRINT "The values for the capacitor pi network seem reasonable, so our final filter";
2550 PRINT "looks like this:";
2560 PRINT ";
2570 PRINT "          зддб*дбдCYдбд*дL2aдбдддбддд* L1=.6395 uH   CX=3069 pF";
2580 PRINT "         50 L1 CX   CZ      C3a L3a    CY=462 pF     CZ=462 pF";
2590 PRINT "          юдда*даддддад*дддддадддаддд* L2A=2.285 uH  C3a=13200 pF";
2600 PRINT "                                       L3a=.15987 uH Zin=12.5 ohms";
2610 PRINT
2620 PRINT "We find the response using a 12.5 ohm source is identical to the response with";
2630 PRINT "our original 50 ohm filter.";
2640 'PRINT :PRINT "Press <c> to continue or <b> to return to previous screen
2650 A$=INKEY$ :IF A$="" THEN 2650 ELSE CLS
2660 IF A$="b" THEN 2200 ELSE RUN 'resets Flag values to zero
2670 '.....end
2680 GOSUB 2690:CLS:GOTO 100
2690 '.....PRT
2700 KEY OFF:GOSUB 2770:LOCATE 25,5:COLOR 0,2
2710 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2720 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2730 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2730 ELSE GOSUB 2770
2740 IF Z$="3"THEN RETURN
2750 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2760 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2700
2770 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'CPLRES  Capacitor Coupled Resonators  21 NOV 09
15 DIM C$(60, 9)
20 CLS: KEY OFF
30 IF EX$ = "" THEN EX$ = "EXIT"
40 IF PROG$ = "" THEN GO$ = EX$ ELSE GO$ = PROG$
50 PI = 4 * ATN(1)
60 '.....title page
70 COLOR 15, 2
80 PRINT " CAPACITY COUPLED RESONATORS"; TAB(57); "by George Murphy VE3ERP ";
90 LOCATE , 24: PRINT " Algorithm by R.J.Dehoney IEEE "
100 COLOR 7, 0
110 PRINT
120 PRINT " This program is derived from CPLRES, a BASIC version of CAPCPL, a";
130 PRINT " C++ program"
140 PRINT " for designing bandpass filters using LC resonators. This program ";
150 PRINT "uses short  "
160 PRINT " lengths of shorted tranmission line to replace the LC resonators.";
170 PRINT " Other      "
180 PRINT " capacitors can be made from short lengths of unshorted cable."
190 PRINT " (Ref. ARRL Handbook 2007, pp. 12.18 - 12.21)"
200 PRINT
210 LL = CSRLIN: D = 10: GOSUB 1770
220 PRINT
230 PRINT " INSTALL IN A GROUNED METAL ENCLOSURE"
240 PRINT: PRINT
250 PRINT " To run this program press <1>"
260 PRINT " To see program notes press <2>"
270 PRINT " To EXIT press <0>"
280 Z$ = INKEY$: IF Z$ = "" THEN 280
290 IF Z$ = "0" THEN RUN GO$
300 IF Z$ = "1" THEN 330
310 IF Z$ = "2" THEN 2360
320 GOTO 280
330 '.....start
340 CLS
350 'DEF FNHT(X)=(EXP(X)-EXP(-X))/(EXP(X)+EXP(-X))
360 'DEF FNHS(X)=(EXP(X)-EXP(-X))/2 :DEF FNHC(X)=(EXP(X)+EXP(-X))/2
370 PI = 4 * ATN(1): NH = 9.999999E-10: PF = 9.999999E-13: MHZ = 10 ^ 6: V0 = 3.000000E+10 / 2.539999 / 12 'FT/SEC
380 Z = CSRLIN
390 LL = 2: GOSUB 1770 'print schematic
400 INPUT " ENTER: Filter center frequency (MHz) "; F0: F0 = F0 * MHZ: W0 = 2 * PI * F0
410 INPUT " ENTER: Passband ripple in db (Max 3) "; AM
420 IF AM > 3 THEN 410
430 RHO = SQR(1 - 10 ^ (-AM / 10)): VSWR = (1 + RHO) / (1 - RHO) 'SWR=VSWR
440 MX = F0 / MHZ * 2.000000E-01
450 PRINT " ENTER: Ripple bandwidth in MHz (max"; MX; ") ";: INPUT BW
460 IF BW = 0 OR BW > MX THEN BW = 0
470 IF BW = 0 THEN 450
480 BW = BW * MHZ
490 COLOR 0, 7: PRINT " NOTE The more sections the steeper the skirts ";: COLOR 7, 0
500 INPUT " ENTER: Number of resonator sections (maximum 9) "; N
510 IF N < 1 OR N > 9 THEN 500
520 INPUT " ENTER: Filter source impedance (ê) "; R0
530 INPUT " ENTER: Filter load impedance (ê) "; RL
540 GOSUB 1940 'select cable
550 V = V0 * VF 'propagation velocity of selected cable
560 LDB = LT / 100 'cable attenuation in dB/foot
570 L = R0 / W0: WL = V / F0 ' cable wavelength in feet
580 CLS
590 '.....calculate
600 BETA = LOG(1 / CalcHT(AM / 1.737000E+01))
610 NU = CalcHS(BETA / 2 / N)
620 FOR M = 1 TO N
    630 A(M) = SIN((2 * M - 1) * PI / 2 / N)
    640 B(M) = NU * NU + (SIN(M * PI / N)) ^ 2
650 NEXT M
660 R = (CalcHT(BETA / 4)) ^ 2
670 G(0) = 1: G(1) = 2 * A(1) / NU
680 FOR M = 2 TO N
    690 G(M) = 4 * A(M - 1) * A(M) / B(M - 1) / G(M - 1)
700 NEXT M
710 IF INT(N / 2) = N / 2 THEN G(N + 1) = 1 / R ELSE G(N + 1) = 1
720 Q(1) = G(0) * G(1)
730 Q(N) = G(N) * G(N + 1)
740 FOR M = 1 TO N - 1
    750 K(M) = 1 / SQR(G(M) * G(M + 1))
760 NEXT M
770 W = 2 * PI * F0
780 QBP = F0 / BW
790 Q = QBP * Q(1)
800 CN = 1 / W / W / L
810 R = W * L * Q
820 CS1 = SQR(1 / W / W / R0 / (R - R0))
830 CP1 = CS1 * (R0 - R) / R
840 CS2 = SQR(1 / W / W / RL / (R - RL))
850 CP2 = CS2 * (RL - R) / R
860 C(0) = 0
870 C(N) = 0
880 FOR M = 1 TO N - 1
    890 K1(M) = K(M) / QBP
    900 C(M) = K1(M) * CN: IF C(M) < 0 THEN L = L / 1.100000: GOTO 800
    910 S(M) = V / W0 * ATN(W0 * C(M) * ZR)
920 NEXT M
930 FOR M = 1 TO N
    940 C1(M) = CN - C(M - 1) - C(M): IF C1(M) < 0 THEN L = L / 1.100000: GOTO 800
950 NEXT M
960 C1(1) = C1(1) + CP1: IF C1(1) < 0 THEN L = L / 1.100000: GOTO 800
970 C1(N) = C1(N) + CP2: IF C1(N) < 0 THEN L = L / 1.100000: GOTO 800
980 C(0) = CS1: S(0) = V / W0 * ATN(W0 * C(0) * ZR)
990 C(N) = CS1: S(N) = V / W0 * ATN(W0 * C(N) * ZR)
1000 '.....calculate capacitances
1010 CLS
1020 S1 = V / W0 * ATN(W0 * L / ZR)
1030 PRINT " RESONATORS"; TAB(22); "Length (mm)"
1040 PRINT USING "## S1 elements, ea.    ###.#"; N, S1 * 3.047999E+01
1050 FOR M = 1 TO N
    1060 S2(M) = V / W0 * ATN(W0 * C1(M) * ZR)
    1070 PRINT USING " S2(##)                ###.#"; M; S2(M) * 3.047999E+01
1080 NEXT M
1090 PRINT " COUPLERS"; TAB(22); "Length (mm)"
1100 TT = 0: FOR M = 0 TO N
    1110 AA = C(M) / PF: BB(M) = S(M) * 3.047999E+01: TT = TT + S(M)
    1120 PRINT USING " C(# ##)= ###.### pF   ###.# "; M; M + 1; AA; S(M) * 3.047999E+01
1130 NEXT M
1140 '.....display
1150 LL = 1: D = 40: E = 41: GOSUB 1770 'pr1nt diagram
1160 J = CSRLIN - 2
1170 LOCATE J + 1, E: PRINT "Cable: "; C$(NC, 1)
1180 LOCATE J + 2, E: PRINT USING "Center Frequency......... ####.### MHz"; F0 / MHZ
1190 LOCATE J + 3, E: PRINT USING "Number of resonators..... ####"; N
1200 LOCATE J + 4, E: PRINT USING "Line velocity factor..... ###.##"; VF
1210 LOCATE J + 5, E: PRINT USING "Line impedance........... ####.# ohms"; ZR
1220 LOCATE J + 6, E: PRINT USING "Passband ripple.......... ####.### dB"; AM
1230 LOCATE J + 7, E: PRINT USING "Passband width........... ####.### MHz"; BW / 10 ^ 6
1240 LOCATE J + 8, E: PRINT USING "Source impedance......... ####.### ê"; R0
1250 LOCATE J + 9, E: PRINT USING "Load impedance........... ####.### ê"; RL
1260 LOCATE J + 10, E: PRINT USING "SWR...................... ####.#"; VSWR
1270 PRINT
1280 LOCATE J + 14, E: PRINT "Press (a) to analyze filter."
1290 LOCATE J + 15, E: PRINT "Press (b) for enclosure details"
1300 LOCATE J + 16, E: PRINT "Press (v) for L & C equivilents"
1310 LOCATE J + 17, E: PRINT "Press <0> to finish."
1320 LOCATE 24, 26: PRINT "CAPACITY COUPLED RESONATORS";
1330 A$ = INKEY$: IF A$ = "" THEN 1330
1340 IF A$ = "0" THEN GOSUB 1840: CLS: RUN
1350 IF A$ = "a" THEN 1390
1360 IF A$ = "v" THEN GOSUB 2800: GOSUB 1840: CLS: GOTO 1000
1370 IF A$ = "b" THEN GOSUB 2580: GOSUB 1840: CLS: GOTO 1000
1380 GOTO 1330
1390 '----------------routine to find insertion loss--------------------
1400 ALPHA = LDB / 8.685999
1410 F = F0: VS = 1: XX = 4
1420 PRINT "  Frequency, MHz         Power Loss, dB";
1430 FOR P = 0 TO 18: F = F0 + BW * (XX / 9 * P - XX)
    1440 R = RL: X = 0: RP(N + 1) = RL: W = 2 * PI * F
    1450 GOSUB 1710 'get GL and BL at each frequency
    1460 R = RL: X = 0: RP(N + 1) = RL: W = 2 * PI * F
    1470 FOR M = N TO 1 STEP -1
        1480 GOSUB 1740 'get GC(M) and BC(M) at each frequency
        1490 XS = X - ZR / TAN(W / V * S(M)): RS = R: GOSUB 1660 'find Geq, Req(m), & Beq
        1500 BP = BEQ + BL + BC(M): GP = GEQ + GL + GC(M): RP(M) = 1 / GP: XP = -1 / BP: GOSUB 1690
    1510 NEXT M
    1520 X = X - ZR / TAN(W / V * S(0)): REQ(0) = R: V(0) = VS * R / SQR((R + R0) ^ 2 + X ^ 2)
    1530 FOR M = 1 TO N + 1
        1540 V(M) = V(M - 1) * SQR(RP(M) / REQ(M - 1))
    1550 NEXT M
    1560 PIN = VS ^ 2 / 4 / R0: POUT = V(N + 1) ^ 2 / RL: LOSS = 10 * LOG(PIN / POUT) / LOG(10)
    1570 PRINT USING "    ####.###              ##.### "; F / MHZ; LOSS
1580 NEXT P 'LOCATE 18
1590 PRINT "Press <2> for narrow range, <1> for wide range, or <0> to QUIT"
1600 A$ = INKEY$
1610 IF A$ = "0" THEN GOSUB 1840: CLS: GOTO 1000
1620 IF A$ = "1" THEN CLS: XX = 4: GOTO 1420
1630 IF A$ = "2" THEN CLS: XX = 5.000000E-01: GOTO 1420
1640 IF A$ = "3" THEN CLS: XX = 2.500000E-01: GOTO 1420
1650 GOTO 1600
1660 '--------------subroutine to find parallel Beq & Geq--------------
1670 XEQ = (XS ^ 2 + RS ^ 2) / XS: REQ(M) = (RS ^ 2 + XS ^ 2) / RS
1680 BEQ = -1 / XEQ: GEQ = 1 / REQ(M): RETURN
1690 '--------------subroutine to find series R & X--------------------
1700 R = RP(M) * XP ^ 2 / (RP(M) ^ 2 + XP ^ 2): X = XP * RP(M) ^ 2 / (RP(M) ^ 2 + XP ^ 2): RETURN
1710 '---------------subroutine to find GL & BL--------------
1720 XS = 2 * ALPHA * S1: YS = 2 * W * S1 / V: DENOM = CalcHC(XS) - COS(YS)
1730 GL = CalcHS(XS) / DENOM / ZR: BL = -SIN(YS) / DENOM / ZR: RETURN
1740 '---------------subroutine to find GC(M) and BC(M)-------------
1750 XS = 2 * ALPHA * S2(M): YS = 2 * W * S2(M) / V: DENOM = CalcHC(XS) + COS(YS)
1760 GC(M) = CalcHS(XS) / DENOM / ZR: BC(M) = SIN(YS) / DENOM / ZR: RETURN
1770 '-------------------Schematic----------------------------
1780 LOCATE LL + 0, D: PRINT "*ÄC(0,1)ÄÂÄÄÄÂÄC(1,2)ÄÂÄÄÄÂÄ\ÄC(N,N+1)Ä¿";
1790 LOCATE LL + 1, D: PRINT "        º³º º³º      º³º º³º           ³";
1800 LOCATE LL + 2, D: PRINT "        º³º º³ºS2(1) º³º º³ºS2(2)      RL";
1810 LOCATE LL + 3, D: PRINT "      S1º³º º³º    S1º³º º³º®Äshield   ³";
1820 LOCATE LL + 4, D: PRINT "*ÄÄÄÄÄÄÄÐÁÐÄÄÄÁÄÄÄÄÄÄÐÁÐÄÄÄÁÄ\ÄÄÄÄÄÄÄÄÄÙ";
1830 PRINT: RETURN
1840 '.....PRT
1850 KEY OFF: GOSUB 1920: LOCATE 25, 5: COLOR 0, 2
1860 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1870 PRINT "(3)Next page? (1/2/3)";: COLOR 7, 0
1880 Z$ = INKEY$: IF Z$ < "1" OR Z$ > "3" THEN 1880 ELSE GOSUB 1920
1890 IF Z$ = "3" THEN RETURN
1900 FOR I% = 1 TO 24: FOR J% = 1 TO 80: LPRINT CHR$(SCREEN(I%, J%));: NEXT J%: NEXT I%
1910 IF Z$ = "2" THEN LPRINT CHR$(12) ELSE GOTO 1850
1920 LOCATE 25, 1: PRINT STRING$(80, 32);: RETURN
1930 END
1940 '--------------subroutine to select cable and get parameters---------
1950 'Given F0 from the calling program, this program gets coax data from
1960 'COAXFIL.FIL, then uses the selected cable data to get values for ZR, VF,
1970 'and dB/100'.
1980 '
1990 'DIM C$(60, 9)
2000 '            ----read data----
2010 OPEN "I", 1, "C:\HAMCALC\COAXFILE\COAXIAL.FIL"
2020 FOR Z = 1 TO 40: FOR Y = 1 TO 9
        2030 INPUT #1, Z$
        2040 C$(Z, Y) = Z$
2050 NEXT Y: NEXT Z
2060 CLOSE
2070 FOR X = 1 TO 40 'calculate attenuation/100ft
    2080 Z0 = VAL(C$(X, 5)) 'impedance
    2090 D = VAL(C$(X, 4)) 'ID of outer conducter
    2100 E = VAL(C$(X, 3)) 'dielectric constant
    2110 VF = INT(1 / SQR(E) * 1000) / 1000
    2120 C$(X, 3) = STR$(VF) 'velocity factor
    2130 B = 10 ^ (Z0 * SQR(E) / 138) 'D/d constant
    2140 IF RIGHT$(C$(X, 7), 1) = "f" THEN K1 = 9.390000E-01 ELSE K1 = 1
    2150 K2 = 1: K = K1 + K2
    2160 A = 4.350000E-01 / Z0 / D * B * K * SQR(F0) / 1000 'attenuation per 100 feet
    2170 A = INT(A * 1000) / 1000
    2180 C$(X, 6) = STR$(A) 'enter attenuation value into array
2190 NEXT X
2200 CLS: PRINT "            ----------Select a Cable Type-----------"
2210 PRINT " For"; F0 / 10 ^ 6; "MHz select a cable with........"
2220 PRINT "       Type          ZR     Velocity Factor  dB Loss per 100 feet";
2230 FOR P = 1 TO 19
    2240 PRINT P; TAB(6); C$(P, 1); TAB(21); C$(P, 5); TAB(34); C$(P, 3); TAB(50); C$(P, 6)
2250 NEXT P
2260 PRINT "            Press any key for more";
2270 A$ = INKEY$: IF A$ = "" THEN 2270 ELSE CLS
2280 PRINT "       Type          ZR     Velocity Factor  dB Loss per 100 feet";
2290 FOR P = 20 TO 40
    2300 PRINT P; TAB(6); C$(P, 1); TAB(21); C$(P, 5); TAB(34); C$(P, 3); TAB(50); C$(P, 6)
2310 NEXT P
2320 INPUT "Enter <0> to review the cables or to select one, enter its number ", P
2330 IF P < 1 THEN 2220 ELSE IF P > 40 THEN 2220
2340 ZR = VAL(C$(P, 5)): VF = VAL(C$(P, 3)): LT = VAL(C$(P, 6)): NC = P
2350 CLS: RETURN
2360 CLS: PRINT "        -------------Program Notes---------------------";
2370 PRINT "This program is derived from a C++ program by Glen V.Rosenbaum, in the October";
2380 PRINT "1998 issue of `Applied Microvaves & Wireless'. His program used the Chebychev";
2390 PRINT "synthesis methods found in Zverev and others. Here, we replace the Ls and Cs";
2400 PRINT "with lengths of coax. Since the equivalence is only true at a single frequency,";
2410 PRINT "the resulting filter differs from the LC filter. You'll find that the rolloff";
2420 PRINT "is better than the LC version, the insertion loss is greater, due to the cable";
2430 PRINT "cable Q being lower than the Q of high quality RF capacitors, and the band";
2440 PRINT "width is much less, about half the LC version. Coupled resonator filters have";
2450 PRINT "the ability to act as matching filters, with no restrictions on source or load";
2460 PRINT "resistance. This makes them attractive for use with antennas which often have";
2470 PRINT "odd ball input impedances.";
2480 PRINT "            ----------Program Description--------------";
2490 PRINT "The ripple, source and load, and frequency data is used to calculate the Ls";
2500 PRINT "and Cs. After the user selects a cable type, the cable lengths are calculated";
2510 PRINT "and displayed. The analysis section does a brute strength ladder analysis,";
2520 PRINT "using the hyperbolic transmission line equations to represent the R and X of";
2530 PRINT "the open and shorted cables. The resistance values are saved at each stage, so";
2540 PRINT "the power loss can be tracked from input to output. Both cable and LC versions";
2550 PRINT "can be analyzed."
2560 PRINT: PRINT "Press any key to return "
2570 A$ = INKEY$: IF A$ = "" THEN 2570 ELSE GOTO 20
2580 '.........enclosure
2590 CLS
2600 PRINT "       ÉÍÍÍÍÍÍÍÍÍ\Í»";
2610 PRINT " line ÄÄaÂÄÂaÂÄÂaÄ\ÄÄto next a";
2620 PRINT "     ÚÄ¶ b c b c   ÇÄ®Ä ground";
2630 PRINT "     Ï ÈÍÏÍÏÍÏÍÏÍ\Í¼";
2640 PRINT "      METAL ENCLOSURE";
2650 PRINT ";"
2660 PRINT
2670 PRINT "        ÕÍÍÍÍÍÍ¸®¿";
2680 PRINT " a= ÄÄÄÄÄÄÄÄÄÄÄÄ ÀÄconnect shield to next <a>";
2690 PRINT "        ÔÍÍÍÍÍÍ¾";
2700 PRINT "        ÕÍÍÍÍÍÍ¸®¿";
2710 PRINT " b= ÄÄÄÄÄÄÄÄÄÄÄÄ®ÁÄconnect shield and inner conductor to ground";
2720 PRINT "        ÔÍÍÍÍÍÍ¾";
2730 PRINT "        ÕÍÍÍÍÍÍ¸®Ä connect shield to ground";
2740 PRINT " c= ÄÄÄÄÄÄÄÄÄÄÄÄ®Ä cut off flush with dielectric and shield";
2750 PRINT "    ³   ÔÍÍÍÍÍÍ¾";
2760 PRINT "    ³   ³      ³";
2770 PRINT "    ³®Â¯³®ÄÄÄÄ¯³®Ä cut shield & dielectric to lengths specified";
2780 PRINT "      À 50 mm inner conductor"
2790 RETURN
2800 '........Filter L and C values"
2810 CLS: LOCATE 2, 2: PRINT USING "Inductor Value...... ####.## nH"; L / NH
2820 FOR M = 1 TO N
    2830 LOCATE 2 + M, 5: PRINT USING "C2(#).... ###.## pF"; M; C1(M) / PF
2840 NEXT M
2850 FOR M = 0 TO N
    2860 LOCATE 2 + N + 2 + M, 3: PRINT USING "C(## ##).... ###.## pF"; M; M + 1; C(M) / PF
2870 NEXT M: LOCATE 23, 1
2880 PRINT "Press (1) to see insertion loss values or (0) to return";
2890 A$ = INKEY$: IF A$ = "" THEN 2890
2900 IF A$ = "1" THEN 2920
2910 RETURN
2920 CLS '----------------routine to find insertion loss--------------------
2930 PRINT " For a perfomance report ENTER an inductor Q at"; F0 / MHZ; "Mhz ";: INPUT QU
2940 RQ = W * L * (1 + QU ^ 2) / QU
2950 F = F0: VS = 1: XX = 4
2960 PRINT "  Frequency, MHz   Power Loss, dB   VSWR";
2970 FOR P = 0 TO 18: F = F0 + BW * (XX / 9 * P - XX)
    2980 R = RL: X = 0: RP(N + 1) = RL: W = 2 * PI * F
    2990 FOR M = N TO 1 STEP -1
        3000 XS = X - 1 / W / C(M): RS = R: GOSUB 1660 'find Geq, Req(m), & Beq
        3010 BP = BEQ + W * C1(M) - 1 / W / L: GP = GEQ + 1 / RQ: RP(M) = 1 / GP: XP = -1 / BP: GOSUB 1690
    3020 NEXT M
    3030 X = X - 1 / W / C(0): REQ(0) = R: V(0) = VS * R / SQR((R + R0) ^ 2 + X ^ 2)
    3040 NUM = (R - R0) ^ 2 + X ^ 2: DENOM = (R + R0) ^ 2 + X ^ 2: RHO = SQR(NUM / DENOM)
    3050 SWR = (1 + RHO) / (1 - RHO): IF SWR > 900 THEN SWR = 900
    3060 FOR M = 1 TO N + 1
        3070 V(M) = V(M - 1) * SQR(RP(M) / REQ(M - 1))
    3080 NEXT M: IF V(N + 1) < 1.000000E-18 THEN V(N + 1) = 1.000000E-18
    3090 PIN = VS ^ 2 / 4 / R0: POUT = V(N + 1) ^ 2 / RL: LOSS = 10 * LOG(PIN / POUT) / LOG(10)
    3100 PRINT USING "    ####.###         ###.###    ######.###"; F / MHZ; LOSS; SWR
3110 NEXT P
3120 PRINT "Press (1),(2),or (3) for different ranges, or <0> to return";
3130 A$ = INKEY$
3140 IF A$ = "0" THEN CLS: GOTO 2910
3150 IF A$ = "1" THEN CLS: XX = 4: GOTO 2960
3160 IF A$ = "2" THEN CLS: XX = 7.500000E-01: GOTO 2960
3170 IF A$ = "3" THEN CLS: XX = 2.500000E-01: GOTO 2960
3180 GOTO 3130
3190 END

FUNCTION ImageBufferSize& (Wide&, Deep&, mode%)
SELECT CASE ScreenMode%
    CASE 1: BPPlane = 2: Planes = 1
    CASE 2, 3, 4, 11: BPPlane = 1: Planes = 1
    CASE 7, 8, 9, 12: BPPlane = 1: Planes = 4
    CASE 10: BPPlane = 1: Planes = 2
    CASE 13: BPPlane = 8: Planes = 1
    CASE ELSE: BPPlane = 0
END SELECT
ImageBufferSize& = 4 + INT((Wide& * BPPlane + 7) / 8) * (Deep& * Planes)
END FUNCTION

' Quick Basic does not like functions that start with 'FN'
'DEF FNHT(X)=(EXP(X)-EXP(-X))/(EXP(X)+EXP(-X))
FUNCTION CalcHT(X)
   CalcHT = x * 3 : 'FNHT = X : '(EXP(X) - EXP(-X)) / (EXP(X) + EXP(-X))
END FUNCTION

'DEF FNHS(X)=(EXP(X)-EXP(-X))/2 :DEF FNHC(X)=(EXP(X)+EXP(-X))/2
FUNCTION CalcHS (x)
CalcHS = x/2 : '(EXP(x) - EXP(-x)) / 2
END FUNCTION
'DEF CalcHC(X)=(EXP(X)+EXP(-X))/2
FUNCTION CalcHC (x)
CalcHC = X : '(EXP(x) + EXP(-x)) / 2
END FUNCTION

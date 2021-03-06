1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$
10 'TRAPROP - Trap Properties - 12 APR 97 rev. 27 JUL 97
20 IF EX$=""THEN EX$="EXIT"
30 
40 CLS:KEY OFF
50 COLOR 7,0,1
60 U$="######.###"
70 V$="#####,###.###
80 '
90 CLS:COLOR 15,2
100 PRINT " TRAP PROPERTIES ESTIMATOR";TAB(62);;"by L.B.Cebik W4RNL ";
110 PRINT STRING$(80,32);
120 LOCATE CSRLIN-1,20:PRINT "Edited for HAMCALC by George Murphy VE3ERP";
130 COLOR 1,0:PRINT STRING$(80,223);:COLOR 7,0
140 TB=7
150 PRINT TAB(TB);
160 PRINT "To estimate the properties of a trap at various frequencies, you"
170 PRINT TAB(TB);
180 PRINT "will need to know or be able to estimate the following information:"
190 PRINT
200 L1=CSRLIN
210 PRINT TAB(TB);
220 PRINT "  (a) The resonant frequency of the trap (normally, just below or at"
230 PRINT TAB(TB);
240 PRINT "      the lowest frequency of the band for which it is a trap), and"
250 PRINT TAB(TB);
260 PRINT "       either:"
270 PRINT TAB(TB);
280 PRINT "  (b) The inductance and Q of the trap coil"
290 PRINT TAB(TB);
300 PRINT "       or"
310 PRINT TAB(TB);
320 PRINT "  (c) The inductive reactance and resistance of the trap coil."
330 PRINT
340 PRINT TAB(TB);
350 PRINT "The program does not attempt to calculate antenna lengths, due to"
360 PRINT TAB(TB);
370 PRINT "the many other interrelated variables involved. Estimated starting"
380 PRINT TAB(TB);
390 PRINT "point antenna lengths for various antenna types may be calculated"
400 PRINT TAB(TB);
410 PRINT "using many of HAMCALC's specific antenna programs."
420 PRINT:L2=CSRLIN:IF FLAG=1 THEN FLAG=0:GOTO 500
430 COLOR 0,7:LOCATE ,24:PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
440 A$=INKEY$:IF A$=""OR A$>"1"OR A$<"0"THEN 440
450 LOCATE L2:PRINT STRING$(80,32);:LOCATE L2
460 IF A$="0"THEN CLS:RUN EX$
470 IF A$="1"THEN 500
480 GOTO 440
490 '
500 INPUT " ENTER: Resonant frequency of trap in MHz.....";FR
510 VIEW PRINT L1 TO L1+1:CLS:VIEW PRINT:LOCATE L2
520 PRINT " Press letter in ( ) above to indicate other known data....."
530 A$=INKEY$:IF A$=""OR A$<"b"OR A$>"c"THEN 530
540 VIEW PRINT L1 TO 24:CLS:VIEW PRINT:LOCATE L1
550 IF A$="b"THEN 590
560 IF A$="c"THEN 630
570 GOTO 530
580 '
590 INPUT " ENTER: Inductance in �H.....";L
600 INPUT " ENTER: Inductor Q...........";Q
610 XL=(6.280000*FR)*L:R=XL/Q:GOTO 670
620 '
630 INPUT " ENTER: Inductive reactance in ohms.....";XL
640 INPUT " ENTER: Coil resistance in ohms.........";R
650 L=XL/(6.280000*FR):Q=XL/R:GOTO 670
660 '
670 C=25330/(L*(FR^2)):CB=C/9.999999E+11
680 VIEW PRINT 4 TO 24:CLS:VIEW PRINT:LOCATE 4
690 PRINT " Basic Trap Properties at Trap Resonance:"
700 PRINT " ";STRING$(40,196);";
710 PRINT " Resonant Frequency......";USING U$;FR;:PRINT " MHz"
720 PRINT " Coil Q..................";USING U$;Q
730 PRINT " Coil inductance.........";USING U$;L;:PRINT " �H"
740 PRINT " Coil reactance..........";USING U$;XL;:PRINT " �"
750 PRINT " Capacitor...............";USING U$;C;:PRINT " pF"
760 PRINT
770 F1$="Trap Assembly at Three Selected Frequencies"
780 F2$="Trap Assembly at Several Frequencies in One Band"
790 LN=CSRLIN:PRINT " Press letter in ( ) to select type of output data:"
800 PRINT
810 PRINT "   (a) ";F1$
820 PRINT "   (b) ";F2$
830 A$=INKEY$:IF A$<"a"OR A$>"b"THEN 830
840 IF A$="a"THEN F$=F1$:GOSUB 880:GOTO 920
850 IF A$="b"THEN F$=F2$:GOSUB 880:GOTO 1190
860 GOTO 830
870 '
880 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN,9:PRINT F$
890 PRINT TAB(9);STRING$(LEN(F$),196)
900 RETURN
910 '
920 LN=CSRLIN
930 INPUT " ENTER: Frequency #1 in MHz.....";F1
940 INPUT " ENTER: Frequency #2 in MHz.....";F2
950 INPUT " ENTER: Frequency #3 in MHz.....";F3
960 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
970 '
980 XL1=6.280000*(F1*L):R1=XL1/Q:N1=R1^2+XL1^2:R1P=N1/R1:XL1P=N1/XL1
990 FC1=F1*10^6:XC1=1/(6.280000*(FC1*CB)):D1=XL1-XC1:IF D1=0 THEN D1=10^-6
1000 XN1=-(XL1*XC1)/D1:DP1=R1P^2+XN1^2:RS1=(R1P*(XN1^2))/DP1
1010 XLS1=((R1P^2)*XN1)/DP1:Q1=ABS(XLS1/RS1)
1020 '
1030 XL2=6.280000*(F2*L):R2=XL2/Q:N2=R2^2+XL2^2:R2P=N2/R2:XL2P=N2/XL2
1040 FC2=F2*10^6:XC2=1/(6.280000*(FC2*CB)):D2=XL2-XC2:IF D2=0 THEN D2=10^-6
1050 XN2=-(XL2*XC2)/D2:DP2=R2P^2+XN2^2:RS2=(R2P*(XN2^2))/DP2
1060 XLS2=((R2P^2)*XN2)/DP2:Q2=ABS(XLS2/RS2)
1070 '
1080 XL3=6.280000*(F3*L):R3=XL3/Q:N3=R3^2+XL3^2:R3P=N3/R3:XL3P=N3/XL3
1090 FC3=F3*10^6:XC3=1/(6.280000*(FC3*CB)):D3=XL3-XC3:IF D3=0 THEN D3=10^-6
1100 XN3=-(XL3*XC3)/D3:DP3=R3P^2+XN3^2:RS3=(R3P*(XN3^2))/DP3
1110 XLS3=((R3P^2)*XN3)/DP3:Q3=ABS(XLS3/RS3)
1120 '
1130 PRINT TAB(5);"Freq.(MHz)";TAB(19);"React.(�)";TAB(32);"Resis.(�)";TAB(49);"Q"
1140 PRINT USING V$;F1;XLS1;RS1;Q1
1150 PRINT USING V$;F2;XLS2;RS2;Q2
1160 PRINT USING V$;F3;XLS3;RS3;Q3
1170 LN=CSRLIN:GOSUB 1410:GOTO 1320
1180 '
1190 LN=CSRLIN
1200 INPUT " ENTER: Lowest frequency (MHz) for band scan.....";FL
1210 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
1220 PRINT TAB(5);"Freq.(MHz)";TAB(19);"React.(�)";TAB(32);"Resis.(�)";TAB(49);"Q"
1230 FOR J=FL TO (FL+5.000000E-01) STEP 1.000000E-01
1240  F1=J:XL1=6.280000*(F1*L):R1=XL1/Q:N1=R1^2+XL1^2:R1P=N1/R1:XL1P=N1/XL1
1250  FC1=F1*10^6:XC1=1/(6.280000*(FC1*CB)):D1=XL1-XC1:IF D1=0 THEN D1=10^-6
1260  XN1=-(XL1*XC1)/D1:DP1=R1P^2+XN1^2:RS1=(R1P*(XN1^2))/DP1
1270  XLS1=((R1P^2)*XN1)/DP1:Q1=ABS(XLS1/RS1)
1280  PRINT USING V$;F1;XLS1;RS1;Q1
1290 NEXT J
1300 LN=CSRLIN:GOSUB 1410:GOTO 1320
1310 '
1320 LOCATE 25,1:PRINT STRING$(79,32);:LOCATE LN+1,10:COLOR 0,7
1330 PRINT " Press 1 for a new trap, 2 for a new frequency run, 0 to quit "
1340 COLOR 7,0
1350 A$=INKEY$:IF A$<"0"OR A$>"2"THEN 1350
1360 IF A$="0"THEN 90
1370 IF A$="1"THEN FLAG=1:GOTO 10
1380 IF A$="2"THEN 680
1390 GOTO 1350
1400 '
1410 'PRT
1420 KEY OFF:GOSUB 1490:LOCATE 25,5:COLOR 0,2
1430 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1440 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1450 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1450 ELSE GOSUB 1490
1460 IF Z$="3"THEN RETURN
1470 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1480 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1420
1490 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

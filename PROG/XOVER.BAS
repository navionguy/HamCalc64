1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 '\hamcalc\prog\XOVER - Crossover Networks - 18 APR 01
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 COLOR 7,0,0
50 PI=4*ATN(1)  '3.141593
60 UL$=STRING$(80,205)
70 R$=" Ref: Radiotron Designer's Handbook, sect.4.8, pp 184-185"
80 '
90 '.....title page
100 CLS
110 COLOR 15,2
120 PRINT " CROSSOVER NETWORKS - LOUDSPEAKERS";
130 PRINT TAB(57);"by George Murphy VE3ERP ";
140 COLOR 1,0:PRINT STRING$(80,223);
150 COLOR 7,0
160 '
170 '.....start
180 L0=0:L1=0:L2=0:L3=0:L4=0:L5=0:L6=0:L7=0
190 C0=0:C1=0:C2=0:C3=0:C4=0:C5=0:C6=0:C7=0
200 GOSUB 1520
210 PRINT UL$;
220 LN=CSRLIN
230 PRINT R$
240 PRINT UL$;
250 PRINT " To run program, ENTER desired crossover frequency in Hz . . . ."
260 PRINT " To exit program, ENTER 0 (zero) . . . ."
270 PRINT UL$;
280 INPUT " ENTER: Frequency in Hz or 0 (zero) to exit......";F:F=INT(F)
290 IF F>0 THEN 310 ELSE CLS:CHAIN EX$
300 '
310 '.....choose attenuation
320 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
330 PRINT " Crossover frequency =";F;;"Hz";
340 PRINT UL$;
350 GOSUB 1640
360 COLOR 0,7:LOCATE ,3
370 PRINT " Press letter in < > to select attenuation:";:COLOR 7,0
380 PRINT "     <a>  6 db"
390 PRINT "     <b> 12 db"
400 PRINT "     <c> 18 db"
410 Z$=INKEY$:IF Z$=""THEN 410
420 IF Z$="a"THEN 550
430 IF Z$="b"THEN 630
440 IF Z$="c"THEN 730
450 GOTO 410
460 '
470 '.....enter impedance
480 CLS:COLOR 0,7
490 LOCATE ,22:PRINT F;;"Hz SPEAKER CROSSOVER NETWORKS ";:COLOR 7,0
500 PRINT UL$;
510 INPUT " ENTER: Loudspeaker circuit impedance Zo (ohms)......";ZO
520 LN=CSRLIN-1:VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
530 RETURN
540 '
550 '.....6db calculation
560 GOSUB 470
570 GOSUB 1130
580 PRINT UL$;
590 L0=ZO/(2*PI*F)
600 C0=1/(2*PI*F*ZO)
610 LN=CSRLIN:GOTO 900
620 '
630 '.....12 db calculation
640 GOSUB 470
650 GOSUB 1260
660 PRINT UL$;
670 L1=ZO/(2*SQR(2)*PI*F)
680 L2=ZO/(SQR(2)*PI*F)
690 C1=1/(SQR(2)*PI*F*ZO)
700 C2=1/(2*SQR(2)*PI*F*ZO)
710 LN=CSRLIN:GOTO 900
720 '
730 '.....18 db calculation
740 GOSUB 470
750 GOSUB 1390
760 PRINT UL$;
770 M=6.000000E-01  'see M-derived filters, page 182-183
780 L3=(1+M)*Z0/(2*PI*F)
790 L4=ZO/(2*PI*F)
800 L5=ZO/(4*PI*F)
810 L6=2*ZO/(2*PI*F)
820 L7=ZO/((1+M)*(2*PI*F))
830 C3=1/(PI*F*ZO)
840 C4=1/((1+M)*(2*PI*F*ZO))
850 C5=1/(2*PI*F*ZO)
860 C6=1/(4*PI*F*ZO)
870 C7=(1+M)/(2*PI*F*ZO)
880 LN=CSRLIN:GOTO 900
890 '
900 '.....print
910 LOCATE LN
920 L$=" .......... ######.# ФH"
930 C$=" .......... ######.# ФF"
940 PRINT USING " Crossover frequency...................... ######.# Hz";F
950 PRINT USING " Zo (speaker impedance)................... ######.# ohms";ZO
960 IF L0 THEN PRINT " L0";USING L$;L0*10^6,C0*10^6;" Ё C0";C$
970 IF L1 THEN PRINT " L1";USING L$;L1*10^6,C1*10^6;" Ё C1";C$
980 IF L2 THEN PRINT " L2";USING L$;L2*10^6,C2*10^6;" Ё C2";C$
990 IF L3 THEN PRINT " L3";USING L$;L3*10^6,C3*10^6;" Ё C3";C$
1000 IF L4 THEN PRINT " L4";USING L$;L4*10^6,C4*10^6;" Ё C4";C$
1010 IF L5 THEN PRINT " L5";USING L$;L5*10^6,C5*10^6;" Ё C5";C$
1020 IF L6 THEN PRINT " L6";USING L$;L6*10^6,C6*10^6;" Ё C6";C$
1030 IF L7 THEN PRINT " L7";USING L$;L7*10^6,C7*10^6;" Ё C7";C$
1040 PRINT UL$;
1050 PRINT R$
1060 PRINT
1070 PRINT " To design custom inductors and capacitors close to these values,";
1080 PRINT " try Hamcalc's"
1090 PRINT " ";CHR$(34);;"Capacitors - Custom Value";CHR$(34);;" and ";CHR$(34);
1100 PRINT "Toroid Inductor Calculator";CHR$(34);;" programs."
1110 GOTO 1800
1120 '
1130 '.....6db diagram
1140 COLOR 0,7:C=14
1150 LOCATE ,C:PRINT "           Series              Parallel              "
1160 LOCATE ,C:PRINT "         здддбддд©          здддбддL0дд©             "
1170 LOCATE ,C:PRINT "         Ё  C0  Zo(LF)╞    ╞Zo  Ё     Zo(LF)╞        "
1180 LOCATE ,C:PRINT "        ╞Zo  цддд╢          юдбдЁдддддды             "
1190 LOCATE ,C:PRINT "         Ё  L0  Zo(HF)╞       Ё юддC0дд©             "
1200 LOCATE ,C:PRINT "         юдддаддды            Ё       Zo(HF)╞        "
1210 LOCATE ,C:PRINT " LF=Tweeter  HF=Woofer        юдддддддды             "
1220 LOCATE ,C:PRINT "             6 db Attenuation Per Octave             "
1230 COLOR 7,0
1240 RETURN
1250 '
1260 '.....12db diagram
1270 COLOR 0,7:C=14
1280 LOCATE ,C:PRINT "          Series                Parallel             "
1290 LOCATE ,C:PRINT "      здддбддL1дд©          здддбддL2ддбддд©         "
1300 LOCATE ,C:PRINT "      Ё  C1     Zo(LF)╞    ╞Zo  Ё     C2  Zo(LF)╞    "
1310 LOCATE ,C:PRINT "     ╞Zo  цдддддд╢          юдбдЁддддддаддды         "
1320 LOCATE ,C:PRINT "      Ё  L1     Zo(HF)╞       Ё юддC2ддбддд©         "
1330 LOCATE ,C:PRINT "      юдддаддC1дды            Ё       L2  Zo(HF)╞    "
1340 LOCATE ,C:PRINT " LF=Tweeter  HF=Woofer        юддддддддаддды         "
1350 LOCATE ,C:PRINT "            12 db Attenuation Per Octave             "
1360 COLOR 7,0
1370 RETURN
1380 '
1390 '.....18db diagram
1400 COLOR 0,7:C=14
1410 LOCATE ,C:PRINT "       Series                     Parallel           "
1420 LOCATE ,C:PRINT "  здддбддL6ддбддд©          здддбддL3ддбддL4дд©      "
1430 LOCATE ,C:PRINT "  Ё  C7     C5  Zo(LF)╞    ╞Zo  Ё     C3     Zo(LF)╞ "
1440 LOCATE ,C:PRINT " ╞Zo  цддддддеддд╢          юдбдЁддддддадддддды      "
1450 LOCATE ,C:PRINT "  Ё  L7     L4  Zo(HF)╞       Ё юддC4ддбддC5дд©      "
1460 LOCATE ,C:PRINT "  юдддаддC6ддаддды            Ё       L5     Zo(HF)╞ "
1470 LOCATE ,C:PRINT " LF=Tweeter  HF=Woofer        юддддддддадддддды      "
1480 LOCATE ,C:PRINT "            18 db Attenuation Per Octave             "
1490 COLOR 7,0
1500 RETURN
1510 '
1520 '.....preface
1530 PRINT "   Frequency dividing networks are of two two types, the filter t";
1540 PRINT "ype (which has"
1550 PRINT " only approximately constant input impedance) and the constant-re";
1560 PRINT "sistance type."
1570 PRINT "   With either type, the Crossover Frequency is the frequency at ";
1580 PRINT "which the     "
1590 PRINT " power delivered to the two loads is equal. This occurs with an a";
1600 PRINT "ttenuation of "
1610 PRINT " 3 db for each load in an ideal dividing network having no loss. "
1620 RETURN
1630 '
1640 '.....attentuation selection
1650 PRINT " The nominal attenuation per octave beyond the crossover frequenc";
1660 PRINT "y may be:     "
1670 PRINT "   6 db: available with constant-resistance, but attenuation not ";
1680 PRINT "sufficiently  "
1690 PRINT " rapid for general use with loudspeaker dividing networks.       "
1700 PRINT "   12 db: available with either type, and very suitable for gener";
1710 PRINT "al use with   "
1720 PRINT " loudspeaker dividing networks. This is a very popular arrangemen";
1730 PRINT "t. This       "
1740 PRINT " program shows the constant-resistance type.                     "
1750 PRINT "   18 db: available with filter type. This is the maximum rate of";
1760 PRINT " attenuation  "
1770 PRINT " normally used with loudspeaker dividing networks.               "
1780 RETURN
1790 '
1800 '.....end
1810 GOSUB 1830:GOTO 90
1820 '
1830 '.....PRT
1840 KEY OFF:GOSUB 1910:LOCATE 25,5:COLOR 0,2
1850 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1860 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1870 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1870 ELSE GOSUB 1910
1880 IF Z$="3"THEN RETURN
1890 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1900 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1840
1910 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

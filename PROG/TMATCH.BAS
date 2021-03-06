1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'TMATCH - T Match for dipoles - 17 NOV 96 rev. 15 MZR 2007
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,1
50 F$="####.###"
60 M$="###.#"
70 U$="####.##"
80 UL$=STRING$(80,205)
90 '
100 COLOR 15,2
110 PRINT " T MATCH - DIPOLE to 600 � LINE";
120 PRINT TAB(57);;"by George Murphy VE3ERP ";
130 COLOR 1,0:PRINT STRING$(80,223);
140 COLOR 7,0
150 LOCATE 3:GOSUB 470  'diagram
160 '
170 '.....start
175 PRINT
180 COLOR 0,7:LOCATE ,22
190 PRINT " Press 1 to continue or 0 to EXIT....";
200 COLOR 7,0
210 Z$=INKEY$:IF Z$=""THEN 210
220 IF Z$="0"THEN CLS:RUN EX$
230 IF Z$="1"THEN 250
240 GOTO 210
250 CLS:GOSUB 470:LN=CSRLIN
260 INPUT " ENTER: Frequency of operation (MHz)";F
270 IF F=0 THEN 250
280 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
290 PRINT "        Frequency of operation................�=";
300 PRINT USING F$;F;:PRINT " MHz (";USING M$;300/F;:PRINT " m.)"
310 L=468/F
320 PRINT "        Length of antenna.....................L=";
330 PRINT USING U$;L*3.048000E-01;:PRINT " m.  =";USING U$;L;:PRINT " ft."
340 A=1.805000E+02/F
350 PRINT "        Length of matching section............A=";
360 PRINT USING U$;A*3.048000E-01;:PRINT " m.  =";USING U$;A;:PRINT " ft."
370 B=114/F
380 PRINT "        Element spacing.......................B=";
390 PRINT USING U$;B*2.539999;:PRINT " cm. =";USING U$;B;:PRINT " in."
400 PRINT
410 GOSUB 670   'notes
420 GOSUB 810   'hardcopy
430 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE 1
440 GOTO 100
450 END
460 '
470 '.....diagram
480 COLOR 0,7:X=12
490 X$=STRING$(5,196)
500 LOCATE ,X:PRINT "             T MATCH - DIPOLE to 600 � LINE              "
510 LOCATE ,X:PRINT "    ����������������������� L ��������������������į�    "
520 LOCATE ,X:PRINT "    �           ����������� A ��������į�           �    "
530 LOCATE ,X:PRINT "    �           �                       �           �    "
540 LOCATE ,X:PRINT " - -�������������������������������������������������- - "
550 LOCATE ,X:PRINT "               B���� A/2 �į���� A/2 �į�B               "
560 LOCATE ,X:PRINT "          � � ����������������������������� � �          "
570 LOCATE ,X:PRINT "                           �͵                           "
580 LOCATE ,X:PRINT "                           �͵��� 600 � line             "
590 LOCATE ,X:PRINT "                           �͵                           "
600 LOCATE ,X:PRINT "                           �͵                           "
610 LOCATE ,X:PRINT "                          �������� Transmatch            "
620 LOCATE ,X:PRINT "                          �����                          "
630 LOCATE ,X:PRINT "                                                         "
640 COLOR 7,0
650 RETURN
660 '
670 '.....notes
680 T=8
690 PRINT TAB(T);
700 PRINT "These dimensions apply for wire antennas with the matching section"
710 PRINT TAB(T);
720 PRINT "made of the same size wire. With an antenna element of different"
730 PRINT TAB(T);
740 PRINT "impedance, or for a matching a line having a Zo other than 600 �,"
750 PRINT TAB(T);
760 PRINT "the matching section dimensions can be determined experimentally."
770 PRINT TAB(T);
780 PRINT "  (The ARRL ANTENNA BOOK, 17th Edition, pages 26-18 and 26-19)";
790 RETURN
800 '
810 'PRT
820 KEY OFF:GOSUB 890:LOCATE 25,5:COLOR 0,2
830 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
840 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
850 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 850 ELSE GOSUB 890
860 IF Z$="3"THEN RETURN
870 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
880 IF Z$="2"THEN LPRINT CHR$(12) ELSE 820
890 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

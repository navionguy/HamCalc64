1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$,U,UH,LX
10 'CENTLOAD - Short Centre Loaded Dipole -  29 DEC 96 rev. 20 AUG 07
20 'ref The ARRL ANTENNA BOOK, 17th Edition, page 6-7, Fig 11, by W0SVM
30 IF EX$=""THEN EX$="EXIT"
40 PROG$="centload"
50 
60 CLS:KEY OFF
70 COLOR 7,0,1
80 DIM GA(40)    'AWG sizes
90 PI=3.141592
100 UL$=STRING$(80,205)
110 U$="#####.##"
120 V$="#####.###"
130 F$="###.###"
140 G$="####.###"
150 '
160 '.....AWG sizes
170 K=(4.600000E-01/4.999999E-03)^(1/39)  'increment multiplier
180  FOR Z=1 TO 40
190   N=Z+3
200   GA(Z)=4.600000E-01/K^N
210  NEXT Z
220 '
230 '.....start
240 CLS
250 COLOR 15,2
260 PRINT " SHORT CENTRE LOADED DIPOLE";
270 PRINT TAB(57);"by George Murphy VE3ERP ";
280 COLOR 1,0:PRINT STRING$(80,223);
290 COLOR 7,0
300 LOCATE 3:GOSUB 680   'diagram
310 PRINT UL$;
320 GOSUB 2180   'notes
330 PRINT UL$;
340 PRINT " Press number in < > to:";
350 PRINT UL$;
360 PRINT "   < 1 > RUN program"
370 IF UH=0 THEN 410
380 PRINT "   < 2 > Design an AIR-CORE coil 'B' long for this antenna"
390 PRINT "   < 3 > SELECT a commercial coil 'B' long for this antenna"
400 PRINT UL$;
410 PRINT "   < 0 > EXIT"
420 Z$=INKEY$
430 IF Z$=""THEN 420
440 IF Z$="0"THEN CLS:RUN EX$
450 IF Z$="1"AND UH<>0 THEN UH=0:GOTO 230
460 IF Z$="1"THEN M=0:UH=0:F=0:D=0:LG=0:BB=0:W=0:DIA=0:GOTO 520   'variables
470 IF Z$="2"THEN CLS:CHAIN"coildsgn"
480 IF Z$="3"THEN U=UH:CLS:CHAIN "aircore"
490 IF F=0 THEN 500
500 GOTO 420
510 '
520 '.....preamble
530 VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
540 PRINT UL$;
550 IF BB THEN 640
560 PRINT " Press number in < > to choose standard units of measure:"
570 PRINT UL$;
580 PRINT "  < 1 >  Metric"
590 PRINT "  < 2 >  U.S.A./Imperial"
600 Z$=INKEY$
610 IF Z$="1"THEN UM=3.048000E-01:UM$=" m.":GOTO 650
620 IF Z$="2"THEN UM=1:UM$=" ft.":GOTO 650
630 GOTO 600
640 IF M THEN STOP
650 VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
660 GOTO 810
670 '
680 '.....diagram
690 T=11    'tab
700 COLOR 0,7
710 LOCATE,T:PRINT"                                                           "
720 LOCATE,T:PRINT"     낟컴컴컴컴컴컴컴컴컴컴� A 컴컴컴컴컴컴컴컴컴컴캄�     "
730 LOCATE,T:PRINT"     낟컴컴컴� C 컴컴컴캄낟� B 캄낟컴컴컴� C 컴컴컴캄�     "
740 LOCATE,T:PRINT" � � 邇컴컴컴컴컴컴컴컴컴程程程程粘컴컴컴컴컴컴컴컴컴� � � "
750 LOCATE,T:PRINT"                           � � Loading coil                "
760 LOCATE,T:PRINT"               52-� Coax컴�픔�                             "
770 LOCATE,T:PRINT"                           �                               "
780 COLOR 7,0
790 RETURN
800 '
810 '.....input parameters
820    IF F THEN 850
830 INPUT" ENTER: Centre frequency (in MHz) of the band you want to operate ";F
840 IF F=0 THEN LOCATE CSRLIN-1:GOTO 830
850 SP=300/F*6.560000E-01*UM
860 VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
870 PRINT " For maximum efficiency at";F;;"MHz, dimension A should be at least";
880 PRINT INT(SP*100)/100;RIGHT$(UM$,LEN(UM$)-1)
890 PRINT " Shorter antennas will also work, but at reduced efficiency."
900 PRINT UL$;
910   IF LG THEN 980
920 HW=468/F*UM
930 PRINT " A full size half-wave";F;;"MHz dipole is about";USING G$;HW;
940 PRINT UM$;;"long."
950 PRINT " ENTER: Approximate Length (";UM$;;")..............A= ";:INPUT LG
960 LG=LG/UM
970 IF LG=0 THEN LOCATE CSRLIN-1:GOTO 950
980   IF DIA THEN 1170
990 VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
1000 PRINT " Use the largest diameter wire that is practical for your ";
1010 PRINT "application......."
1020 PRINT " Press letter in < > to select conductor size:"
1030 PRINT UL$;
1040 PRINT "   < a >  Diameter in millimetres"
1050 PRINT "   < b >  Diameter in inches"
1060 PRINT "   < c >  AWG#"
1070 Z$=INKEY$
1080 IF Z$="a"OR Z$="A"THEN WS$="mm":GOTO 1120
1090 IF Z$="b"OR Z$="B"THEN WS$="inches":GOTO 1120
1100 IF Z$="c"OR Z$="C"THEN WS$="AWG#":GOTO 1120
1110 GOTO 1070
1120 PRINT " ENTER: Wire size (";WS$;;")";:INPUT ZD
1130 IF WS$="mm"THEN DIA=ZD/2.539999E+01
1140 IF WS$="inches"THEN DIA=ZD
1150 IF WS$="AWG#"THEN DIA=GA(ZD)
1160 '
1170 '.....display data
1180 VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
1190 COLOR 0,7
1200 IF UM=1 THEN 1210 ELSE 1260
1210 X=LG:GOSUB 2070:LOCATE 4,34:PRINT X$;;" + B "
1220 X=LG/2:GOSUB 2070:LOCATE 5,22:PRINT X$;;" "
1230 LOCATE 5,50:PRINT X$;;" "
1240 GOTO 1300
1250 '
1260 LOCATE 4,34:PRINT USING F$;LG*3.048000E-01;:PRINT ;" m.+B "
1270 LOCATE 5,19:PRINT USING F$;LG/2*3.048000E-01;:PRINT ;" m. "
1280 LOCATE 5,51:PRINT USING F$;LG/2*3.048000E-01;:PRINT ;" m. "
1290 '
1300 COLOR 7,0:LOCATE 11:PRINT STRING$(80,32);
1310 LOCATE CSRLIN-1:PRINT F;;"MHz ";
1320 Y$=WS$
1330 IF WS$="inches"THEN Y$=CHR$(34)
1340 IF WS$="AWG#"THEN Y$=CHR$(34)
1350 Y=DIA:IF Y$="mm"THEN Y=Y*2.539999E+01
1360 PRINT "SHORT CENTRE-LOADED DIPOLE USING";USING "##.###";Y;
1370 PRINT Y$;;" WIRE.";
1380 IF WS$="AWG#"THEN PRINT " ( ";WS$;ZD;;")";ELSE PRINT ;""
1390 PRINT UL$;
1400 W=ZD   'AWG # for chaining to coil designer program
1410 '
1420 '.....calculate and print
1430 F1=10^6/(68*PI^2*F^2)
1440 B=0
1450   F2=LOG((24*(234/F)-B)/DIA-1)
1460   F3=(1-F*B/234)^2-1
1470   F4=234/F-B
1480   F5=LOG((24*LG/2-B)/DIA-1)
1490   F6=((F*LG/2-F*B)/234)^2-1
1500   F7=LG/2-B
1510   L=F1*(F2*F3/F4-F5*F6/F7)     'inductance
1520 IF L<=0 THEN 1840
1530 '.....display inductance
1540 UH=2*L       'coil for dipole
1550 LN=CSRLIN
1560 COLOR 0,7:LOCATE 7,41
1570 PRINT USING F$;UH;:PRINT ;" �H loading coil"
1580 COLOR 7,0:LOCATE LN
1590 PRINT TAB(TB);
1600 PRINT "For maximum efficiency use the largest practicable diameter wire"
1610 PRINT TAB(TB);
1620 PRINT "for both the coil and the dipole legs."
1630 PRINT
1640 PRINT TAB(TB);
1650 PRINT "Coil length should not exceed twice its diameter. Avoid close-"
1660 PRINT TAB(TB);
1670 PRINT "wound coils of enamelled wire. Use either bare wire spaced at its"
1680 PRINT TAB(TB);
1690 PRINT "own diameter, or close-wound insulated wire."
1700 PRINT
1710 PRINT TAB(TB);
1720 PRINT "Connect Coax shield to centre of coil and offset centre conductor"
1730 PRINT TAB(TB);
1740 PRINT "as many turns as required for best SWR."
1750 PRINT
1760 PRINT TAB(TB);
1770 PRINT "(ref. The ARRL ANTENNA BOOK, 17th edition, page 6-7)";
1780 GOSUB 2320   'hardcopy
1790 'CLS:GOTO 230
1800 VIEW PRINT 13 TO 24:CLS:VIEW PRINT:LOCATE 14
1810 GOTO 340
1820 END
1830 '
1840 BEEP:VIEW PRINT 10 TO 24:CLS:VIEW PRINT:LOCATE 10
1850 PRINT UL$;
1860 PRINT " Available space is ample for a full size half-wave";
1870 PRINT F;;"MHz Dipole, which is"
1880 WL=492/F          '1/2 wavelength in free space, in feet
1890 RA=WL*12/DIA      'ratio of 1/2 wavelength to wire diameter
1900 K=9.100000E-01
1910 IF RA>10 THEN K=9.200000E-01
1920 IF RA>12 THEN K=9.300000E-01
1930 IF RA>15 THEN K=9.399999E-01
1940 IF RA>22 THEN K=9.499999E-01
1950 IF RA>50 THEN K=9.599999E-01
1960 IF RA>200 THEN K=9.700000E-01
1970 IF RA>2000 THEN K=9.800000E-01
1980 HW=492*K/F        'length of half wave flat top, in feet
1990 HW=9.499999E-01*HW/K       'end effect correction for F<30MHz
2000 IF UM=1 THEN X=HW:GOSUB 2070:PRINT X$;;" long";:GOTO 2020
2010 IF UM<>1 THEN X=HW*3.048000E-01:X=INT(X*100+5.000000E-01)/100:PRINT X;;"m. long"
2020 PRINT UL$;
2030 GOSUB 2320
2040 GOTO 230
2050 END
2060 '
2070 '.....convert to feet and inches
2080 FT=INT(X)
2090 IN=(X-FT)*12
2100 IF IN>=12 THEN IN=IN-12:FT=FT+1
2110 FT$=STR$(FT)
2120 IF LEN(FT$)<3 THEN FT$=" "+FT$:GOTO 2120
2130 IN$=STR$(IN)
2140 IF INT(IN)>9 THEN IN$=LEFT$(IN$,5)ELSE IN$=LEFT$(IN$,4)
2150 X$=FT$+"'-"+IN$+CHR$(34)
2160 RETURN
2170 '
2180 '.....text block
2190 TB=7      'tab for text
2200 PRINT TAB(TB);
2210 PRINT "  This antenna is a dipole with lumped-constant loading. At modest"
2220 PRINT TAB(TB);
2230 PRINT "heights (10-30 metres) it will work well if its physical length is"
2240 PRINT TAB(TB);
2250 PRINT "longer than 0.2 x wavelength. This threshold length is calculated"
2260 PRINT TAB(TB);
2270 PRINT "by the program. Shorter lengths will be less efficient. The coil"
2280 PRINT TAB(TB);
2290 PRINT "length should not exceed twice its diameter for acceptable Q."
2300 RETURN
2310 '
2320 'PRT
2330 KEY OFF:GOSUB 2400:LOCATE 25,5:COLOR 0,2
2340 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2350 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2360 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2360 ELSE GOSUB 2400
2370 IF Z$="3"THEN RETURN
2380 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2390 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2330
2400 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

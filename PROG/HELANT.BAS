1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'HELANT - Helical Antenna-VHF/UHF - 15 APR 2000, rev.
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 COLOR 7,0,1
50 LF=1/LOG(10) 'factor to convert log to base 10
60 PI=4*ATN(1)  '3.141593
70 V=2.997925E+05 'velocity of light in m,/sec.
80 UL$=STRING$(80,205)
90 '
100 '.....title page
110 VIEW PRINT:CLS
120 COLOR 15,2
130 PRINT " HELICAL ANTENNA - VHF/UHF";TAB(57);"by George Murphy VE3ERP ";
140 COLOR 1,0:PRINT STRING$(80,223);
150 COLOR 7,0
160 '
170 '.....diagram
180 COLOR 0,7:J=55
190 LOCATE ,J:PRINT "                         "
200 LOCATE ,J:PRINT "        ³®ÄDÄÄ¯³         "
210 LOCATE ,J:PRINT "        ³   ÍÍÍ»         "
220 LOCATE ,J:PRINT "      Ú¯ÉÍÍÍÍÍÍ¼         "
230 LOCATE ,J:PRINT "      S ÈÍÍÍÍÍÍ»         "
240 LOCATE ,J:PRINT "      À¯ÉÍÍÍÍÍÍ¼<Ä helix "
250 LOCATE ,J:PRINT "        ÈÍÍÍÍÍÍ»         "
260 LOCATE ,J:PRINT "        ÉÍÍÍÍÍÍ¼         "
270 LOCATE ,J:PRINT "        ÈÍÍÍ¸®ÄÄÄÄÄÄÄÄÄ¿ "
280 LOCATE ,J:PRINT " feed wireÄ>³          W "
290 LOCATE ,J:PRINT " ÚÄ>ÍÍÍÍÍÍÍ¸³ÕÍÍÍÍÍÍÍ ®Ù "
300 LOCATE ,J:PRINT " ³ÚÄÅÄÄÄÄÄ>³³³      ³    "
310 LOCATE ,J:PRINT " ³³ ³®ÄÄÄÄÄ G ÄÄÄÄÄ¯³    "
320 LOCATE ,J:PRINT " ³ÀÄ coaxial cable       "
330 LOCATE ,J:PRINT " ÀÄÄ ground plane        "
340 LOCATE ,J:PRINT "                         "
350 COLOR 7,0
360 '
370 LOCATE 3
380 PRINT " D = diameter of helix conductor centreline."
390 PRINT " C = circumference of helix conductor centreline."
400 PRINT " S = axial length of one turn."
410 PRINT " G = ground plane diameter (or side length)."
420 PRINT " W = ground plane to first turn distance."
430 PRINT " L = total length of conductor."
440 LN=CSRLIN-1
450 GOTO 510
460 '
470 '.....clear bottom of screen
480 VIEW PRINT 22 TO 24:CLS:VIEW PRINT:LOCATE 22
490 RETURN
500 '
510 '.....start
520 LOCATE 22,10
530 PRINT " Ref: ARRL 'ANTENNA BOOK', 18th edition, pp. 19-22 to 19-24"
540 COLOR 0,7:LOCATE ,21:PRINT " Press 1 to continue or 0 to EXIT . . ."
550 Z$=INKEY$:IF Z$=""THEN 550
560 IF Z$="0"THEN COLOR 7,0:CLS:RUN EX$
570 IF Z$="1"THEN COLOR 7,0:GOTO 600
580 GOTO 550
590 '
600 '.....frequency & wavelength
610 GOSUB 470
620 COLOR 0,7:INPUT " ENTER: Frequency in MHz ";F:COLOR 7,0
630 LOCATE LN+1
640 PRINT USING " Frequency............................ #####.### MHz";F
650 GOSUB 470
660 LOCATE LN+2:WL=V/F/10^3
670 PRINT USING " Wavelength (wL)...................... #####.### m. ";WL
680 GOSUB 470
690 '
700 '.....number of turns
710 IF N THEN 790
720 COLOR 0,7:INPUT " ENTER: Number of turns ";N:COLOR 7,0
730 LOCATE LN+3
740 PRINT USING " Number of turns...................... #####.#";N
750 LOCATE LN+4:AR=(2*N+1)/(2*N)
760 PRINT USING " Axial ratio.......................... #####.###:1";AR
770 GOSUB 470
780 '
790 '.....diameter & circumference
800   IF D THEN 920
810 X=7.500000E-01*WL/PI*100:Y=1.330000*WL/PI*100:GOSUB 470
820 PRINT USING " Select a helix diameter between ###.# and ###.# cm. ";X,Y
830 COLOR 0,7:INPUT " ENTER: helix diameter in centimeters ";D:COLOR 7,0
840 GOSUB 470
850 LOCATE LN+5:X=D/WL/100:Y=D:Z=Y/2.539999
860 PRINT USING " D =........ ####.### wL= ####.### cm.= ####.### in.";X,Y,Z
870 GOSUB 470
880 LOCATE LN+6:C=PI*D:X=C/WL/100:Y=C:Z=Y/2.539999
890 PRINT USING " C =........ ####.### wL= ####.### cm.= ####.### in.";X,Y,Z
900 GOSUB 470
910 '
920 '.....spacing
930 IF S THEN 1020
940 X=C*2.125999E-01:Y=C*2.867000E-01:GOSUB 470
950 Z$=" Select axial length of one turn between ###.# and ###.# cm. "
960 PRINT USING Z$;X,Y
970 COLOR 0,7:INPUT " ENTER: axial length of one turn in cm. ";S:COLOR 7,0
980 LOCATE LN+7:X=S/WL/100:Y=S:Z=Y/2.539999
990 PRINT USING " S =........ ####.### wL= ####.### cm.= ####.### in.";X,Y,Z
1000 GOSUB 470
1010 '
1020 '.....ground plane
1030 IF G THEN 1120
1040 X=WL*8.000000E-01*100:Y=WL*1.100000*100:GOSUB 470
1050 Z$=" Select ground plane diameter or side between ###.# and ###.# cm. "
1060 PRINT USING Z$;X,Y
1070 COLOR 0,7:INPUT " ENTER: ground plane diameter or side in cm. ";G:COLOR 7,0
1080 LOCATE LN+8:X=G/WL/100:Y=G:Z=Y/2.539999
1090 PRINT USING " G =........ ####.### wL= ####.### cm.= ####.### in.";X,Y,Z
1100 GOSUB 470
1110 '
1120 '.....ground plane to first turn
1130 IF W THEN 1220
1140 X=WL*1.199999E-01*100:Y=WL*1.299999E-01*100:GOSUB 470
1150 Z$=" Select distance W to first turn between ###.# and ###.# cm. "
1160 PRINT USING Z$;X,Y
1170 COLOR 0,7:INPUT " ENTER: distance W in cm. ";W:COLOR 7,0
1180 LOCATE LN+9:X=W/WL/100:Y=W:Z=W/2.539999
1190 PRINT USING " W =........ ####.### wL= ####.### cm.= ####.### in.";X,Y,Z
1200 GOSUB 470
1210 '
1220 '.....length
1230 LOCATE LN+10:Y=SQR((PI*D)^2+S^2)*N:Z=Y/2.539999:X=Y/WL/100
1240 PRINT USING " L =........ ####.### wL= ####.### cm.= ####.### in.";X,Y,Z
1250 '
1260 '.....re-define variables
1270 CC=C/WL/100  'circumf. in wL
1280 SS=S/WL/100  'spacing in wL
1290 LOCATE LN+11
1300 '
1310 '.....gain
1320 DB=1.180000E+01+LF*(10*LOG(CC^2*N*SS))
1330 PRINT USING " Gain................................. #####.# dBi";DB
1340 '
1350 '.....impedance
1360 ZO=140*CC
1370 PRINT USING " Input impedance...................... #####.# ê";ZO
1380 '
1390 '.....beam width
1400 BW=52/(CC*SQR(N*SS) )  'half power
1410 NUL=115/(CC*SQR(N*SS)) 'to first nuls
1420 PRINT USING " Beamwidth: Half power (HPBW)= ###.#ø";BW;
1430 PRINT USING ", To first nuls= ###.#ø";NUL
1440 GOSUB 1570
1450 '
1460 LOCATE 22
1470 COLOR 0,7:PRINT " Another N,D,S,G,W or (q)uit?   (n/d/s/g/w/q) ";:COLOR 7,0
1480 Z$=INKEY$
1490 IF Z$="n"THEN N=0:GOTO 700
1500 IF Z$="d"THEN D=0:GOTO 700
1510 IF Z$="s"THEN S=0:GOTO 700
1520 IF Z$="g"THEN G=0:GOTO 700
1530 IF Z$="w"THEN W=0:GOTO 700
1540 IF Z$="q"THEN N=0:D=0:S=0:G=0:W=0:GOTO 100
1550 GOTO 1480
1560 '
1570 'PRT
1580 KEY OFF:GOSUB 1650:LOCATE 25,5:COLOR 0,2
1590 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1600 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1610 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1610 ELSE GOSUB 1650
1620 IF Z$="3"THEN RETURN
1630 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1640 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1580
1650 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

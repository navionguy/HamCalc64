1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON Z$,FLAG
10 'FILTER - 16 MAY 86  rev. 12 MAR 2000
20 CLEAR:CLS
30 
40 DIM Q(19)
50 U1$=STRING$(29,205)
60 U2$=STRING$(23,205)
70 U3$=STRING$(80,205)
80 E$=STRING$(25,32)
90 DIM T$(20,2)
100 T$=" FILTER CALCULATOR "
110 COLOR 12,5,5
120 PRINT U3$;
130 COLOR 11,5:LOCATE CSRLIN-1,(80-LEN(T$))/2:PRINT T$
140 COLOR 5,0:PRINT STRING$(80,223);
150 COLOR 7,0
160 PRINT " Press number in < > for:"
170 PRINT U3$;
180 PRINT "  < 1 >  Colour Decimired / Wratten filters (camera)"
190 PRINT "  < 2 >  Colour Compensating filters (enlarger)"
200 PRINT "  < 3 >  Filter Factor effect on exposure"
210 PRINT U3$;
220 PRINT "  < 0 >  EXIT"
230 Z$=INKEY$
240 IF Z$="0"THEN RUN "fotomenu"
250 IF Z$="1"THEN 300
260 IF Z$="2"THEN 1500
270 IF Z$="3"THEN FLAG=1:CHAIN "exposure"
280 GOTO 230
290 '
300 '.....camera filters
310 CLS:RESTORE
320 DATA 240.86,131.852,112.85,81.853,52.815,42.814,35.813,27.812,18.811,9.81
330 DATA -10.82,-21.821,-32.822,-45.823,-56.804,-81.803,-112.802,-131.801
340 DATA -240.78
350 FOR Z=1 TO 19:READ Q(Z):NEXT Z
360 DATA A,B,C,D,EF
370 FOR Z=1 TO 5:READ A$(Z):NEXT Z
380 '
390 COLOR 15,5
400 LOCATE 1,39:PRINT " Typical colour temperatures:             ";
410 COLOR 7,0
420 LOCATE 2,39:PRINT STRING$(41,205)
430 DATA 6,1500K  Candle flame
440 DATA 6,2600K  60 watt house lamp
450 DATA 6,2865K  100 watt house lamp
460 DATA 6,3150K  500 watt projector lamp
470 DATA 6,3200K  Photoflood lamp
480 DATA 6,3200K  Warm White fluorescent tube
490 DATA 6,3400K  500w. #1/2/4 Photoflood
500 DATA 6,4500K  Cool White fluorescent tube
510 DATA 6,4800K  Daylight (blue) floodlamp
520 DATA 7,5400K  Noon sunlight
530 DATA 3,5800K  Noon sun & haze
540 DATA 3,6000K  Electronic flash
550 DATA 3,6500K  Haze/mountains/aerial photography
560 DATA 3,7250K  Overcast sky or open shade
570 DATA 3,20000K Clear blue sky
580 FOR Z=1 TO 14:READ T$(Z,1):READ T$(Z,2):NEXT Z
590 FOR Z=1 TO 14
600 IF Z=10 THEN PRINT
610 GOSUB 740:COLOR VAL(T$(Z,1)):PRINT T$(Z,2)
620 IF Z=10 THEN PRINT
630 NEXT Z
640 COLOR 7,0
650 PRINT TAB(39);STRING$(41,205)
660 COLOR 14,1
670 GOSUB 740
680 PRINT" NOTE:  Most daylight film emulsions are "
690 GOSUB 740
700 PRINT"        balanced for about 5500-5600� K. "
710 COLOR 7,0
720 GOTO 760
730 '
740 LOCATE CSRLIN,39:RETURN
750 '
760 '.....inputs
770 COLOR 7,0
780 LOCATE 1
790 INPUT " ENTER: Light Source....�K.";T
800 IF T=0 THEN LOCATE CSRLIN-1:PRINT STRING$(38,32):LOCATE CSRLIN-1:GOTO 790
810 Z=T:GOSUB 1440:V=Z
820 INPUT " ENTER: Film Emulsion...�K.";U
830 IF U=0 THEN LOCATE CSRLIN-1:PRINT STRING$(38,32):LOCATE CSRLIN-1:GOTO 820
840 Z=U:GOSUB 1440:W=Z
850 IF T*U=0 THEN 760
860 COLOR 7,0
870 FOR Z=1 TO 2:LOCATE Z:PRINT STRING$(38,32):NEXT Z:LOCATE 1
880 X=W-V:CC$="+#####":DD$="######"
890 '
900 '.....screen print
910 COLOR 15,5
920 LOCATE CSRLIN,16:PRINT "  MIRED";TAB(26);;"�K  "
930 COLOR 7,0
940 PRINT U1$
950 PRINT " Source colour";:PRINT TAB(16);USING CC$;V;
960 PRINT TAB(23);USING DD$;T;:PRINT "�"
970 PRINT " Sought colour";:PRINT TAB(16);USING CC$;W;
980 COLOR 0,7
990 LOCATE CSRLIN,23:PRINT USING DD$;U;:PRINT "�"
1000 COLOR 7,0
1010 PRINT " Colour Shift ";:PRINT TAB(16);USING CC$;X;
1020 PRINT TAB(23);USING DD$;ABS(T-U);:PRINT "�"
1030 PRINT U1$
1040 '
1050 '.....filter check
1060 IF ZZ=0 AND ABS(X)<4.750000 THEN 1070 ELSE FIL=0:GOTO 1090
1070 PRINT " No filter needed";:PRINT U1$:ZZ=ABS(T-U)/U*100:XX=ABS(T-U)
1080 FIL=1:GOTO 1270
1090 GOSUB 1370
1100 X=SGN(Z)*INT(ABS(Z))
1110 Y=CINT(1.000000E+02*ABS(Z-X))
1120 AA=(ABS(Z)*100-INT(ABS(Z)*100))*10
1130 BB$=A$(AA)
1140 AA=SGN(X)
1150 Z=1.000000E+06/(V+X):GOSUB 1480
1160 IF SGN(X)=-1 THEN COLOR 15,3 ELSE COLOR 15,4
1170 PRINT " Filter #";Y;BB$;:PRINT TAB(16);USING CC$;X;:PRINT "�"
1180 COLOR 7,0
1190 PRINT TAB(17);"shifts";:PRINT TAB(23);USING DD$;T;:PRINT "�"
1200 PRINT TAB(17);"to";:PRINT TAB(23);USING DD$;Z;:PRINT "�"
1210 PRINT U1$
1220 T=Z
1230 XX=ABS(T-U)
1240 ZZ=ABS((U-Z)/U)*1.000000E+02:GOSUB 1460
1250 IF ZZ>=3 THEN Z=T:GOSUB 1440:V=Z:X=W-V:IF SGN(X)=AA THEN 1050
1260 '
1270 '.....calculate % error
1280 PRINT " Off-target by";
1290 PRINT USING "###.#";ZZ;:PRINT "%";:PRINT TAB(23);USING DD$;XX;:PRINT "�"
1300 IF FIL=1 THEN 1330
1310 LOCATE CSRLIN-3,23:COLOR 0,7
1320 PRINT USING DD$;Z;:PRINT "�"
1330 COLOR 7,0
1340 GOSUB 2190
1350 GOTO 10
1360 '
1370 '.....find filter
1380 FOR I=1 TO 18
1390 L=(INT((Q(I))+INT(Q(I+1)))/2)
1400 IF X>L THEN Z=Q(I):GOTO 1420
1410 NEXT I:Z=Q(19)
1420 RETURN
1430 '
1440 Z=1.000000E+06/Z:GOSUB 1480
1450 '
1460 Z=INT((100*Z+5)/10)/10:RETURN
1470 '
1480 Z=INT((10*Z+5)/10):RETURN
1490 '
1500 '.....colour compensating filters
1510 CLS:COLOR 15,5:PRINT " COLOUR COMPENSATING FILTERS for ENLARGERS "
1520 COLOR 7,0
1530 PRINT U3$;
1540 PRINT " Press number in < > to:";
1550 PRINT U3$;
1560 PRINT "  < 1 >  Correct filter pack for new paper batch"
1570 PRINT "  < 2 >  Change filtration in current filter pack"
1580 Z$=INKEY$:IF Z$="c"THEN 300
1590 IF Z$="1"THEN 1630
1600 IF Z$="2"THEN 2130
1610 GOTO 1580
1620 '
1630 '.....current filter pack
1640 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1650 COLOR 0,7:LOCATE 14,24:PRINT " ENTER VALUE OF INDICATED FILTER "
1660 COLOR 7,0:LOCATE 3
1670 PRINT " Current Filter Pack:";
1680 PRINT U2$
1690 GOSUB 1950:GOSUB 2040
1700 U=T:W=V:Y=X
1710 IF AA THEN AA=0:GOTO 1770
1720 PRINT " Last Batch Correction:";
1730 PRINT U2$
1740 GOSUB 1950:GOSUB 2040
1750 U=U-T:W=W-V:Y=Y-X
1760 '
1770 '.....new color correction
1780 PRINT " New Correction:";
1790 PRINT U2$
1800 GOSUB 1950:GOSUB 2040
1810 U=U+T:W=W+V:Y=Y+X
1820 Z=U:IF W<Z THEN Z=W
1830 IF Y<Z THEN Z=Y
1840 T=U-Z:V=W-Z:X=Y-Z
1850 '
1860 PRINT " New Filter Pack:";
1870 LOCATE CSRLIN+4:GOSUB 2040
1880 IF T+U+X THEN 1910
1890 COLOR 0,7:LOCATE CSRLIN-2,18:PRINT " NO FILTRATION REQUIRED "
1900 COLOR 7,0
1910 LOCATE 14:PRINT STRING$(80,32);
1920 GOSUB 2190
1930 GOTO 10
1940 '
1950 '.....inputs
1960 C=11:GOSUB 2160
1970 INPUT "Cyan ......";T
1980 C=13:GOSUB 2160
1990 INPUT "Magenta ...";V
2000 C=14:GOSUB 2160
2010 INPUT "Yellow ....";X
2020 RETURN
2030 '
2040 '.....screen print
2050 P$="#####":W$=STRING$(6,32)
2060 FOR Z=CSRLIN-3 TO CSRLIN-1:LOCATE Z:PRINT E$:NEXT Z:LOCATE CSRLIN-5,24
2070 IF T THEN COLOR 11,0:PRINT USING P$;T;:PRINT "C"; ELSE PRINT W$;
2080 IF V THEN COLOR 13,0:PRINT USING P$;V;:PRINT "M"; ELSE PRINT W$;
2090 IF X THEN COLOR 14,0:PRINT USING P$;X;:PRINT "Y"; ELSE PRINT W$
2100 COLOR 7,0
2110 PRINT STRING$(41,205):RETURN
2120 '
2130 '.....revise filter pack
2140 AA=1:GOTO 1630
2150 '
2160 '.....print color bar
2170 COLOR C,0:PRINT " ";STRING$(6,219);:COLOR 7,0:PRINT " ";
2180 RETURN
2190 '
2200 'PRT
2210 KEY OFF:GOSUB 2280:LOCATE 25,5:COLOR 0,2
2220 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2230 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2240 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2240 ELSE GOSUB 2280
2250 IF Z$="3"THEN RETURN
2260 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2270 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2210
2280 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

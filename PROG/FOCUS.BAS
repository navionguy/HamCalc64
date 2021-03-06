1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'FOCUS - 21 MAR 94 rev. 12 MAR 2000
20 CLEAR:CLS:KEY OFF:SCREEN 0
30 COLOR 7,0,3
40 DIM A(12)    'standard apertures
50 E$=STRING$(80,32)
60 UL$=STRING$(80,205)
70 T$=" FOCUSING CALCULATOR "
80 '
90 DATA 1.4,2,2.8,4,5.6,8,11,16,22,32,45,64
100 FOR Z=1 TO 12:READ A(Z):NEXT Z
110 '
120 '.....start
130 COLOR 0,7
140 PRINT UL$;
150 LOCATE CSRLIN-1,INT((80-LEN(T$))/2):PRINT T$
160 COLOR 3,0:PRINT STRING$(80,223);
170 COLOR 7,0
180 PRINT " Press number in < > to:";
190 PRINT UL$;
200 PRINT "   < 1 >  Calculate Hyperfocal Distance"
210 PRINT "   < 2 >  Calculate Depth-of-Field"
220 PRINT "   < 3 >  Calculate Point-of-Focus"
230 PRINT UL$;
240 PRINT "   < 0 >  EXIT"
250 Z$=INKEY$
260 IF Z$="0"THEN CLS:RUN "fotomenu"
270 IF Z$="1"THEN 380
280 IF Z$="2"THEN 1260
290 IF Z$="3"THEN 790
300 GOTO 250
310 '
320 '.....format aperture display
330 IF A<100 THEN A$="##"
340 IF A<10 THEN A$="#"
350 IF A<>INT(A)THEN A$=A$+".#"
360 RETURN
370 '
380 '.....hyperfocal distance
390 CLS
400 COLOR 0,7:PRINT UL$;
410 COLOR 3,0:PRINT STRING$(80,223);
420 T$=" HYPERFOCAL DISTANCE "
430 COLOR 0,7:LOCATE 1,INT((80-LEN(T$))\2):PRINT T$:LOCATE 3
440 COLOR 7,0
450 INPUT " ENTER: Focal length of lens (mm) ";FM
460 IF FM=0 THEN LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1:GOTO 450
470 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
480 T$=STR$(FM)+"mm LENS "
490 COLOR 15,4
500 LOCATE CSRLIN-1,INT((80-LEN(T$))\2):PRINT T$
510 COLOR 7,0
520 PRINT UL$;
530 T=9
540 PRINT TAB(T);
550 PRINT "The Hyperfocal Distance is the distance from the camera to the"
560 PRINT TAB(T);
570 PRINT "nearest object in focus when the lens is focused at infinity."
580 PRINT UL$;
590 FI=FM/2.539999E+01
600 C=2.000000E-03   'circle of confusion in inches
610   FOR Z=1 TO 12
620 A=A(Z):GOSUB 320         'format aperture
630 D=FI^2*(1/C)/(A*12)      'hyperfocal distance
640 PRINT TAB(T);;"at �/";USING A$;A(Z);
650 D$=CHR$(34)
660 PRINT TAB(T+10);;"........";D$;;"........";D$;;"........";USING "###,###.#";D;
670 PRINT " ft.  (";USING "####.#";D*3.048000E-01;
680 PRINT " m.)"
690   NEXT Z
700 PRINT UL$;
710 PRINT TAB(T);
720 PRINT "All objects between the hyperfocal distance and infinity will be"
730 PRINT TAB(T);;"in acceptably sharp focus."
740 PRINT UL$;
750 LOCATE 7,T+10:PRINT "the Hyperfocal Distance is"
760 GOSUB 1950
770 GOTO 10
780 '
790 '.....point-of-focus
800 CLS
810 COLOR 0,7:PRINT UL$;
820 T$=" POINT-OF-FOCUS "
830 COLOR 0,7:LOCATE 1,INT((80-LEN(T$))\2):PRINT T$
840 COLOR 3,0:PRINT STRING$(80,223);
850 COLOR 7,0
860 '
870 PRINT " Do you want distances in (m)etres, (f)eet or (i)nches?    (m/f/i)"
880 Z$=INKEY$
890 IF Z$="m"OR Z$="M"THEN X$=" m. ":X=1000:GOTO 930
900 IF Z$="f"OR Z$="F"THEN X$=" ft.":X=3.047999E+02:GOTO 930
910 IF Z$="i"OR Z$="I"THEN X$=" in.":X=2.539999E+01:GOTO 930
920 GOTO 880
930 LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1
940 '
950 U$="###.##"
960 PRINT " ENTER: Distance (";X$;;") to NEAREST point to be in focus ....";
970 INPUT DA
980 IF DA=0 THEN LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1:GOTO 960
990 LOCATE CSRLIN-1:PRINT SPC(7);:LOCATE CSRLIN,61:PRINT USING U$;DA;
1000 PRINT STRING$(6,32)
1010 DA=DA*X
1020 '
1030 PRINT " ENTER: Distance (";X$;;") to FARTHEST point to be in focus ...";
1040 INPUT DB
1050 IF DB=0 THEN LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1:GOTO 1030
1060 LOCATE CSRLIN-1:PRINT SPC(7);:LOCATE CSRLIN,61:PRINT USING U$;DB;
1070 PRINT STRING$(6,32)
1080 DB=DB*X
1090 '
1100 D=(2*DA*DB/(DA+DB))      'distance in mm
1110 IF X$=" m. "THEN DD=D/10^3
1120 IF X$=" ft."THEN DD=D/3.048999E+02
1130 IF X$=" in."THEN DD=D/2.539999E+01
1140 PRINT TAB(9);;"Distance (";X$;;") to BEST POINT on which to focus ....";
1150 PRINT USING U$;DD
1160 PRINT UL$;
1170 PRINT TAB(9);;"Do you want a depth-of-field chart?    (y/n)"
1180 Z$=INKEY$
1190 IF Z$="n"OR Z$="N"THEN 1220
1200 IF Z$="y"OR Z$="Y"THEN 1260
1210 GOTO 1180
1220 LOCATE CSRLIN-1:PRINT E$;
1230 GOSUB 1950
1240 GOTO 10
1250 '
1260 '.....depth-of-field
1270 CLS
1280 COLOR 0,7:PRINT UL$;
1290 T$=" DEPTH-OF-FIELD "
1300 COLOR 0,7:LOCATE 1,INT((80-LEN(T$))\2):PRINT T$
1310 COLOR 3,0:PRINT STRING$(80,223);
1320 COLOR 7,0
1330 IF D THEN 1430
1340 '
1350 PRINT " Do you want distances in (m)etres, (f)eet or (i)nches?    (m/f/i)"
1360 Z$=INKEY$
1370 IF Z$="m"OR Z$="M"THEN X$=" m. ":X=1000:GOTO 1420
1380 IF Z$="f"OR Z$="F"THEN X$=" ft.":X=3.047999E+02:GOTO 1420
1390 IF Z$="i"OR Z$="I"THEN X$=" in.":X=2.539999E+01:GOTO 1420
1400 GOTO 1360
1410 '
1420 LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1
1430 INPUT " ENTER: Focal length of lens (mm) ";F
1440 LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1
1450 IF D THEN 1520
1460 IF F=0 THEN 1430
1470 PRINT " ENTER: Distance to point of focus (";X$;;")";:INPUT D
1480 LOCATE CSRLIN-1:PRINT E$;:LOCATE CSRLIN-1
1490 IF D THEN D=D*X:GOTO 1520       'distance in mm
1500 GOTO 1470
1510 '
1520 '.....display
1530 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
1540 V$="###.##"
1550 IF X$=" m. "THEN DD=D/10^3
1560 IF X$=" ft."THEN DD=D/3.047999E+02
1570 IF X$=" in."THEN DD=D/2.539999E+01
1580 PRINT " DEPTH-OF-FIELD TABLE FOR A";F;;"mm LENS FOCUSED AT";USING V$;DD;
1590 PRINT X$
1600 PRINT UL$;
1610 PRINT " LENS";TAB(18);;"NEAREST POINT";TAB(42);;"POINT OF";
1620 PRINT TAB(60);;"FARTHEST POINT"
1630 PRINT " APERTURE";TAB(18);;"IN SHARP FOCUS";TAB(43);;"FOCUS";
1640 PRINT TAB(60);;"IN SHARP FOCUS"
1650 PRINT UL$;
1660 '
1670 M=F/(D-F)           'magnification at point-of focus
1680 V=M*D               'lens-to-film distance at point of focus
1690 C=2.539999E-02             'radius of circle of confusion in mm
1700 '
1710 FOR Z=1 TO 12
1720 A=A(Z):GOSUB 320    'format aperture
1730 R=F/(2*A)           'aperture radius
1740 NR=R*V/(R-C)        'lens-to film distance for nearest point
1750 FR=R*V/(R+C)        'lens-to film distance for farthest point
1760 MN=F/(NR-F)         'magnification to nearest point
1770 MF=F/(FR-F)         'magnification to farthest point
1780 DN=NR*MN            'distance to nearest point (mm)
1790 DF=FR*MF            'distance to farthest point (mm)
1800 IF DN<=DA THEN COLOR 15,4:DA=0
1810 PRINT " �/";USING A$;A;
1820 PRINT TAB(20);USING "###.##";DN/X;:PRINT X$;   'nearest point
1830 PRINT TAB(41);USING "###.##";D/X;:PRINT X$;    'point-of-focus
1840 IF MF<=1 THEN PRINT TAB(63);;"INFINITY";:GOTO 1860
1850 PRINT TAB(62);USING "###.##";DF/X;:PRINT X$    'farthest point
1860 COLOR 7,0
1870 NEXT Z
1880 PRINT UL$;
1890 PRINT" This table is based on a maximum circle of confusion diameter of ";
1900 PRINT USING "#.###";C*2/2.539999E+01;:PRINT CHR$(34)
1910 PRINT UL$;
1920 GOSUB 1950
1930 GOTO 10
1940 '
1950 'PRT
1960 KEY OFF:GOSUB 2030:LOCATE 25,5:COLOR 0,2
1970 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1980 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1990 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1990 ELSE GOSUB 2030
2000 IF Z$="3"THEN RETURN
2010 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2020 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1960
2030 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

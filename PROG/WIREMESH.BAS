1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'WIREMESH - Wire Mesh Screens - 24 FEB 96 rev. 10 JAN 1999
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,1
50 U$="#####.###"
60 U1$="#####.#"
70 UL$=STRING$(80,205)
80 PI=3.141592
90 DIM WP(11)
100 DATA .3, .8, 1.3, 2.1, 3.1, 4.2, 5.5, 6.9, 8.5, 10.3, 12.3
110 FOR Z=1 TO 11:READ WP(Z):NEXT Z
120 GOTO 200
130 '
140 '.....format input line
150 LOCATE CSRLIN-1:PRINT SPC(7);
160 IF ZZ=SA THEN X$=U1$ ELSE X$=U$
170 LOCATE CSRLIN,47:PRINT STRING$(7,".");USING X$;ZZ;
180 RETURN
190 '
200 '.....start
210 CLS
220 COLOR 15,2
230 PRINT " WIRE MESH SCREENS";TAB(57);;"by George Murphy VE3ERP ";
240 COLOR 1,0:PRINT STRING$(80,223);
250 COLOR 7,0
260 GOSUB 960   'preface
270 PRINT UL$;
280 COLOR 0,7
290 LOCATE,22:PRINT " Press 1 to continue or 0 to quit...."
300 COLOR 7,0
310 Z$=INKEY$:IF Z$=""THEN 310
320 IF Z$="0"THEN CLS:RUN EX$
330 IF Z$="1"THEN 430
340 GOTO 310
350 '
360 '.....mesh cell data (LS=lgth.of side, CA=cell area, WL=wire length
370 '.....(calculation for hex mesh - results same for square mesh)
380 LS=1.154999*A/2:CA=3.463999*(A/2)^2:WL=3*LS
390 LSO=1.154999*(A-D)/2:OA=3.463999*((A-D)/2)^2           'open area
400 WA=CA-OA:GOTO 410                                'wire area
410 RETURN
420 '
430 '.....inputs
440 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
450 PRINT " SQUARE OR HEXAGONAL WIRE MESH SCREEN"
460 INPUT " ENTER: c.c. Distance across mesh opening.......(in.)";A
470 ZZ=A:GOSUB 140:PRINT " in.";TAB(68);USING U1$;A*2.539999E+01;:PRINT " mm"
480 INPUT " ENTER: Wire diameter...........................(in.)";D
490 ZZ=D:GOSUB 140:PRINT " in.";TAB(68);USING U1$;D*2.539999E+01;:PRINT " mm"
500 COLOR 0,7
510 INPUT " ENTER: Screen area.............................(ftý)";SA
520 ZZ=SA:GOSUB 140:PRINT " ftý";TAB(68);USING U1$;SA*9.290000E-02;:PRINT " mý   ";
530 COLOR 7,0
540 '
550 GOSUB 360         'cell data
560 WV=WL*PI*(D/2)^2  'wire volume per cell
570 N=SA/CA*144       'total number of cells
580 TV=WV*N           'total wire volume
590 FF=WA/CA          'filled area factor
600 FA=SA*FF          'wind vane area
610 WTA=9.749998E-02         'weight/cu.in. of aluminum
620 WTC=3.210000E-01          'weight/cu.in. of copper
630 WTS=2.834999E-01         'weight/cu.in. of steel
640 Z=TV*WTA
650 PRINT "        Weight of this area of mesh: aluminum wire...";
660 PRINT USING U1$;Z;:PRINT " lb.";TAB(68)USING U1$;Z*4.535999E-01;:PRINT " kg"
670 Z=TV*WTS
680 PRINT "                                     steel wire......";
690 PRINT USING U1$;Z;:PRINT " lb.";TAB(68)USING U1$;Z*4.535999E-01;:PRINT " kg"
700 Z=TV*WTC
710 PRINT "                                     copper wire.....";
720 PRINT USING U1$;Z;:PRINT " lb.";TAB(68)USING U1$;Z*4.535999E-01;:PRINT " kg"
730 PRINT "        Percent of screen area filled by wire........";
740 PRINT USING U1$;FF*100;:PRINT " %"
750 PRINT "        Wind vane area (screen area filled by wire)..";USING U1$;FA;
760 PRINT " ftý";TAB(68);USING U1$;FA*9.290000E-02;:PRINT " mý"
770 COLOR 0,7:LOCATE ,9
780 PRINT " Wind load on this area of vertical mesh at various wind speeds:"
790 COLOR 7,0
800 '
810 X=0
820 FOR Z=1 TO 6 STEP 5.000000E-01
830 SM=Z*10*1.150799
840 KM=Z*10*1.851899
850 X=X+1
860 Y=WP(X)*FA
870 K=8.399999E-01
880 PRINT "    @ ";USING "##";Z*10;
890 PRINT " knots =";USING "###";SM;:PRINT " mph =";USING "###";KM;
900 PRINT " km/hr =";USING " Beaufort###.#";(3-K)+X*K;:PRINT "...";
910 PRINT USING U1$;Y;:PRINT " lb.";TAB(68)USING U1$;Y*4.535999E-01;:PRINT " kg"
920 NEXT Z
930 GOTO 1240
940 END
950 '
960 '.....preface
970 T=(7)
980 PRINT TAB(T);
990 PRINT "This program may be used in the design of reflectors for antennas"
1000 PRINT TAB(T);
1010 PRINT "in the VHF/UHF frequency range, or for anything else you want to"
1020 PRINT TAB(T);
1030 PRINT "make out of chain link fencing, chicken wire, hardware cloth, or"
1040 PRINT TAB(T);
1050 PRINT "window screening.";
1060 PRINT
1070 PRINT TAB(T);
1080 PRINT "The calculated effects of wind do not apply if any part of the mesh"
1090 PRINT TAB(T);
1100 PRINT "is covered by ice, snow, leaves, plastic sheets, or other matter"
1110 PRINT TAB(T);
1120 PRINT "that obstructs the free passage of air through the mesh."
1130 PRINT
1140 PRINT TAB(T);
1150 PRINT "A hexagonal mesh screen of a given size contains the same amount of"
1160 PRINT TAB(T);
1170 PRINT "wire as a square mesh screen of the same size, if both screens have"
1180 PRINT TAB(T);
1190 PRINT "the same size mesh (i.e. same distance between between opposite"
1200 PRINT TAB(T);
1210 PRINT "sides of the hexagonal or square opening)."
1220 RETURN
1230 '
1240 '.....end
1250 GOSUB 1280:GOTO 200
1260 END
1270 '
1280 '.....PRT
1290 KEY OFF:GOSUB 1360:LOCATE 25,5:COLOR 0,2
1300 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1310 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1320 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1320 ELSE GOSUB 1360
1330 IF Z$="3"THEN RETURN
1340 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1350 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1290
1360 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

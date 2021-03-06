1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'LOOPANT - Loop Antenna Coil Inductance - 16 JAN 97 rev.12 APR 97
15 DIM K(5,5), K$(5)
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,1
50 U$="###.###"
60 UL$=STRING$(80,205)
70 PI=3.141592
80 'DIM K(4,4)
90 FOR Z=1 TO 4:READ K$(Z):NEXT Z
100 FOR Z=1 TO 4:FOR Y=1 TO 4
110 READ K(Y,Z):NEXT Y:NEXT Z
120 GOTO 250
130 '
140 DATA TRIANGULAR,SQUARE,HEXAGONAL,OCTAGONAL
150 DATA   .006,   .008,   .012,   .016
160 DATA 1.1547, 1.4142,      2,  2.613
170 DATA .65533, .37942, .65533,   .75143
180 DATA  .1348,  .3333,  .1348,   .07153
190 '
200 '.....format input line
210 LOCATE CSRLIN-1:PRINT SPC(7);
220 LOCATE CSRLIN,47:PRINT STRING$(7,".");USING U$;ZZ;
230 RETURN
240 '
250 '.....start
260 CLS:N=0:D=0
270 COLOR 15,2
280 PRINT " LOOP ANTENNA COIL INDUCTANCE";TAB(57);;"by George Murphy VE3ERP ";
290 COLOR 1,0:PRINT STRING$(80,223);
300 COLOR 7,0
310 GOSUB 1030   'preface
320 PRINT UL$;
330 PRINT " Press number in < > for:"
340 PRINT UL$;
350 PRINT "  < 1 >  Triangular Loop"
360 PRINT "  < 2 >  Square Loop"
370 PRINT "  < 3 >  Hexagonal Loop"
380 PRINT "  < 4 >  Octagonal Loop"
390 PRINT
400 PRINT "  < 0 >  EXIT"
410 Z$=INKEY$:IF Z$=""THEN 410
420 IF Z$="0"THEN CLS:RUN EX$
430 Y=VAL(Z$)
440 IF Y=1 THEN X=3:GOTO 490      'X=no.of sides
450 IF Y=2 THEN X=4:GOTO 490
460 IF Y=3 THEN X=6:GOTO 490
470 IF Y=4 THEN X=8:GOTO 490
480 GOTO 510
490 PRINT UL$;
500 PRINT " Do you want dimensions in (c)entimetres or (i)nches?  (c/i)"
510 Z$=INKEY$:IF Z$=""THEN 510
520 IF Z$="c"THEN UM=1:UM$="cm.":GOTO 550
530 IF Z$="i"THEN UM=2.539999:UM$="in.":GOTO 550
540 GOTO 510
550 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
560 PRINT " ";K$(Y);;" LOOP:"
570 PRINT UL$;
580 '
590 '.....inputs
600 INPUT " ENTER: Number of turns..............................";N
610 ZZ=N:GOSUB 200
620 PRINT " ENTER: Length of each of";X;;"sides...............(";UM$;;")";
630 INPUT S:S=S*UM
640 ZZ=S:GOSUB 200:PRINT " cm. (";USING U$;S/2.539999;:PRINT " in.)"
650 IF Y=1 THEN R=S/1.731999
660 IF Y=2 THEN R=(SQR(2*S^2))/2
670 IF Y=3 THEN R=S
680 IF Y=4 THEN R=1.307000*S
690 PRINT "        Radius (centre of loop to each corner)......."
700 ZZ=R:GOSUB 200:PRINT " cm. (";USING U$;R/2.539999;:PRINT " in.)"
710 PRINT "        Length of conductor per turn................."
720 ZZ=S*X/100:GOSUB 200:PRINT " m.  (";USING U$;ZZ/3.048000E-01;:PRINT " ft.)"
730 PRINT "        Total conductor length......................."
740 ZZ=ZZ*N:GOSUB 200:PRINT " m.  (";USING U$;ZZ/3.048000E-01;:PRINT " ft.)"
750 IF X=3 THEN AR=S/2*SQR(S^2-(S^2/2/S)^2)
760 IF X=4 THEN AR=S^2
770 IF X=6 THEN AR=2.598000*S^2
780 IF X=8 THEN AR=4.828000*S^2
790 PRINT "        Area of loop................................."
800 ZZ=AR/10^4
810 GOSUB 200:PRINT " m�  (";USING U$;ZZ/3.048000E-01^2;:PRINT " ft�)"
820 IF N=1 THEN 830 ELSE 860
830 PRINT " ENTER: Conductor diameter........................(";UM$;;")";
840 INPUT D:D=D*UM
850 ZZ=D:GOSUB 200:PRINT " cm. (";USING U$;D/2.539999;:PRINT " in.)"
860 PRINT " ENTER: Coil length...............................(";UM$;;")";
870 IF D THEN L=D:PRINT:GOTO 890
880 INPUT L:L=L*UM
890 ZZ=L:GOSUB 200:PRINT " cm. (";USING U$;L/2.539999;:PRINT " in.)"
900 IF D THEN 940
910 PRINT "        Turn spacing................................."
920 SP=L/N
930 ZZ=SP:GOSUB 200:PRINT " cm. (";USING U$;SP/2.539999;:PRINT " in.)"
940 '.....calculate
950 A=K(Y,1):BX=K(Y,2):C=K(Y,3):DX=K(Y,4)
960 B=BX*S*N/((N+1)*L)
970 D=DX*(N+1)*L/(S*N)
980 UH=A*N^2*S*(LOG(B)+C+D)
990 PRINT "        Inductance..................................."
1000 ZZ=UH:GOSUB 200:PRINT " �H"
1010 GOSUB 1150:GOTO 250
1020 '
1030 '.....preface
1040 TB=7
1050 PRINT TAB(TB);
1060 PRINT "This program calculates inductances of very short large diameter"
1070 PRINT TAB(TB);
1080 PRINT "single-layer air-core coils (loop antennas), using equations"
1090 PRINT TAB(TB);
1100 PRINT "provided by F. W. Grover of the U.S. National Bureau of Standards."
1110 PRINT TAB(TB);
1120 PRINT "(The ARRL ANTENNA BOOK, 17th edition, page 5-4, Table 1)"
1130 RETURN
1140 '
1150 'PRT
1160 KEY OFF:GOSUB 1230:LOCATE 25,5:COLOR 0,2
1170 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1180 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1190 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1190 ELSE GOSUB 1230
1200 IF Z$="3"THEN RETURN
1210 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1220 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1160
1230 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$,D,T,NT,WW
10 'RESICOP - Resistor, Copper Wire - 19 JUL 95 rev. 14 JUN 2001
20 IF EX$=""THEN EX$="EXIT"
30 PROG$="resicop"
40 
50 CLS:KEY OFF
60 COLOR 7,0,1
70 PI=3.141592
80 UL$=STRING$(80,205)       'underline
90 ER$=STRING$(80,32)        'erase
100 U$="####.###"
110 DIM A(50)                 'AWG sizes (American Wire Gauge)
120 '
130 '.....AWG wire sizes
140 K#= (4.600000E-01/4.999999E-03)^(1/39)        'incremental multiplier
150 FOR Z=1 TO 50
160 J=Z+3
170 WW=4.600000E-01/K#^J                  'wire diameter in inches
180 A(Z)=WW
190 NEXT Z
200 '
210 '.....start
220 CLS
230 COLOR 15,2
240 PRINT" RESISTOR - COPPER WIRE WOUND";TAB(57);;"by George Murphy VE3ERP ";
250 COLOR 1,0:PRINT STRING$(80,223);
260 COLOR 7,0
270 PRINT " Press number in < > to choose standard units of measure:"
280 PRINT UL$;
290 PRINT "  < 1 >  Metric"
300 PRINT "  < 2 >  U.S.A./Imperial"
310 PRINT UL$;
320 PRINT "    or press <0> to EXIT....."
330 Z$=INKEY$
340 IF Z$="0"THEN CLS:RUN EX$
350 IF Z$="1"THEN UM=2.539999E+01:UM$="mm ":GOTO 380
360 IF Z$="2"THEN UM=1:UM$="in.":GOTO 380
370 GOTO 330
380 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
390 GOTO 410
400 '
410 '.....input data
420 INPUT " ENTER: Desired Resistance.............(ohms)";R
430 COLOR 0,7
440 PRINT " Want to rate resistor by (c)urrent or (w)atts?   (c/w) ";:COLOR 7,0
450 Z$=INKEY$:IF Z$=""THEN 450
460 IF Z$="c"OR Z$="w"THEN LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1:GOTO 490
470 GOTO 450
480 '
490 IF Z$="c"THEN INPUT " ENTER: Maximum Current................(amps)";I
500 IF Z$="w"THEN INPUT " ENTER: Power rating..................(watts)";P
510 IF I=0 THEN I=SQR(P/R)
520 IF P=0 THEN P=I^2*R
530 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
540 PRINT TAB(8);USING " Resistance........................... ###.### ohms";R
550 PRINT TAB(8);USING " Maximum current...................... ###.### amp.";I
560 PRINT TAB(8);USING " Power rating......................... ###.### watts";P
570 '
580 C=I*250
590  FOR Z=50 TO 1 STEP-1
600   CM=(A(Z)*10^3)^2
610   IF CM>C THEN W=Z:MIN=A(Z):GOTO 640
620  NEXT Z
630 '
640 PRINT TAB(9);;"Minimum wire diameter................";USING U$;MIN*UM;
650 PRINT " ";UM$;USING "  (AWG ##.#)";W
660 LN=CSRLIN
670 IF UM=2.539999E+01 THEN 820
680 COLOR 0,7
690 PRINT " Press number in < > to indicate how you wish to select wire size:"
700 COLOR 7,0
710 PRINT "        <1> by DIAMETER"
720 PRINT "        <2> by AWG #"
730 Z$=INKEY$
740 IF Z$="1"OR Z$="2"THEN VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN:GOTO 760
750 GOTO 730
760 IF Z$="1"THEN 820
770 PRINT " ENTER: AWG wire gauge #";:INPUT Z:WW=A(Z)  'WW = wire diameter
780 LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1
790 'PRINT "        Selected Wire Gauge..............AWG#";Z
800 GOTO 850
810 '
820 PRINT " ENTER: Wire diameter (";UM$;;")";:INPUT WW:WW=WW/UM
830 LOCATE CSRLIN-1:PRINT ER$;:LOCATE CSRLIN-1
840 '
850 OF=10371/(WW*10^3)^2        'ohms per 1000 ft.
860 '
870 LW=R/OF*10^3*12             'length in inches
880 PRINT "        Selected Wire Diameter...............";USING U$;WW*UM;
890 Z=LOG(4.600000E-01/WW)/(LOG(1.122900))-3
900 PRINT " ";UM$;USING "  (AWG ##.#)";Z
910 IF WW>=MIN THEN 950
920 BEEP:COLOR 0,7:PRINT " WARNING! ";
930 PRINT "Selected wire size is too small for specified maximum current "
940 COLOR 7,0
950 UUM=3.048000E-01:UUM$=" ê/Km"
960 IF UM=1 THEN UUM=1:UUM$=" ê/M ft."
970 PRINT "        Unit resistance......................";USING U$;OF/UUM;
980 PRINT UUM$
990 IF UM=2.539999E+01 THEN UUM=3.048000E-01:UUM$="m."ELSE UUM=1:UUM$="ft."
1000 PRINT "        Length of wire.......................";USING U$;LW/12*UUM;
1010 PRINT " ";UUM$
1020 PRINT
1030 LN=CSRLIN:LOCATE LN,17:COLOR 0,7
1040 PRINT " Want to design a resistor coil form?   (y/n) ";:COLOR 7,0
1050 Z$=INKEY$:IF Z$=""THEN 1050
1060 IF Z$="n"THEN LOCATE LN:PRINT ER$;:GOTO 1430
1070 IF Z$="y"THEN LOCATE LN:PRINT ER$;:GOTO 1100
1080 GOTO 1050
1090 '
1100 '.....design form
1110 LOCATE LN
1120 IF UM=1 THEN M=UM:M$=UM$
1130 IF UM=2.539999E+01 THEN M=2.539999:M$="cm"
1140 PRINT " ENTER: Desired Coil Form Diameter (";M$;;")......";:INPUT Z
1150 LOCATE CSRLIN-1
1160 Z$="        Coil form diameter................... ###.### "+M$
1170 PRINT USING Z$;Z
1180 D=(Z/M)+WW
1190 Z$="        Coil pitch circle diameter........... ###.### "+M$
1200 PRINT USING Z$;D*M
1210 LL=LW-2 'length less 2" for end leads
1220 NT=LL/(PI*D)   'no.of turns
1230 Z$="        Number of turns...................... ###.### "
1240 PRINT USING Z$;NT
1250 LG=NT*WW*2
1260 Z$="        Length of coil ...................... ###.### "
1270 PRINT USING Z$;LG*M;:PRINT M$
1280 T=1/(2*WW)
1290 Z$="        Turns per inch (25.4 mm)............. ###.### "
1300 PRINT USING Z$;T
1310 'LD=(LW-NT*D*PI)/2
1320 'Z$="        Length of coil end leads............. ###.### "+M$
1330 'PRINT USING Z$;LD*M
1340 PRINT
1350 COLOR 0,7:LOCATE ,12
1360 PRINT " Want to see inductive properties of this coil?   (y/n) ";:COLOR 7,0
1370 Z$=INKEY$:IF Z$=""THEN 1370
1380 IF Z$="n"THEN LOCATE CSRLIN-1:PRINT ER$;:GOTO 1430
1390 IF Z$="y"THEN CLS:CHAIN"coilequa"
1400 GOTO 1370
1410 END
1420 '
1430 '.....end
1440 GOSUB 1470
1450 GOTO 210
1460 '
1470 'PRT
1480 KEY OFF:GOSUB 1550:LOCATE 25,5:COLOR 0,2
1490 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1500 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1510 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1510 ELSE GOSUB 1550
1520 IF Z$="3"THEN RETURN
1530 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1540 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1480
1550 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

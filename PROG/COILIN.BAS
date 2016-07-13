1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'COILIN - Insulated wire coils - 17 APR 08
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PI=4*ATN(1)
70 DIM F(10)
80 DIM Q(10)
90 DIM H(10)
100 DIM GA(50)
110 '.....AWG calculation
120 K#=(4.600000E-01/4.999999E-03)^(1/39)
130 FOR WG=1 TO 40
140 N=WG+3
150 GA(WG)=4.499999E-01/K#^N
160 NEXT WG
170 GOTO 210
180 '.....clear to bottom
190 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
200 RETURN
210 '.....title page
220 CLS
230 COLOR 15,2
240 PRINT " COILS - INSULATED WIRE";TAB(57);"by George Murphy VE3ERP ";
250 COLOR 1,0:PRINT STRING$(80,223);
260 COLOR 7,0
270 '.....start
280 PRINT " Coils wound with insulated wire have a `built-in' turn spacer in ";
290 PRINT "that when    "
300 PRINT " close-wound the insulation can separate the wires enough to incre";
310 PRINT "ase the Q of "
320 PRINT " the coil over what it would be if close-wound with enamelled wire";
330 PRINT ". Insulated  "
340 PRINT " stranded wire is also easier to wind neatly than solid wire."
350 PRINT:LN=CSRLIN
360 PRINT " .....Press 1 to continue or 0 to EXIT....."
370 Z$=INKEY$:IF Z$=""THEN 370
380 IF Z$="0"THEN CLS:RUN EX$
390 IF Z$="1"THEN 450
400 GOTO 380
410 PRINT " For wire sizes in AWG enter a size in the range 3-40"
420 PRINT " For wire sizes in inches enter a diameter larger than .001"
430 PRINT " To EXIT enter size 0"
440 PRINT
450 LOCATE CSRLIN-1:PRINT STRING$(79,32):LOCATE CSRLIN-1
460 INPUT " ENTER: Wire size";X
470 INPUT " ENTER: Inductance (ÊH)";U
480 IF X>=3 AND X<=40 THEN WG=X:D=4.600000E-01/K#^(WG+3):GOTO 510
490 IF X>=1.000000E-03 THEN D=X:WG=LOG(4.600000E-01/D)/(LOG(K#))-3:GOTO 510
500 BEEP:GOSUB 180:GOTO 410
510 PRINT USING " (wire diameter is #.### in.)";D
520 INPUT " ENTER: Outside diameter of insulation in inches";OD
530 IF OD>D THEN 580
540 BEEP:PRINT USING " O.D. CANNOT BE SMALLER THAN ##.###";D;:PRINT ;" !"
550 PRINT " Press any key......"
560 Z$=INKEY$:IF Z$=""THEN 560
570 GOTO 520
580 '.....initial display
590 CLS
600 COLOR 0,7:PRINT " HI-Q INSULATED WIRE COIL ";:COLOR 7,0
610 PRINT
620 PRINT USING " Inductance........... ###.## ÊH";U
630 PRINT USING " Wire size.........AWG ###.###";WG
640 PRINT USING " Wire dia............. ###.### in.(###.# mm.)";D,D*2.539999E+01
650 PRINT USING " Insulation O.D....... ###.### in.(###.# mm.)";OD,OD*2.539999E+01
660 '.....find a diameter
670 PD=10:X=1:R=5.000000E-01   'R=L/D ratio
680 L=R*PD:N=L/OD    'L=length
690 Y=SQR(U*(18*PD+40*L))/PD  'number of turns
700 IF ABS(Y-N)<9.999999E-05 THEN 730
710 IF SGN(Y-N)<>SGN(X)THEN X=-X/10
720 PD=PD+X:GOTO 680
730 '.....continue display
740 LN=CSRLIN:PRINT
750 FD=PD-OD
760 PD=FD+OD:L=PD*R:N=SQR(U*(18*PD+40*L))/PD
770 GOSUB 180
780 PRINT USING " Pitch dia............ ###.### in.(###.# mm.)";PD,PD*2.539999E+01
790 PRINT USING " Coil length.......... ###.### in.(###.# mm.)";L,L*2.539999E+01
800 PRINT USING " Coil form diameter... ###.### in.(###.# mm.)";FD,FD*2.539999E+01
810 PRINT USING " Coil lgth/dia.ratio.. ###.###:1";L/PD
820 PRINT USING " Windings pitch....... ###.### in.(###.# mm.)";OD,OD*2.539999E+01
830 WF=PI*PD*N/12:WM=WF*3.048000E-01
840 PRINT USING " Close-wound turns.... ###.### ( ##.# ft. = ##.## m.)";N,WF,WM
850 PRINT:Z=CSRLIN
860 PRINT" (Q increases and turns decrease as coil diameter increases)        "
870 PRINT " Want to change coil form diameter?   (y/n)                        "
880 Z$=INKEY$:IF Z$=""THEN 880
890 VIEW PRINT Z TO 24:CLS:VIEW PRINT:LOCATE Z
900 IF Z$="y"THEN INPUT " ENTER: New form diameter (inches)";FD:GOTO 760
910 IF Z$="n"THEN 930
920 GOTO 900
930 '.....calculate Q
940 DATA 1.8,3.5,7,10.1,14,18.068,21,24.89,28,30
950 FOR Y=1 TO 10:READ F(Y):NEXT Y:RESTORE
960 LOCATE Z
970 FOR H=1 TO 10
980 SW=OD/D   'pitch/wire diameter
990 A=PD/2/2.539999:B=L*2.539999:JJ=2.533028E+04    'A=radius in cm.  B=coil length in cm.
1000 IHC=LOG(SW+SQR(SW^2-1))
1010 CO=PI*PD*2.539999/(3.599999*IHC)
1020 FS=SQR((JJ/(CO*L*2.539999)))
1030 CR=JJ/(F(H)^2*U)
1040 Q(H)=SQR(F(H))*10^3/(6.900000/A+4.500000/B)
1050 BW(H)=F(H)/Q(H)*10^3
1060 PRINT USING " at ###.### MHz Q = ####: Bandwidth = ### kHz";F(H),Q(H),BW(H)
1070 NEXT H
1080 PRINT USING " at ###.### MHz the coil is self-resonant.";FS;
1090 '.....end
1100 GOSUB 1110:GOTO 210
1110 '.....PRT
1120 KEY OFF:GOSUB 1190:LOCATE 25,5:COLOR 0,2
1130 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1140 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1150 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1150 ELSE GOSUB 1190
1160 IF Z$="3"THEN RETURN
1170 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1180 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1120
1190 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'vacap14 - 26 DEC 2007
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PI=4*ATN(1)  '3.141593
70 UL$=STRING$(80,205)
80 U1$="#####.###"
90 DIM A(13)
100 DIM B(13)
110 DIM R(13)
120 '.....title page
130 CUC=0:LUC=0:DUC=0:FLC=0:CD=0:AM=0:KUC=0:DLC=0:KLC=0:NLC=0:ARE=0:R1=0
140 COLOR 15,2,0
150 PRINT " VARIABLE CAPACITOR PLATE DESIGNER";TAB(57);"by George Murphy VE3ERP ";
160 COLOR 1,0:PRINT STRING$(80,223);
170 COLOR 15,2:LOCATE CSRLIN-1,25:PRINT " Algorithm by R.J. Dehoney IEEE "
180 COLOR 7,0
190 FOR J=0 TO 12 :A(J)=PI/12*J :NEXT J
200 '.....start
210 PRINT
220 PRINT "This program calculates plotting data for straight line frequency ";
230 PRINT "variable"
240 PRINT "capacitor plates in an L/C network tuned circuit."
250 PRINT
260 PRINT "Ref. Terman - page 123 (plate shapes)"
270 PRINT "Ref. Forbes - Proceedings of the IRE Aug 1925 vol.13 issue 4.";
280 PRINT
290 PRINT "Required inputs are inductance in ÊH, capacitances in pF, maximum ";
300 PRINT "voltage, gap "
310 PRINT "between plates, and radius of the of the shaft cutout in the fixed";
320 PRINT " stator "
330 PRINT "plates."
340 PRINT
350 PRINT "If the application is for link coupled tuners, first run the `Link ";
360 PRINT "Coupled"
370 PRINT "Tuners' program for input values required by this program.";
380 PRINT
390 PRINT "If the physical size of the capacitor is not practical run Hamcalc";
400 PRINT "'s"
410 PRINT "`Capacitors - Trimmer' program to selcet an appropriate capacitor/";
420 PRINT "trimmer "
430 PRINT "combination."
440 PRINT
450 LOCATE ,20
460 COLOR 0,7:PRINT ".....Press 1 to continue or 0 to exit.....";:COLOR 7,0
470 Z$=INKEY$:IF Z$=""THEN 470
480 IF Z$="0"THEN RUN EX$
490 IF Z$="1"THEN CLS:GOTO 510
500 GOTO 470
510 '.....calculation
520 PRINT
530 PRINT "    Want to enter dimensions in centimetres or inches?   (c/i)"
540 Z$=INKEY$:IF Z$=""THEN 540
550 IF Z$="c"THEN M=1:M$="cm.":GOTO 580
560 IF Z$="i"THEN M=2.539999:M$="in.":GOTO 580
570 GOTO 540
580 CLS
590 KD=1.000599  'dielectric constant for air
600 INPUT " ENTER: Maximum voltage ";V
610 GAP=V/21/10^3
620 'LL=CSRLIN
630 PRINT USING " Minimum gap between plates = ##.### ";GAP/M;:PRINT M$
640 LL=CSRLIN:INPUT " ENTER: Your choice of gap (not less than the above)";DLC
650 IF DLC<GAP/M THEN 640
660 DLC=DLC*M
670 VM=V/1000/DLC*2.539999
680 PRINT USING " Volts per mil (.001 in.) = ###.### ";VM
690 LN=CSRLIN:PRINT " .....Press 1 to contimue or 0 to change gap....."
700 Z$=INKEY$:IF Z$=""THEN 700
710 IF Z$="1"THEN VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN:GOTO 730
720 IF Z$="0"THEN CLS:GOTO 630
730 INPUT " ENTER: Capacitance of Variable Capacitor fully CLOSED (pF)";CMAX
740 INPUT " ENTER: Capacitance of Variable Capacitor fully OPEN (pF)";CMIN
750 CUCMAX=CMAX/10^12 :CUCMIN=CMIN/10^12
760 INPUT " ENTER: Inductance of tuned circuit in uH";Z
770 LUC=Z/10^6
780 FL=1/(2*PI*SQR(Z*CMAX))*1.000000E+03 :FH=1/(2*PI*SQR(Z*CMIN))*1.000000E+03
790 X=2*PI*FL*Z
800 PRINT " ENTER: Radius of cutout on stator plates (";M$;;")";:INPUT Z:R1=Z*M
810 CLS
820 PRINT USING " Maximun capacitance......... ####.### pF";CMAX
830 PRINT USING " Minimum capacitance......... ####.### pF";CMIN
840 PRINT USING " Inductance.................. ####.### ÊH";Z
850 PRINT USING " Lowest frequency, FL........ ####.### MHz";FL
860 PRINT USING " Reactance at FL............. ####.### ohms";X
870 PRINT USING " Stator cutout radius........ ####.### ";R1/M;:PRINT M$
880 PRINT USING " Voltage..................... ####.### volts";V
890 PRINT USING " Gap between plates.......... ####.### ";DLC/M;:PRINT M$
900 PRINT USING " Volts per mil............... ####.### ";VM
910 LN=CSRLIN:INPUT " ENTER: a reasonable number of plates";N
920 IF N<2 THEN 910
930 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
940 PRINT USING " Number of plates............ ####";N
950 NLC= N-1
960 DUC=1/(2*PI*SQR(LUC))
970 FLC=DUC/SQR(CUCMAX)
980 DF=(FH-FL)*1.000000E+06 :CD=DF/12
990 AM=A(1)
1000 KUC=DF/PI 'Forbes' K factor
1010 KLC=10^-11/(36*PI*DLC) ' Forbes' k factor
1020 ARE=CUCMAX/KLC/NLC
1030 GOSUB 1480
1040 VIEW PRINT LN TO 24:CLS:VIEW PRINT
1050 PRINT USING "Frequency............... ####.### MHz";FLC/10^6
1060 PRINT USING "Capacitance............. ####.### pF";C
1070 PRINT USING "Inductance.............. ####.### ÊH";LUC*10^6
1080 PRINT USING "Number of plates........ ####.### ";N
1090 PRINT USING "Radius of shaft bulge... ####.### cm.";R1
1100 CLS
1110 PRINT "The program calculates plotting data for capacitor plates consis";
1120 PRINT "ting of 12";
1130 PRINT "segments, each 15¯wide, radiating from the centre of the capacito";
1140 PRINT "r shaft."
1150 PRINT
1160 PRINT "RADIAL";TAB(15);M$;" & ANGLE";TAB(32);"CAPACITANCE";
1170 PRINT  TAB(49);"FREQ.";TAB(61);"SHAFT ROTATION"
1180 FOR H=0 TO 12
1190 B=DUC/KUC/SQR(CUCMIN)
1200 FHZ=FH*10^6-H*CD
1210 C=(1/4/PI^2/FHZ^2/LUC)*9.999999E+11 ' Capacitance in pF
1220 F=SQR(2.533028E+04/C/LUC)/10^3  ' Frequency in MHz
1230 Q1=(B-A(H))^3
1240 Q2=(NLC*KLC*KUC^2*Q1)
1250 Q3=(4*DUC^2)/Q2
1260 R(H)=SQR(Q3+R1^2)
1270 PRINT "  #";H+1;
1280 LOCATE CSRLIN,10:PRINT USING " ###.## long @ ###¯";R(H)/M,A(H)*180/PI;
1290 PRINT USING  "   ####.### pF";C;
1300 PRINT USING  " ####.### MHz";F;
1310 PRINT TAB(65);180-15*H;;"¯"
1320 NEXT H
1330 PRINT
1340 Z=DLC:IF Z<5.000000E-02 THEN Z=5.000000E-02
1350 PRINT N;"plates,";
1360 PRINT USING " ##.### ";DLC/M;:PRINT M$; " gap betwwen plates";
1370 PRINT " (arcing limit =";:PRINT USING " ##### volts";V;:PRINT ")"
1380 PRINT TAB(20);" To decrease plate size use more plates."
1390 PRINT TAB(20);" To increase plate size use fewer plates."
1400 PRINT
1410 LN=CSRLIN:PRINT TAB(20);" Want to change number of plates?   (y/n)";
1420 Z$=INKEY$:IF Z$=""THEN 1420
1430 IF Z$="n"THEN VIEW PRINT LN TO 24:CLS:VIEW PRINT:GOTO 1460
1440 IF Z$="y"THEN 810
1450 GOTO 1420
1460 '.....end
1470 GOSUB 1480:CLS:GOTO 120
1480 '.....PRT
1490 KEY OFF:GOSUB 1560:LOCATE 25,5:COLOR 0,2
1500 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1510 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1520 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1520 ELSE GOSUB 1560
1530 IF Z$="3"THEN RETURN
1540 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1550 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1490
1560 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'SPEEDTD - 13 SEP 1986  rev. 17 FEB 2008, JAN 2010
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PROG$="speedtd"
70 L$=STRING$(80,205)
80 P$="#########,###.##"
90 '.....start
100 CLS
110 COLOR 15,2
120 PRINT " SPEED, TIME and DISTANCE Calculator";
130 PRINT TAB(57);"by George Murphy VE3ERP ";
140 PRINT
150 COLOR 7,0
160 S=0:T=0:D=0
170 PRINT " Press number in < > to:"
180 PRINT L$;
190 PRINT "  < 1 >  RUN Speed, Time & Distance Calculator"
200 PRINT "  < 2 >  Run Speed Chase program"
210 PRINT "  < 0 >  EXIT"
220 Z$=INKEY$:IF Z$="0"THEN CLS:RUN EX$
230 IF Z$="1"THEN 250 ELSE 220
240 IF Z$="2"THEN CLS:CHAIN"chase"
250 GOSUB 580
260 PRINT " Press letter in < > to select Unit of Distance:"
270 PRINT L$;
280 PRINT "   < a >  Millimetre"
290 PRINT "   < b >  Centimetre"
300 PRINT "   < c >  Inch"
310 PRINT "   < d >  Foot"
320 PRINT "   < e >  Metre"
330 PRINT "   < f >  Kilometre"
340 PRINT "   < g >  Statute mile"
350 PRINT "   < h >  Nautical mile"
360 Z$=INKEY$:IF Z$=""THEN 360
370 IF Z$="a"THEN D$="mm."
380 IF Z$="b"THEN D$="cm."
390 IF Z$="c"THEN D$="in."
400 IF Z$="d"THEN D$="ft."
410 IF Z$="e"THEN D$="m."
420 IF Z$="f"THEN D$="km."
430 IF Z$="g"THEN D$="stat.miles "
440 IF Z$="h"THEN D$="naut.miles "
450 IF ASC(Z$)>=97 AND ASC(Z$)<=104 THEN 470
460 GOTO 360
470 PRINT L$;
480 PRINT " Press letter in < > to select unit of time:"
490 PRINT L$;
500 PRINT "   < h >  Hour"
510 PRINT "   < m >  Minute"
520 PRINT "   < s >  Second"
530 Z$=INKEY$:IF Z$="h"THEN T$="hr.":GOTO 570
540 IF Z$="m"THEN T$="min.":GOTO 570
550 IF Z$="s"THEN T$="sec.":GOTO 570
560 GOTO 530
570 GOSUB 580:GOTO 630
580 '.....erase line
590 FOR I=3 TO CSRLIN:LOCATE I:PRINT STRING$(80,32);:NEXT I:LOCATE 3:RETURN
600 '..... Enter command
610 PRINT " ENTER: ";STRING$(30,"."):RETURN
620 '.....data entry
630 IF D THEN 670
640 GOSUB 600:LOCATE CSRLIN-1,9:PRINT "Distance (";D$;")";
650 LOCATE CSRLIN,41:INPUT D
660 IF D THEN IF S+T THEN 780
670 IF S THEN 720
680 LOCATE 3:PRINT L$;
690 GOSUB 600:LOCATE CSRLIN-1,9:PRINT "Speed (";D$;;"/";T$;")";
700 LOCATE CSRLIN,41:INPUT S
710 IF S THEN IF D+T THEN 780
720 IF T THEN 630
730 GOSUB 600:LOCATE CSRLIN-1,9:PRINT "Time (";T$;")";
740 LOCATE CSRLIN,41:INPUT T
750 IF T THEN IF D+S THEN 780
760 GOTO 630
770 '.....calculate
780 IF D*S THEN T=D/S:GOTO 810
790 IF D*T THEN S=D/T:GOTO 810
800 IF S*T THEN D=S*T:GOTO 810
810 IF T$="sec." THEN TS=T:TM=T/60:TH=T/3600:GOTO 840
820 IF T$="min." THEN TS=T*60:TM=T:TH=T/60:GOTO 840
830 IF T$="hr."  THEN TS=T*3600:TM=T*60:TH=T:GOTO 840
840 GOSUB 580:LOCATE 2
850 HR=INT(TS/3600):BAL=TS-HR*3600
860 MIN=INT(BAL/60):SEC=BAL-MIN*60
870 SIC=INT(SEC):BAL=SEC-SIC
880 MIN$=RIGHT$(STR$(MIN),LEN(STR$(MIN))-1)
890 IF LEN(MIN$)<2 THEN MIN$="0"+MIN$:GOTO 890
900 SIC$=RIGHT$(STR$(SIC),LEN(STR$(SIC))-1)
910 IF LEN(SIC$)<2 THEN SIC$="0"+SIC$:GOTO 910
920 IF D$="naut.miles "THEN 950
930 PRINT L$;
940 PRINT " SPEED";USING P$;S;:PRINT " ";D$;"/ ";T$:GOTO 960
950 PRINT " SPEED";USING P$;S;:PRINT " knots"
960 PRINT L$;
970 '.....screen print
980 PRINT " TIME  ";USING "#########,##";HR;
990 PRINT ":";MIN$;;":";SIC$
1000 PRINT "     =";USING P$;TH;:PRINT " hr.
1010 PRINT "     =";USING P$;TM;:PRINT " min.
1020 PRINT "     =";USING P$;TS;:PRINT " sec.
1030 PRINT L$;
1040 PRINT " DISTANCE";USING P$;D;:PRINT " ";D$
1050 PRINT L$;
1060 LN=CSRLIN:PRINT " Want to calculate vehicular fuel consumption?   (y/n)"
1070 Z$=INKEY$:IF Z$=""THEN 1070
1080 LOCATE LN:PRINT STRING$(79,32)
1090 IF Z$="n"THEN 1350
1100 IF Z$="y"THEN 1120
1110 GOTO 1070
1120 '.....fuel consumption
1130 X=2.352149E+02
1140 LOCATE LN
1150 IF D$<>"km."THEN 1170
1160 INPUT " ENTER: Fuel efficiency in litres per 100 km.";LKM:GOTO 1270
1170 INPUT " ENTER: Fuel efficiency in miles per gallon";MPG
1180 LOCATE LN:PRINT STRING$(79,32):LOCATE LN
1190 LKM=X/MPG
1200 G=D/MPG:R=G/T
1210 PRINT USING " Fuel efficiency........... ###.## miles/gallon";MPG;
1220 PRINT USING " (##.## litres/100 km)";LKM
1230 PRINT USING " Fuel consumption.......... ###.## gallons/hr.";R
1240 PRINT USING " Fuel required............. ###.## gallons";G
1250 GOTO 1350
1260 '.....metric
1270 LOCATE LN:PRINT STRING$(79,32):LOCATE LN
1280 MPG=X/LKM
1290 PRINT USING " Fuel efficiency........... ###.## litres/100 km";LKM;
1300 PRINT USING " (##.## miles/gallon)";MPG
1310 G=D*6.213999E-01/MPG:R=G/T
1320 RR=R*3.785000:GG=G*3.785000:
1330 PRINT USING " Fuel consumption.......... ###.## km/hr.";RR
1340 PRINT USING " Fuel required............. ###.## litres";GG
1350 '.....end
1360 IF D$="km."THEN 1400
1370 LOCATE 23
1380 PRINT "    Quantities are in U.S. gallons. Divide by 1.2 for Imperial ";
1390 PRINT "gallons."
1400 GOSUB 1410:GOTO 90
1410 '.....PRT
1420 KEY OFF:GOSUB 1490:LOCATE 25,5:COLOR 0,2
1430 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1440 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1450 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1450 ELSE GOSUB 1490
1460 IF Z$="3"THEN RETURN
1470 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1480 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1420
1490 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

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
230 IF Z$="1"THEN 260
240 IF Z$="2"THEN CLS:CHAIN"chase"
250 GOTO 220
260 GOSUB 590
270 PRINT " Press letter in < > to select Unit of Distance:"
280 PRINT L$;
290 PRINT "   < a >  Millimetre"
300 PRINT "   < b >  Centimetre"
310 PRINT "   < c >  Inch"
320 PRINT "   < d >  Foot"
330 PRINT "   < e >  Metre"
340 PRINT "   < f >  Kilometre"
350 PRINT "   < g >  Statute mile"
360 PRINT "   < h >  Nautical mile"
370 Z$=INKEY$:IF Z$=""THEN 370
380 IF Z$="a"THEN D$="mm."
390 IF Z$="b"THEN D$="cm."
400 IF Z$="c"THEN D$="in."
410 IF Z$="d"THEN D$="ft."
420 IF Z$="e"THEN D$="m."
430 IF Z$="f"THEN D$="km."
440 IF Z$="g"THEN D$="stat.miles "
450 IF Z$="h"THEN D$="naut.miles "
460 IF ASC(Z$)>=97 AND ASC(Z$)<=104 THEN 480
470 GOTO 370
480 PRINT L$;
490 PRINT " Press letter in < > to select unit of time:"
500 PRINT L$;
510 PRINT "   < h >  Hour"
520 PRINT "   < m >  Minute"
530 PRINT "   < s >  Second"
540 Z$=INKEY$:IF Z$="h"THEN T$="hr.":GOTO 580
550 IF Z$="m"THEN T$="min.":GOTO 580
560 IF Z$="s"THEN T$="sec.":GOTO 580
570 GOTO 540
580 GOSUB 590:GOTO 640
590 '.....erase line
600 FOR I=3 TO CSRLIN:LOCATE I:PRINT STRING$(80,32);:NEXT I:LOCATE 3:RETURN
610 '..... Enter command
620 PRINT " ENTER: ";STRING$(30,"."):RETURN
630 '.....data entry
640 IF D THEN 680
650 GOSUB 610:LOCATE CSRLIN-1,9:PRINT "Distance (";D$;")";
660 LOCATE CSRLIN,41:INPUT D
670 IF D THEN IF S+T THEN 790
680 IF S THEN 730
690 LOCATE 3:PRINT L$;
700 GOSUB 610:LOCATE CSRLIN-1,9:PRINT "Speed (";D$;;"/";T$;")";
710 LOCATE CSRLIN,41:INPUT S
720 IF S THEN IF D+T THEN 790
730 IF T THEN 640
740 GOSUB 610:LOCATE CSRLIN-1,9:PRINT "Time (";T$;")";
750 LOCATE CSRLIN,41:INPUT T
760 IF T THEN IF D+S THEN 790
770 GOTO 640
780 '.....calculate
790 IF D*S THEN T=D/S:GOTO 820
800 IF D*T THEN S=D/T:GOTO 820
810 IF S*T THEN D=S*T:GOTO 820
820 IF T$="sec." THEN TS=T:TM=T/60:TH=T/3600:GOTO 850
830 IF T$="min." THEN TS=T*60:TM=T:TH=T/60:GOTO 850
840 IF T$="hr."  THEN TS=T*3600:TM=T*60:TH=T:GOTO 850
850 GOSUB 590:LOCATE 2
860 HR=INT(TS/3600):BAL=TS-HR*3600
870 MIN=INT(BAL/60):SEC=BAL-MIN*60
880 SIC=INT(SEC):BAL=SEC-SIC
890 MIN$=RIGHT$(STR$(MIN),LEN(STR$(MIN))-1)
900 IF LEN(MIN$)<2 THEN MIN$="0"+MIN$:GOTO 900
910 SIC$=RIGHT$(STR$(SIC),LEN(STR$(SIC))-1)
920 IF LEN(SIC$)<2 THEN SIC$="0"+SIC$:GOTO 920
930 IF D$="naut.miles "THEN 960
940 PRINT L$;
950 PRINT " SPEED";USING P$;S;:PRINT " ";D$;"/ ";T$:GOTO 970
960 PRINT " SPEED";USING P$;S;:PRINT " knots"
970 PRINT L$;
980 '.....screen print
990 PRINT " TIME  ";USING "#########,##";HR;
1000 PRINT ":";MIN$;;":";SIC$
1010 PRINT "     =";USING P$;TH;:PRINT " hr.
1020 PRINT "     =";USING P$;TM;:PRINT " min.
1030 PRINT "     =";USING P$;TS;:PRINT " sec.
1040 PRINT L$;
1050 PRINT " DISTANCE";USING P$;D;:PRINT " ";D$
1060 PRINT L$;
1070 LN=CSRLIN:PRINT " Want to calculate vehicular fuel consumption?   (y/n)"
1080 Z$=INKEY$:IF Z$=""THEN 1080
1090 LOCATE LN:PRINT STRING$(79,32)
1100 IF Z$="n"THEN 1360
1110 IF Z$="y"THEN 1130
1120 GOTO 1080
1130 '.....fuel consumption
1140 X=2.352149E+02
1150 LOCATE LN
1160 IF D$<>"km."THEN 1180
1170 INPUT " ENTER: Fuel efficiency in litres per 100 km.";LKM:GOTO 1280
1180 INPUT " ENTER: Fuel efficiency in miles per gallon";MPG
1190 LOCATE LN:PRINT STRING$(79,32):LOCATE LN
1200 LKM=X/MPG
1210 G=D/MPG:R=G/T
1220 PRINT USING " Fuel efficiency........... ###.## miles/gallon";MPG;
1230 PRINT USING " (##.## litres/100 km)";LKM
1240 PRINT USING " Fuel consumption.......... ###.## gallons/hr.";R
1250 PRINT USING " Fuel required............. ###.## gallons";G
1260 GOTO 1360
1270 '.....metric
1280 LOCATE LN:PRINT STRING$(79,32):LOCATE LN
1290 MPG=X/LKM
1300 PRINT USING " Fuel efficiency........... ###.## litres/100 km";LKM;
1310 PRINT USING " (##.## miles/gallon)";MPG
1320 G=D*6.213999E-01/MPG:R=G/T
1330 RR=R*3.785000:GG=G*3.785000:
1340 PRINT USING " Fuel consumption.......... ###.## km/hr.";RR
1350 PRINT USING " Fuel required............. ###.## litres";GG
1360 '.....end
1370 IF D$="km."THEN 1410
1380 LOCATE 23
1390 PRINT "    Quantities are in U.S. gallons. Divide by 1.2 for Imperial ";
1400 PRINT "gallons."
1410 GOSUB 1420:GOTO 90
1420 '.....PRT
1430 KEY OFF:GOSUB 1500:LOCATE 25,5:COLOR 0,2
1440 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1450 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1460 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1460 ELSE GOSUB 1500
1470 IF Z$="3"THEN RETURN
1480 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1490 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1430
1500 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

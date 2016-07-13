1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'SKIPDIST - Skip Calculator - 16 DEC 1998
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,1
50 PI=4*ATN(1)  '3.141593
60 UL$=STRING$(80,205)
70 '
80 '.....title page
90 COLOR 15,2
100 PRINT " SKIP DISTANCE CALCULATOR";TAB(57);"by George Murphy VE3ERP ";
110 COLOR 1,0:PRINT STRING$(80,223);
120 PRINT
130 COLOR 7,0
140 '
150 '.....START
160 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
170 GOSUB 1160   'preface
180 LN=CSRLIN:LOCATE 21,TB
190 PRINT " Program suggested by Michel Berlie-Sarrazin, SWL/France"
200 PRINT
210 LOCATE ,TB:PRINT " Algorithm by Yardley Beers, W0JF"
220 COLOR 0,7:LOCATE,24
230 LOCATE LN+1
240 PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
250 Z$=INKEY$:IF Z$=""THEN 250
260 IF Z$="0"THEN CLS:RUN EX$
270 IF Z$="1"THEN 290
280 GOTO 250
290 LN=CSRLIN-1:VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
300 COLOR 0,7:PRINT " Want distances in (k)ilometres, (n)autical miles or ";
310 PRINT "(s)tatute miles?  (k/n/s) ";:COLOR 7,0
320 Z$=INKEY$:IF Z$=""THEN 320
330 IF Z$="k"THEN D$="kilometres":DK=2.485680E+04/4.000000E+04:GOTO 370
340 IF Z$="n"THEN D$="naut.miles":DK=2.485680E+04/21600:GOTO 370
350 IF Z$="s"THEN D$="stat.miles":DK=1:GOTO 370
360 GOTO 320
370 LN=CSRLIN-1:VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
380 INPUT " ENTER: Your latitude in decimal degrees (minus if South)......";LA
390 INPUT " ENTER: Your longitude in decimal degrees (minus if West)......";Z
400 LO=-Z
410 INPUT " ENTER: Azimuth of signal in decimal degrees from true north...";HD
420 B=HD
430 INC=16   'increment for display
440 PRINT " ENTER: Length of skip in ";D$;;"...........................";
450 INPUT SK:S=SK*DK*INC
460 GOTO 530
470 '
480 '.....conversion
490 IF LAT<0 THEN NS$="S"ELSE NS$="N"
500 IF LON<0 THEN EW$="W"ELSE EW$="E"
510 RETURN
520 '
530 '.....calculate
540 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
550 PRINT USING " Heading= ###.#๘ from True North";HD;
560 PRINT USING " ณ Skip=####.# ";SK;:PRINT D$+"ณ"
570 PRINT "อออออัออออออออออออออออออออัออออออฯอออออออออัอออออออออออออต"
580 PRINT " Skipณ Dist. from antenna ณ    Location    ณ Grid Square ณ"
590 PRINT "อออออุออออออออออออออออออออุออออออออออออออออุอออออออออออออต"
600 B=B*PI/180
610 LZ=LO
620 LO=-LO
630 D=90-LA
640 D=D*PI/180:LO=LO*PI/180
650 Q=S*PI/(180*6.904666E+01)
660 LAT=LA:LON=-LZ:GOSUB 480
670 PRINT TAB(6);"ณ";
680 PRINT TAB(27);USING "ณ###.#๘";ABS(LAT);
690 PRINT NS$;USING " ###.#๘";ABS(LON);
700 PRINT EW$;;" ณ   ";
710 GOSUB 1060:PRINT G$;;"  ฎฤฤฤ Antenna Location"
720 '
730 FOR K=1 TO INC
740 Q1=Q*K/INC
750 H1=COS(D)*COS(Q1)
760 H2=SIN(D)*SIN(Q1)*COS(B)
770 H=H1+H2
780 T=-ATN(H/(SQR(1-H^2)))+PI/2
790 X1=COS(Q1)-COS(D)*COS(T)
800 X2=SIN(D)*SIN(T):X=X1/X2
810 IF ABS(FIX(X*10^4))=10^4 THEN L=PI:GOTO 830
820 L=-PI/2-ATN(X/(SQR(1-X^2)))
830 N1=LO+L:N2=LO-L
840 T=90-T*180/PI
850 N1=180*N1/PI+180:N2=180*N2/PI+180
860 S1=S*K/INC
870 IF B>PI THEN 890
880 N=-N1:GOTO 900
890 N=-N2
900 IF N<-180 THEN 950
910 IF N>180 THEN 930
920 GOTO 960
930 N=N-360
940 GOTO 960
950 N=360+N
960 LAT=T:LON=-N:GOSUB 480
970 IF K=1 THEN X$=D$ ELSE X$="    "+CHR$(34)+"     "
980 PRINT USING "###  ณ##,###.# ";K,S1/DK;
990 PRINT X$;USING " ณ###.#๘";ABS(LAT);
1000 PRINT NS$;USING " ###.#๘";ABS(LON);
1010 PRINT EW$;;" ณ   ";
1020 GOSUB 1060:PRINT G$;;"    ณ"
1030 NEXT K
1040 GOTO 1260
1050 '
1060 '.....grid square
1070 W3=180+LON
1080 W1=INT(W3/20+E9)
1090 W2=INT((W3-20*W1)/2+E9)+48:W1=W1+65
1100 W3=INT(24*(W3/2-INT(W3/2)+E9))+65
1110 L1=INT((LAT+90)/10+E9):L2=INT(LAT+90+E9-10*L1)
1120 L3=INT((LAT+90-10*L1-L2)*24+E9):L1=L1+65:L2=L2+48:L3=L3+65
1130 G$=CHR$(W1)+CHR$(L1)+CHR$(W2)+CHR$(L2)+CHR$(W3)+CHR$(L3)
1140 RETURN
1150 '
1160 '.....preface
1170 TB=8
1180 LOCATE ,TB
1190 PRINT "Given a directional antenna heading and the length of skip, this"
1200 LOCATE ,TB
1210 PRINT "program computes the latitude, longitude, grid square number and"
1220 LOCATE ,TB
1230 PRINT "distance from the antenna to the nearest 16 skip touchdown points."
1240 RETURN
1250 '
1260 '.....end
1270 GOSUB 1290:GOTO 150
1280 '
1290 'PRT
1300 KEY OFF:GOSUB 1370:LOCATE 25,5:COLOR 0,2
1310 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1320 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1330 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1330 ELSE GOSUB 1370
1340 IF Z$="3"THEN RETURN
1350 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1360 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1300
1370 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'WINDCHIL - Wind Chill Factor - 16 NOV 2005 rev. 23 FEB 2010
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 PROG$="windchil"
60 
70 '.....start
80 CLS
90 COLOR 15,2
100 PRINT " WINDCHILL FACTOR";TAB(57);"by George Murphy VE3ERP ";
110 COLOR 1,0:PRINT STRING$(80,223);
120 COLOR 7,0
130 LN=CSRLIN
140 PRINT " ref: http//ourworld.compuserve.com/homepages/Gene_Nygaard/windch";
150 PRINT "ill.htm"
160 PRINT STRING$(80,205);
170 PRINT " Press <1) to run program"
180 PRINT " Press <2> to run Humidex Calculator"
190 PRINT " Press <0> to EXIT"
200 Z$= INKEY$:IF Z$="" THEN 200
210 IF Z$="0"THEN RUN EX$
220 IF Z$="1"THEN 240
225 IF Z$="2"THEN CHAIN"humid"
230 GOTO 200
240 GOSUB 620
250 COLOR 15,0:PRINT " Want (c)entigrade or (f)ahrenheit ? (c/f)";:COLOR 7,0
260 U$=INKEY$:IF U$<>"c"AND U$<>"f"THEN 260
270 GOSUB 620
280 T$="øC":IF U$="f"THEN T$="øF"
290 PRINT " ENTER actual air temperarture in ";T$;:INPUT T
300 IF U$="f"THEN T=(T-32)/1.799999
310 GOSUB 620
320 LN=CSRLIN
330 M$=" 6.5 to  90.0 km/hr."
340 IF U$="f"THEN M$=" 4.1 to 55.9 mph"
350 COLOR 14,0
360 PRINT " Results not valid for wind speeds outside the range of";M$
370 PRINT
380 COLOR 7,0:LN=CSRLIN
390 W$="km/hr":IF U$="f"THEN W$="mph"
400 PRINT "ENTER: estimated wind speed in ";W$;:INPUT W
410 IF U$="f"THEN W=W*1.608999:W$="mph"
420 IF W<6.500000 OR W>90 THEN BEEP:GOSUB 620:GOTO 400
430 GOSUB 620
440 '.....calculate
450 MS=W*10^3/60^2         'metres per second
460 IF MS<1.788159 OR MS>25 THEN BEEP:GOSUB 620:GOTO 400
470 WI=(1.044999E+01+(10*(SQR(MS)))-MS)*(33-T)  'equation
480 WT=(-4.543999E-02*WI)+33      'wind temp Celcius
490 COLOR 11,0
500 IF U$="c"THEN X=T
510 IF U$="f"THEN W$="mph":W=W/1.608999:X=T*1.799999+32:WT=WT*1.799999+32
520 PRINT USING " Wind speed= ###.# ";W; : print w$
530 PRINT USING " .......Air temp.= ###.#";X;: print T$;
540 PRINT USING ", but feels like ###.#";WT;: print T$
550 COLOR 7,0
560 PRINT
570 LN=CSRLIN:PRINT  " Want to change wind speed?   (y/n)";
580 Z$=INKEY$:IF Z$=""THEN 580
590 IF Z$="n"THEN GOSUB 620:GOSUB 650:GOTO 70
600 IF Z$="y"THEN GOSUB 620:GOTO 400
610 GOTO 580
620 '.....erase previous line
630 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
640 RETURN
650 '.....PRT
660 KEY OFF:GOSUB 730:LOCATE 25,5:COLOR 0,2
670 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
680 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
690 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 690 ELSE GOSUB 730
700 IF Z$="3"THEN RETURN
710 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
720 IF Z$="2"THEN LPRINT CHR$(12) ELSE 660
730 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

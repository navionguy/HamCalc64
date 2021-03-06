1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$
10 'COILAMP - 02 FEB 05
20 CLS:KEY OFF
30 
40 PI=4*ATN(1)  '3.141593
50 UL$=STRING$(80,205)
60 GOSUB 400
70 LN=CSRLIN
80 PRINT " Press 1 to continue or 0 to EXIT"
90 Z$=INKEY$:IF Z$=""THEN 90
100 IF Z$="0"THEN CLS:RUN EX$
110 IF Z$="1"THEN 130
120 GOTO 90
130 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
140 INPUT " ENTER: Frequency (MHz)....................";F
150 INPUT " ENTER: Coil imductance (�H)...............";L
160 INPUT " ENTER: Whip length in electrical degrees..";H
170 RR=H^2/312
180 INPUT " ENTER: Estimated radiated power (watts)...";P
190 I=SQR(P/RR)
200 E=I*RR
210 IN=H*3.279999E+01/F
220 W=SQR(700*I)/10^3
230 K#=(4.600000E-01/4.999999E-03)^(1/39)
240 AWG=LOG(4.600000E-01/W)/(LOG(K#))-3
250 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
260 PRINT USING " Frequency.................. ###.### MHz";F
270 PRINT USING " Coil inductance............ ###.### �H";L
280 PRINT USING " Whip electrical length..... ###.###� ( ###.# inches )";H;IN
290 PRINT USING " Radiation resistance....... ###.### �";RR
300 PRINT USING " Effective radiated power... ###.### watts";P
310 PRINT USING " Current through coil....... ###.### amps";I
320 PRINT USING " Voltage.................... ###.### volts";E
330 PRINT USING " Minimum wire diameter...... ###.### inches";W;
340 PRINT USING " (use AWG ## or larger)";AWG
350 PRINT UL$;
360 GOSUB 500:GOTO 10
370 END
380 '
390 '.....prologue
400 PRINT " This program is primarily intended as a guide in choosing wire si";
410 PRINT "zes for small"
420 PRINT " loading coils in base loaded mobile/marine antennas with effectiv";
430 PRINT "e radiated   "
440 PRINT " power at QRP levels. For coils wound on PVC pipe form a whip leng";
450 PRINT "th between   "
460 PRINT " 20 and 30 electrical degrees is recommended."
470 PRINT UL$;
480 RETURN
490 '
500 '.....PRT
510 KEY OFF:GOSUB 580:LOCATE 25,5:COLOR 0,2
520 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
530 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
540 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 540 ELSE GOSUB 580
550 IF Z$="3"THEN RETURN
560 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
570 IF Z$="2"THEN LPRINT CHR$(12) ELSE 510
580 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
590 PRINT USING " Effective radiated power... ###.### watts";P

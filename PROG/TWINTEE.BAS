1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'TWINTEE - Twin-T Audio Oscillator - 03 AUG 1998
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,0
50 PI=4*ATN(1)  '3.141593
60 UL$=STRING$(80,205)
70 '
80 '.....title page
90 CLS:KEY OFF
100 COLOR 15,2
110 PRINT " AUDIO OSCILLATOR (Twin-T)";TAB(62);;"by Bob Stein W6NBI ";
120 PRINT STRING$(80,32);
130 LOCATE CSRLIN-1,20:COLOR 15
140 PRINT "Edited for HAMCALC by George Murphy VE3ERP"
150 COLOR 1,0:PRINT STRING$(80,223);
160 COLOR 7,0
170 GOSUB 560
180 PRINT UL$;
190 LN=CSRLIN
200 PRINT " This program designs a Twin-T audio-frequency oscillator, using ";
210 PRINT "an empirically"
220 PRINT " designed formula derived by Bob Stein, W6NBI."
230 PRINT
240 COLOR 0,7:LOCATE ,24:PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
250 Z$=INKEY$:IF Z$=""THEN 250
260 IF Z$="0"THEN CLS:RUN EX$
270 IF Z$="1"THEN 300
280 GOTO 250
290 '
300 '.....input data
310 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
320 INPUT " ENTER: Frequency in Hz ....................................";F
330 INPUT " ENTER: Starting value of C1 & C2 in æF (.01 is optimum)....";Z
340 C1=Z*10^-6:C2=C1
350 C4=2*C1
360 R1=3300
370 R2=(5.000000E-01/F)*SQR(1/(C1*C4))
380 R3=R2
390 R4=R2/10
400 C3=1/(2*PI*5000*F)
410 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
420 J=30
430 LOCATE ,J:PRINT USING " Freq. = ####### Hz";F
440 PRINT
450 LOCATE ,J:PRINT USING " C1,C2 = ###.### æF";C1*10^6
460 LOCATE ,J:PRINT USING " C3    = ###.### nF (minimum)";C3*10^9
470 LOCATE ,J:PRINT USING " C4    = ###.### æF";C4*10^6
480 PRINT
490 LOCATE ,J:PRINT USING " R1    = ###,### ê";R1
500 LOCATE ,J:PRINT USING " R2,R3 = ###,### ê";R2
510 LOCATE ,J:PRINT USING " R4    = ###,### ê";R4
520 '
530 '.....end
540 GOSUB 700:GOTO 80
550 END
560 '.....diagram
570 COLOR 0,7:Y=15
580 LOCATE ,Y:PRINT "                          R2                        "
590 LOCATE ,Y:PRINT "              ÚÄÄÄÄÄÄÂÄÄ\/\/\ÄÄÄÄÄ¿                 "
600 LOCATE ,Y:PRINT "         R1   ³   C1ÍØÍ   R4      ³      C3         "
610 LOCATE ,Y:PRINT "  + ÄÄÄ\/\/\ÄÄ´      ÃÄÄ\/\/\ÄÄ¿  ÃÄÄÄÂÄÄ×ÄÄÄÄÄ     "
620 LOCATE ,Y:PRINT " Vcc        Q1À\³ C2ÍØÍ   R3   ³  ³   ³      Hi-Z   "
630 LOCATE ,Y:PRINT "  - ÄÄ¿     NPN ÃÄÄÄÄÁÄÄ\/\/\ÄÄ³ÄÄÙ  ÍØÍ C4  Output "
640 LOCATE ,Y:PRINT "      ³       Ú/³              ³      ³     ÚÄÄ     "
650 LOCATE ,Y:PRINT "      ³       ³                ³      ³     ³       "
660 LOCATE ,Y:PRINT "     \\\     \\\              \\\    \\\   \\\      "
670 COLOR 7,0
680 RETURN
690 '
700 '.....PRT
710 KEY OFF:GOSUB 780:LOCATE 25,5:COLOR 0,2
720 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
730 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
740 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 740 ELSE GOSUB 780
750 IF Z$="3"THEN RETURN
760 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
770 IF Z$="2"THEN LPRINT CHR$(12) ELSE 710
780 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

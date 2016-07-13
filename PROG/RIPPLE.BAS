1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'RIPPLE - 07 NOV 2007
20 KEY OFF
30 
40 IF EX$=""THEN EX$="EXIT"
50 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
60 'PROG$="ripple"
70 '
80 '.....title page
90 CLS:COLOR 15,2,1
100 PRINT " RIPPLE CALCULATOR";TAB(57);"by George Murphy VE3ERP ";
110 COLOR 1,0:PRINT STRING$(80,223);
120 COLOR 15,2:LOCATE CSRLIN-1,28:PRINT " algorithm by R.J.DeHoney "
130 COLOR 7,0,0
140 PRINT
150 PRINT " Press 1 to continue or 0 to exit."
160 Z$=INKEY$:IF Z$="0"THEN RUN GO$
170 IF Z$="1"THEN LOCATE CSRLIN-1:PRINT STRING$(79,32):GOTO 200
180 GOTO 160
190 COLOR 7,0,0
200 LOCATE CSRLIN-1
210 A1=5.000000E-01 :A2=5.600000E-02 :A3=5.800000 :F=60
220 PRINT "ENTER desired output (volts)................";:INPUT VDC
230 PRINT "ENTER desired current (amps)................";:INPUT IDC
240 PRINT "ENTER maximum peak to peak ripple (volts)...";:INPUT VRIP
250 C=IDC*6.000000E-01/2/F/VRIP
260 PRINT :PRINT "      Minimum value of C =";INT(C*1.000000E+06);"uF
270 PRINT :PRINT "Transformer secondary    Required secondary
280 PRINT " winding resistance        RMS voltage";
290 FOR RSEC=2.500000E-01 TO 1.600000 STEP 2.500000E-01
300 VD=A1+A2*LOG(4000*IDC+A3)
310 VRMS=(VDC+2*VD+IDC*2.500000*2*RSEC+VRIP/2)/SQR(2)
320 PRINT USING "      ##.##             ";RSEC;VRMS
330 NEXT RSEC
340 PRINT "ENTER transformer secondary RMS voltage.........";:INPUT VRMS
350 PRINT "ENTER transformer secondary resistance (ohms)...";:INPUT RSEC
360 PRINT "ENTER filter capacitor value (Êf)...............";:INPUT C
370 C=C*9.999999E-07
380 CLS
390 PRINT USING "Transformer secondary voltage    ####,###.### volts";VRMS
400 PRINT USING "Transformer secondary resistance ####,###.### ohms";RSEC
410 PRINT USING "Filter capacitor.                ####,### ÊF";C/9.999999E-07
420 PRINT
430 PRINT " Amps    Volts     Ripple"
440 READ IDC :IF IDC=0 THEN 510
450 VD=A1+A2*LOG(4000*IDC+A3)
460 VRIP=6.000000E-01*IDC/2/F/C
470 VDC=SQR(2)*VRMS-2*VD-IDC*2.500000*2*RSEC-VRIP/2 :IF VDC=<0 THEN 440
480 PRINT USING "##.##    ";IDC;VDC;:PRINT USING "##.1 volts";VRIP
490 GOTO 440
500 DATA .01,.03,.1,.2,.5,1,1.5,3,5,10,0
510 '.....end
520 GOSUB 530:GOTO 80
530 '.....PRT
540 KEY OFF:GOSUB 610:LOCATE 25,5:COLOR 0,2
550 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
560 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
570 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 570 ELSE GOSUB 610
580 IF Z$="3"THEN RETURN
590 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
600 IF Z$="2"THEN LPRINT CHR$(12) ELSE 540
610 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

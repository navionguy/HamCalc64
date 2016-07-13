1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'QSK Break-in module -16 DEC 2010
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 '.....start
70 PRINT ";
80 PRINT " +12v,DC¯ ÄÄÄÄS1ÄÄÄÂÄÄÄÄ¿ K1n.o.ÄÄ*¿+DC to transmitter";
90 PRINT "                  R1®¿  ³ K1c.p.ÄÄ*Ù+DC from source";
100 PRINT "                   ÃÄÙ  ÃÄÂÄ´<¿";
110 PRINT "                  R2 ÚÄÄÁÄÁ¿D1³K1";
120 PRINT " *ÄÄPTT line K2c.p.ÃÄ´6 8 4³  ³coil";
130 PRINT " *ÄÄPTT line K2n.o.ÃÄ´7 U 2ÃÄÄÁÄÄÄ*¿";
140 PRINT "           Ú>ÃÄÄÂÄÄ³Ä´3   5³       key";
150 PRINT "           ³D2 K2  ³ ÀÂÄÄÄÂÙ  ÚÄÄÄ*Ù";
160 PRINT "           ³  coil C2 ³  C1   ³";
170 PRINT " -12v.DC¯  Ï    Ï  Ï  Ï   Ï   Ï";
180 PRINT ";
190 PRINT " PTT=    Push-to-Talk";
200 PRINT " n.o.=   relay normally open contact";
210 PRINT " c.p.=   relay centre pole contact";
220 PRINT " S1 =    SPST switch";
230 PRINT " K1,K2 = Reed relay Radio Shack #275-233";
240 PRINT " R1 =    10Kê pot Radio Shack #271-234";
250 PRINT " U  =    555 timer";
260 PRINT " D1,S2 = 1N914 silicon diode";
270 PRINT " R2 =    4.7 Kê resistor";
280 PRINT " C1 =    .01 æF disc ceramic capacitor";
290 PRINT " C2 =    10 æF electrolytic capacitor";
300 T=42
310 LOCATE 5,T:PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ";
320 LOCATE 6,T:PRINT " ³ This simple device fools you into";
330 LOCATE 7,T:PRINT " ³ thinking your rig has full break-in";
340 LOCATE 8,T:PRINT " ³ capabilities when it really hasn't.";
350 LOCATE 9,T:PRINT " ³ At the first tap of your key, four";
360 LOCATE 10,T:PRINT " ³ things happen:";
370 LOCATE 11,T:PRINT " ³ 1. K2 activates your PTT circuit.";
380 LOCATE 12,T:PRINT " ³ 2. U turns on, latching K2.";
390 LOCATE 13,T:PRINT " ³ 3. K1 keys your transmitter.";
400 LOCATE 14,T:PRINT " ³ 4. After pause set by R1 timer turns";
410 LOCATE 15,T:PRINT " ³    off and rig returns to Receive.";
420 LOCATE 16,T:PRINT " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ";
430 LOCATE 22,T::PRINT "          QSK BREAK-IN MODULE";
440 LOCATE 23,T::PRINT "          ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ";
450 KEY OFF:GOSUB 520:LOCATE 25,5:COLOR 0,2
460 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
470 PRINT "(3)QUIT? (1/2/3)";:COLOR 7,0
480 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 480 ELSE GOSUB 520
490 IF Z$="3"THEN CLS:RUN EX$
500 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
510 IF Z$="2"THEN LPRINT CHR$(12) ELSE 450
520 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

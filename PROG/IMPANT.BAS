1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'IMPANT  Antenna Impedance Calculator - 19 MAR 08
20 KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PI=4*ATN(1)
70 '.....start
80 CLS
90 COLOR 15,2
100 PRINT " ANTENNA IMPEDANCE CALCULATOR";TAB(61);"by R.J.Dehoney IEEE ";
110 COLOR 1,0:PRINT STRING$(80,223);
120 COLOR 15,2:LOCATE CSRLIN-1,25:PRINT " (suggested by Gerd Wenz DJ4IB) "
130 COLOR 7,0
140 PRINT
150 GOTO 180
160 '.....clear to bottom
170 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN:RETURN
180 '.....start
190 PRINT " This program calculates the input impedance of dipoles in free space and";
200 PRINT " monopoles over good ground. It is based on the formulas developed by Quang,";
210 PRINT " Tieng, and Gunn as reported in their Jan 1993 IEEE TAP paper `Equivalent";
220 PRINT " Circuit of a Dipole Antenna Using Frequency Independent Lumped Elements.'";
230 PRINT
240 PRINT TAB(20);" ....Press 1 to continue or 0 to exit...."
250 Z$=INKEY$:IF Z$=""THEN 250
260 IF Z$="0"THEN CLS:RUN GO$
270 IF Z$="1"THEN CLS:GOTO 290
280 GOTO 250
290 PRINT "                ANTENNA IMPEDANCE"
300 PRINT
310 PRINT " ÚÄÄÄÄÂÄÄÄÄÂÄCBÄÄ*"
320 PRINT " RA   LA   CA         Equivalent circuit of dipole"
330 PRINT " ÀÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄ*"
340 PRINT
350 LN=CSRLIN
360 INPUT " ENTER: Operating Frequency (MHz)";F :B=300/F   'B=band in metres
370 LN=LN+1:GOSUB 160
380 IF F<1 THEN BEEP:GOSUB 160:GOTO 350
390 FQ=INT(F*100)/100:B=INT(B*100)/100
400 LOCATE CSRLIN-1:PRINT " Center Frequency =";FQ;"MHz  (";B;"metres )"
410 H=B/2
420 PRINT " Half Wavelength =";H;"metres (";H/3.048000E-01 ;"feet )"
430 LN=CSRLIN:PRINT " Maximum element diameter =";H/50*10^3;"mm.";
440 INPUT " ENTER: Diameter of Dipole Elements (mm.)";Z
450 D=Z/10^3
460 IF Z>H/50*10^3 THEN BEEP:GOSUB 160:GOTO 430 ELSE LN=LN+1:GOSUB 160
470 IN=INT(D*3.936999E+01*1000)/1000
480 LOCATE 8,1 :PRINT " Dipole Length=";H;"meters (";H*3.936999E+01;"inches)    "
490 Z1=H*20 :IF Z1>Z THEN Z1=Z
500 LOCATE 9,1 :PRINT " Element Diameter=";Z1;"mm (";Z1/2.539999E+01;"inches)"; :PRINT
510 S=2*H/D :LS=LOG(S)/LOG(10)
520 RA=4.128800E+02*LS^2+7.407540E+03*S^(-2.388999E-02)-7.274080E+03
530 PRINT " RA =";RA;"ohms"
540 CA=H*(8.907499E-01/(LS^8.005999E-01-8.610000E-01)-2.541000E-02)
550 PRINT " CA =";CA;"pF"
560 LA=H*(1.481300E+02*(LS^1.011999)-6.188000E+01)
570 PRINT " LA =";LA;"nH"
580 CB=1.206739E+01*H/2/(LS-7.245000E-01)
590 PRINT " CB =";CB;"pF"
600 PRINT
610 FX=F*10^6
620 WX=FX*2*3.141592700000000
630 L1=LA*9.999999E-10 :C1=CA*9.999999E-13 :C2=CB*9.999999E-13 :Y1=WX*C1-1/WX/L1 :X1=-1/Y1
640 DENOM=RA^2+X1^2 :RE=RA*X1^2/DENOM :IM=RA^2*X1/DENOM-1/WX/C2
650 R$=STR$(RE):X$=STR$(ABS(IM)):IF IM<0 THEN B$="-j"ELSE B$="+j
660 PRINT " Impedance: ";R$;B$;X$
670 PRINT USING " Resistive part: ####.## ohms";RE
680 PRINT USING " Reactive part: +####.## ohms";IM
690 IF IM<0 THEN C=-10^6/(2*PI*F*IM) ELSE 710
700 PRINT USING " Capacitance:    ####.## pF    ";C :GOTO 730
710 IF IM>0 THEN L=IM/(2*PI*F)
720 PRINT USING " Inductance=     ####.## nH   ";L*10^3
730 PRINT:COLOR 0,7
740 PRINT " Values not valid if height above ground is less than";B;"metres"
750 COLOR 7,0 :PRINT
760 PRINT "Press `l' or `s' to lengthen or shorten the dipole or `c' to continue";
770 A$=INKEY$ :IF A$="" THEN 770
780 K=1.000000E-03 :IF H=>6.000000E-01*B THEN K=0
790 IF A$="l" THEN H=H*(1+K) :GOTO 480
800 IF A$="s" THEN H=H/(1.001000) :GOTO 480
810 '.....finish
820 LOCATE 23:PRINT STRING$(80,32);
830 GOSUB 840:CLS:GOTO 70
840 '.....PRT
850 KEY OFF:GOSUB 920:LOCATE 25,5:COLOR 0,2
860 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
870 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
880 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 880 ELSE GOSUB 920
890 IF Z$="3"THEN RETURN
900 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
910 IF Z$="2"THEN LPRINT CHR$(12) ELSE 850
920 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
930 END

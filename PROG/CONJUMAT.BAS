1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'CONJUMAT - Conjugate Match Calculator - 15 MAR 96 rev. 05 MAR 1999
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 COLOR 7,0,0
70 UL$=STRING$(80,205)
80 B$=STRING$(80,32)
90 U$="#####.###"
100 E=2.718580     'base of natural logarithms
110 GOTO 180
120 '
130 '.....format input line
140 LOCATE CSRLIN-1:PRINT SPC(7);
150 LOCATE CSRLIN,47:PRINT STRING$(7,".");USING U$;ZZ;
160 RETURN
170 '
180 '.....start
190 CLS
200 COLOR 15,2
210 PRINT " CONJUGATE MATCH CALCULATOR";TAB(57);"by George Murphy VE3ERP ";
220 COLOR 1,0:PRINT STRING$(80,223);
230 COLOR 7,0
240 '
250 T=7
260 PRINT TAB(T);
270 PRINT "This program is a basic design tool to help determine the conditions"
280 PRINT TAB(T);
290 PRINT "existing when connecting a transmitter to a complex load, such as an"
300 PRINT TAB(T);
310 PRINT "antenna, via a transmission line of known characteristics."
320 PRINT
330 PRINT TAB(T);
340 PRINT "Program < 1 > will ask you to enter the feedline attenuation in dB,"
350 PRINT TAB(T);
360 PRINT "if known. You may be able to calculate this value by using programs"
370 PRINT TAB(T);
380 PRINT "< 3 >, < 4 >, or < 5 >."
390 PRINT UL$;
400 PRINT " Press number in < > for:"
410 PRINT UL$;
420 Z$=CHR$(34)
430 PRINT "   < 1 >  Conjugate Match program"
440 PRINT "   < 2 >  AUTEK RF Analyst - meter reading conversions"
450 PRINT
460 PRINT "   < 3 >  ";Z$;"COAXIAL CABLE CHARACTERISTICS";Z$;" program"
470 PRINT "   < 4 >  ";Z$;"TRANSMISSION LINE LOSSES";Z$;" program"
480 PRINT "   < 5 >  ";Z$;"TRANSMISSION LINES - OPEN WIRE";Z$;" program"
490 PRINT
500 PRINT "   < 0 >  EXIT"
510 '
520 Z$=INKEY$:IF Z$=""THEN 520
530 IF Z$="0"THEN CLS:CHAIN GO$
540 IF Z$="1"THEN 610
550 IF Z$="2"THEN CLS:GOTO 1500
560 IF Z$="3"THEN CLS:CHAIN "coaxchar"
570 IF Z$="4"THEN CLS:CHAIN "lineloss"
580 IF Z$="5"THEN CLS:CHAIN "openwire"
590 GOTO 520
600 '
610 '.....inputs
620 CLS
630 GOSUB 1350    'diagram
640 '
650 COLOR 15
660 INPUT " ENTER: Transmitter output....................(watts)";W
670 ZZ=W:GOSUB 130:PRINT " watts"
680 COLOR 7
690 '
700 INPUT " ENTER: Characteristic impedance of feedline.. (ohms)";ZO
710 ZZ=ZO:GOSUB 130:PRINT " ohms"
720 '
730 INPUT " ENTER: Complex load resistive component......(ohms)";R
740 ZZ=R:GOSUB 130:PRINT " ohms"
750 '
760 COLOR 0,7
770 PRINT " Is the reactive component (i)nductive or (c)apacitive?   (i/c) "
780 COLOR 7,0
790 Z$=INKEY$:IF Z$=""THEN 790
800 IF Z$="i"THEN J$="+ j":Z$="inductive component..":GOTO 830
810 IF Z$="c"THEN J$="- j":Z$="capacitive component.":GOTO 830
820 GOTO 790
830 LOCATE CSRLIN-1:PRINT B$;:LOCATE CSRLIN-1
840 '
850 PRINT " ENTER: Value of ";Z$;;".........(ohms)";:INPUT X
860 LN=CSRLIN-2:VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
870 R=R/ZO:X=X/ZO
880 B=((X^2+1)/R)+R
890 SWR=(B+SQR(B^2-4))/2
900 X$=STR$(ABS(X*ZO)):X$=RIGHT$(X$,LEN(X$)-1)
910 PRINT "        Complex load impedance.......................";
920 '
930 PRINT R*ZO;J$;X$;;" ohms"
940 PRINT "        SWR at load R................................";USING U$;SWR;
950 PRINT ":1"
960 '
970 P=(SWR-1)/(SWR+1)   'reflection coefficient
980 PRINT "        Reflection coefficient.......................";USING U$;P
990 '
1000 INPUT " ENTER: Attenuation of feedline when matched.....(dB)";LM
1010 ZZ=LM:GOSUB 130:PRINT " dB"
1020 BB=10^(LM/10)
1030 DB=10*LOG((BB^2-P^2)/(BB*(1-P^2)))/LOG(10)   'converted to base 10
1040 '
1050 PRINT "        Additional loss due to SWR...................";
1060 PRINT USING U$;DB-LM;:PRINT ;" dB"
1070 PRINT "        TOTAL feedline Attenuation ..................";USING U$;DB;
1080 PRINT " dB"
1090 A=DB/8.685999    'attenuation in nepers
1100 PRINT TAB(53);;"=";:PRINT USING U$;A;:PRINT ;" nepers"
1110 '
1120 SI=(BB+P)/(BB-P)
1130 PRINT "        SWR at conjugate matching point..............";USING U$;SI;
1140 PRINT ":1"
1150 '
1160 W1=W*1/(1-P^2*E^(-(4*A)))
1170 PRINT "        Forward power at conjugate matching point....";USING U$;W1;
1180 PRINT " watts"
1190 '
1200 W2=W*E^-(2*A)/(1-P^2*E^(-(4*A)))
1210 PRINT "        Forward power arriving at mismatched load R..";USING U$;W2;
1220 PRINT " watts"
1230 '
1240 W3=W*(1-P^2)*E^(-(2*A))/(1-P^2*E^(-(4*A)))
1250 COLOR 15
1260 PRINT "        Power absorbed in the load R.................";USING U$;W3;
1270 PRINT " watts"
1280 COLOR 7
1290 '
1300 W4=W*P^2*E^(-(2*A))/(1-P^2*E^(-(4*A)))
1310 PRINT "        Power Reflected..............................";USING U$;W4;
1320 PRINT " watts";
1330 GOTO 1790
1340 '
1350 '.....diagram
1360 T=21:COLOR 0,7
1370 LOCATE ,T:PRINT "            CONJUGATE MATCH           "
1380 LOCATE ,T:PRINT "                                      "
1390 LOCATE ,T:PRINT "       낟컴컴컴 feedline 컴컴컴��     "
1400 LOCATE ,T:PRINT "   旼컴�                        旼컴� "
1410 LOCATE ,T:PRINT " �캑 T �컴컴컴컴컴컴컴컴컴컴컴컴� R � "
1420 LOCATE ,T:PRINT "   읕컴�                        읕컴� "
1430 LOCATE ,T:PRINT " T = Transmatch or transmitter output "
1440 LOCATE ,T:PRINT " � = Conjugate matching point         "
1450 LOCATE ,T:PRINT " R = Complex load (e.g. an antenna)   "
1460 LOCATE ,T:PRINT "                                      "
1470 COLOR 7,0
1480 RETURN
1490 '
1500 '.....AUTEK conversions
1510 'from AUTEK.BAS by Erik Madsen OZ8EM
1520 CLS:KEY OFF:PI=4*ATN(1)
1530 PRINT " AUTEK METER READINGS:";
1540 PRINT TAB(47);;"(Algorithm by Erik Madsen, OZ8EM)"
1550 LN=CSRLIN
1560 INPUT "  ENTER: Impedance value reading (ohms)...";Z
1570 INPUT "  ENTER: SWR reading (ref. 50� feedline)..";SWR
1580 RHO=(SWR-1)/(SWR+1):  RA=(Z^2/100+25)*(1-RHO^2)/(1+RHO^2)
1590 IF RA>Z THEN PRINT "IMPOSSIBLE!";: GOTO 1750
1600 XA=SQR(Z^2-RA^2):  FI=180/PI*ATN(XA/RA)
1610 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
1620 PRINT USING "  Impedance reading ..... ####.# �";Z
1630 PRINT USING "  SWR/50� reading ....... ####.#:1";SWR
1640 PRINT:PRINT ;" CONVERSIONS:";
1650 PRINT USING "  Phase angle ........... ####.##�";FI
1660 Y$=STR$(CINT(RA)):RA$=RIGHT$(Y$,LEN(Y$)-1)
1670 Y$=STR$(CINT(XA)):XA$=RIGHT$(Y$,LEN(Y$)-1)
1680 J$=RA$+"�j"+XA$
1690 PRINT USING "  Resistive Component ... ####.## �";RA
1700 PRINT USING "  Reactive Component .... �###.## �";XA
1710 PRINT:COLOR 0,7:PRINT ;" COMPLEX IMPEDANCE = ";J$;:PRINT ;" � "
1720 COLOR 7,0:PRINT
1730 PRINT " (The Autek does not differentiate between inductive and capaciti";
1740 PRINT "ve reactance.)"
1750 PRINT:COLOR 0,7:LOCATE 25,34:PRINT ;" More? (y/n)";:COLOR 7,0
1760 S$=INKEY$:IF S$="y"THEN 1520 ELSE IF S$="n"THEN 1790
1770 GOTO 1760
1780 '
1790 '.....end
1800 GOSUB 1840
1810 GOTO 180
1820 END
1830 '
1840 'PRT
1850 KEY OFF:GOSUB 1920:LOCATE 25,5:COLOR 0,2
1860 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1870 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1880 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1880 ELSE GOSUB 1920
1890 IF Z$="3"THEN RETURN
1900 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1910 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1850
1920 LOCATE 25,1:PRINT STRING$(80,32);:RETURN

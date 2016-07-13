1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON UL$,U$,PI,EX$,CKT$,T$,PROG$
10 'DSGNMENU - R/L/C/ Circuits Menu - 05 JUL 96 rev.11 APR 97
20 IF EX$=""THEN EX$="EXIT"
25 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
30 
40 CLS:KEY OFF:COLOR 7,0,1
50 UL$=STRING$(80,205)
60 U$="#####,###.###"
80 PI=3.141592
90 '
100 COLOR 15,2
110 PRINT " RESISTOR/INDUCTOR/CAPACITOR CIRCUIT DESIGN";
120 PRINT TAB(57);;"by George Murphy VE3ERP ";
130 COLOR 1,0:PRINT STRING$(80,223);:COLOR 7,0
140 LOCATE 3:K=15:GOSUB 400
150 PRINT
160 PRINT " Press number in ( ) for:"
170 PRINT UL$;
180 PRINT "   (1) R/C Circuit Design"
190 PRINT "   (2) R/L Circuit Design"
200 PRINT "   (3) L/C Circuit Design"
210 PRINT "   (4) R/L/C Circuit Design"
220 PRINT
230 PRINT "   (0) EXIT";
240 LOCATE 16,31:PRINT "з";STRING$(46,"д");;"©"
250 LOCATE 17,31:PRINT "Ё This program is based on data presented in   Ё"
260 LOCATE 18,31:PRINT "╣ ";CHR$(34);;"Programs for Electronic Circuit Design";
270 PRINT CHR$(34);;" by  ф"
280 LOCATE 19,31:PRINT "Ё David Leithauser (Radio Shack Cat.# 62-1054) Ё"
290 LOCATE 20,31:PRINT "ю";STRING$(46,"д");;"ы"
300 Z$=INKEY$:IF Z$=""THEN 300
310 IF Z$="0"THEN CLS:CHAIN GO$
320 IF Z$="1"THEN CLS:CKT$="RC":T$=" RESISTOR/CAPACITOR CIRCUITS":CHAIN"dsgnrc"
330 IF Z$="2"THEN CLS:CKT$="RL":T$=" RESISTOR/INDUCTOR CIRCUITS":CHAIN"dsgnrc"
340 IF Z$="3"THEN CLS:CKT$="LC":T$=" INDUCTOR/CAPACITOR CIRCUITS":CHAIN"dsgnrc"
350 IF Z$="4"THEN CLS:CKT$="RLC":CHAIN"dsgnrlc"
360 GOTO 300
370 '
380 END
390 '
400 '.....R/C diagrams
410 COLOR 0,7
420 LOCATE ,K:PRINT "               здддRддд©                            "
430 LOCATE ,K:PRINT " ╞ддRдддCдд╞  ╞╢       ц╞       R/C CIRCUITS        "
440 LOCATE ,K:PRINT "               юдддCддды                            "
450 LOCATE ,K:PRINT "               здддRддд©                            "
460 LOCATE ,K:PRINT " ╞ддRдддLдд╞  ╞╢       ц╞       R/L CIRCUITS        "
470 LOCATE ,K:PRINT "               юдддLддды                            "
480 LOCATE ,K:PRINT "               здддLддд©                            "
490 LOCATE ,K:PRINT " ╞ддLдддCдд╞  ╞╢       ц╞       L/C CIRCUITS        "
500 LOCATE ,K:PRINT "               юдддCддды                            "
510 LOCATE ,K:PRINT "               здддRддд©    здRдддLд©    здRдддLд©  "
520 LOCATE ,K:PRINT " ╞дRддLддCд╞  ╞едддLддде╞  ╞╢       ц╞  ╞╢       ц╞ "
530 LOCATE ,K:PRINT "               юдддCддды    юдддCддды    юдRдддCды  "
540 LOCATE ,K:PRINT "                                R/L/C CIRCUITS      "
550 COLOR 7,0
560 RETURN

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'WINDOM - Windom Antenna - 08 JUN 98 rev. 28 NOV 2007
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,1
50 UL$=STRING$(80,205)
60 U$="####.##"
70 V$="###.###"
80 VL#=2.997924580000000E+08/10^6
90 '
100 COLOR 15,2
110 PRINT " WINDOM ANTENNA";TAB(57);;"by George Murphy VE3ERP ";
120 COLOR 1,0:PRINT STRING$(80,223);
130 COLOR 7,0
140 '
150 '.....start
160 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
170 GOSUB 510     'diagram
180 PRINT UL$;
190 '
200 LOCATE 23,24:COLOR 0,7
210 PRINT " Press 1 to continue or 0 to EXIT ";
220 COLOR 7,0
230 Z$=INKEY$:IF Z$=""THEN 230
240 IF Z$="0"THEN CLS:RUN EX$
250 IF Z$="1"THEN 280
260 GOTO 230
270 '
280 '.....data input
290 VIEW PRINT 14 TO 24:CLS:VIEW PRINT:LOCATE 15,T
300 INPUT "ENTER: Centre frequency (MHz) of band of interest....";F
310 VIEW PRINT 15 TO 24:CLS:VIEW PRINT:LOCATE 15,T
320 PRINT "Windom Antenna cut for";USING V$;F;
330 PRINT " MHz (";USING "###.###";300/F;:PRINT " metres )"
340 LGM=VL#/2/F:LGF=LGM/3.048000E-01
350 EM=LGM*3.600000E-01:EF=EM/3.048000E-01
360 PRINT
370 PRINT TAB(T);"Length LG  =";USING U$;LGM;:PRINT "m. (";USING U$;LGF;
380 PRINT " ft.)"
390 PRINT TAB(T);"Distance A =";USING U$;EM;:PRINT "m. (";USING U$;EF;
400 PRINT " ft.)"
410 PRINT
420 TB=7
430 PRINT TAB(TB);
440 PRINT "Because the feed line is brought to the operating position, RF in"
450 PRINT TAB(TB);
460 PRINT "the shack and a potential radiation hazard may be experienced with"
470 PRINT TAB(TB);
480 PRINT "this antenna (ARRL ANTENNA BOOK, 17th edition, page 7-20)."
490 GOTO 670
500 '
510 '.....diagram
520 COLOR 0,7:LOCATE 3:T=13
530 LOCATE ,T:PRINT "                                                        "
540 LOCATE ,T:PRINT "    ³®ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ LG ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¯³    "
550 LOCATE ,T:PRINT "    ³®ÄÄÄ A ÄÄÄ¯³                                  ³    "
560 LOCATE ,T:PRINT " - -ìÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄì- - "
570 LOCATE ,T:PRINT "                ³                                       "
580 LOCATE ,T:PRINT "                ³®ÄÄ single wire feedline               "
590 LOCATE ,T:PRINT "                ³                                       "
600 LOCATE ,T:PRINT "                ÀÄ feed                                 "
610 LOCATE ,T:PRINT "                ÚÄ point                                "
620 LOCATE ,T:PRINT "      ground \\\Á\\\                                    "
630 LOCATE ,T:PRINT "                                                        "
640 COLOR 7,0
650 RETURN
660 '
670 '.....end
680 GOSUB 700:GOTO 150
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

1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$
10 'LOGOCLOK - 30 SEP 90  rev. 18 APR 96
20 '     By George Murphy, 77 McKenzie St., Orillia, Ont. L3V 6A6
30 CLS:KEY OFF                   'clear screen
40 
50 IF EX$=""THEN EX$="exit"
60 N$="H A M C A L C"            'logo
70 SCREEN 9                      '640 x 350 pixel graphics monitor
80 '
90 DIM J$(7)                     'dimension day name variables
100 DIM M$(12)                    'dimension month name variables
110 DIM C(15)                     'dimension colour variables
120 '
130 '.....memorize variables
140 DATA Sunday,Monday,Tuesday, Wednesday,Thursday,Friday,Saturday
150  FOR Z=1 TO 7
160   READ J$(Z)
170  NEXT Z
180 DATA Jan.,Feb.,Mar.,Apr.,May.,Jun.,Jul.,Aug.,Sep.,Oct.,Nov.,Dec.
190  FOR Z=1 TO 12
200   READ M$(Z)
210  NEXT Z
220 DATA 10,11,12,13,14
230  FOR Z=2 TO 6                   'colours
240   READ C(Z)
250  NEXT Z
260 '
270 PI=3.141592
280 ASP=1.379999                        'aspect ratio correction
290 CX=315:CY=104                   'initial coordinates of dial centre
300 LIN=8                           'initial line for text
310 RD=147                          'length of dial radial
320 RN=98                           'length of 5 minute markers radial
330 RS=98                           'length of seconds radial
340 RM=90                           'length of minutes radial
350 RH=60                           'length of hours radial
360 B1=1                            'blue background color
370 B2=4                            'red background color
380 '
390 '.....start one minute cycle
400 SWAP B1,B2:BG=B1
410 FOR Z=2 TO 6
420  C(Z)=C(Z)+1                    'change colours
430  IF C(Z)=15 THEN C(Z)=10
440  IF C(Z)=7 THEN C(Z)=2
450 NEXT Z
460 '
470 '.....draw clock face
480 LIN=LIN+1                       'shift display one line down
490 IF LIN=18 THEN 270
500 CY=CY+14                        'shift Y coordinate one line down
510 '
520 X=1                             'set color base for dial outline
530 FOR W=1 TO 13 STEP 3            'draw dial outline
540 X=X+1                           'set new color for each ring
550 COLOR C(X),BG
560 CIRCLE(CX,CY),RD+W              'draw ring
570 NEXT W
580 '
590 L=PI/6                          '5 minute marker angle in radians
600 COLOR C(3),BG
610 FOR Z=1 TO 12                   'draw 5 minute markers
620  DX=SIN(L*Z)*RN*ASP
630  DY=COS(L*Z)*RN
640  CIRCLE(CX+DX,CY+DY),7
650  CIRCLE(CX+DX,CY+DY),8
660 NEXT Z
670 '
680 L=PI/30                         'seconds marker angle in radians
690 FOR Z=1 TO 60                   'draw seconds markers
700  DX=SIN(L*Z)*RS*ASP
710  DY=COS(L*Z)*RS
720  CIRCLE(CX+DX,CY+DY),1
730 NEXT Z
740 '
750 HRS=VAL(LEFT$(TIME$,2))         'hour
760 MIN=VAL(MID$(TIME$,4,2))        'minute
770 IF HRS>12 THEN HRS=HRS-12       'convert 24 hr. clock to 12 hr. clock
780 HS=HRS*30+MIN/2+180             'hours angle in degrees
790 HS=-HS*PI/180                   'hours angle in radians, clockwise rotation
800 HX=SIN(HS)*RH                   'X coordinate of hour hand
810 HX=ASP*HX                       'aspect ratio correction
820 HY=COS(HS)*RH                   'Y coordinate of hour hand
830 '
840 '.....start seconds counter
850 IF INKEY$<>""THEN CLS:SCREEN 0:CHAIN EX$
860 T=INT(TIMER)                    'current time to nearest second
870 IF T=INT(TIMER)THEN 870         'delay until next second
880 SEC=VAL(RIGHT$(TIME$,2))        'get seconds
890 '
900 '.....seconds
910 COLOR BG,BG
920 CIRCLE (CX+SX,CY+SY),4          'subdue seconds indicator
930 '
940 S=SEC*6+180                     'seconds angle in degrees
950 LS=-S*PI/180                    'angle in radians, clockwise rotation
960 SX=SIN(LS)*RS                   'X coordinate of second hand
970 SX=ASP*SX                       'aspect ratio correction
980 SY=COS(LS)*RS                   'Y coordinate of second hand
990 '
1000 'COLOR C(6),BG
1005 COLOR 15,BG
1010 CIRCLE (CX+SX,CY+SY),4          'print new seconds indicator
1020 '
1030 GOSUB 1360                      'get day of week and date J$ & D$
1040 JD$=J$+" "+D$
1050 COLOR C(4),BG
1060 LOCATE LIN-2,(80-LEN(JD$))/2    'centre day and date printout
1070 PRINT JD$                       'print day & date
1080 T$=TIME$
1090 LOCATE LIN+2,(80-LEN(T$))/2     'centre digital time printout
1100 PRINT T$                        'print digital time
1110 '
1120 COLOR C(5),BG                   'set logo colour
1130 GOSUB 1600                      'print text logo
1140 COLOR C(6),BG
1150 'CIRCLE (CX,CY),3                'draw dial centre
1160 'PAINT(CX,CY+1)                  'fill dial centre
1170 LINE (CX,CY)-(CX+HX,CY+HY)      'print hour hand
1180 '
1190 '.....minutes
1200 COLOR BG,BG
1210 LINE (CX,CY)-(CX+MX,CY+MY)      'blank current minute hand
1220 '
1230 M=MIN*6+SEC/10+180              'minutes angle in degrees
1240 IF SEC=0 THEN M=M+6
1250 MS=-M*PI/180                    'angle in radians, clockwise rotation
1260 MX=SIN(MS)*RM                   'X coordinate of minute hand
1270 MX=MX*ASP                       'aspect ratio correction
1280 MY=COS(MS)*RM                   'Y coordinate of minute hand
1290 '
1300 COLOR C(6),BG
1310 LINE (CX,CY)-(CX+MX,CY+MY)      'print new minute hand
1320 '
1330 IF SEC=0 THEN CLS:GOTO 390      'end of 1 minute cycle
1340 GOTO 840                        'end of 1 second cycle
1350 '
1360 '.....day of week subroutine
1370 MO=VAL(LEFT$(DATE$,2))          'month no.
1380 M$=M$(MO)                       'month name
1390 DA=VAL(MID$(DATE$,4,2))         'day no.
1400 D$=RIGHT$(STR$(DA),2)           'day name
1410 IF LEN(D$)=2 THEN 1430          'insert leading space to single digit day
1420 D$=" "+D$:GOTO 1410
1430 D$=M$+D$                        'month & day e.g. Nov.  9
1440 '
1450 '.....calculate day of week
1460 YR=VAL(RIGHT$(DATE$,4))         'year
1470 K=INT(6.000000E-01+1/MO)
1480 L=YR-K
1490 O=MO+12*K
1500 P=L/100
1510 Z1=INT(P/4)
1520 Z2=INT(P)
1530 Z3=INT(5*L/4)
1540 Z4=INT(13*(O+1)/5)
1550 Z=Z4+Z3-Z2+Z1+DA-1
1560 ZZ=Z-(7*INT(Z/7))+1
1570 J$=J$(ZZ)                       'name of day e.g. Friday
1580 RETURN
1590 '
1600 '.....text logo
1610 T=(80-LEN(N$))/2                'centre logo
1620 LOCATE LIN,T:PRINT N$           'N$ is logo as defined at start of program
1630 RETURN

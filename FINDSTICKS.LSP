;***********************************************************
;FINDSTICKS
;**********************************************************
;Program to draw wellbore traces as polylines
;Programmer: Ozkaya
;Date: July 2000
;**********************************************************
;Prepare FINDHORS.INP file. It should look like this 
;fnm_hor.prn
;find_horlcz.prn
;out_horlcz.prn
;100
;280000
;3040000
;320000
;3100000   
;3
;
;
;rows
;1  horizontal wells trajecteory data file name
;2  input file- horizontal well ids md at anomaly other specs 
;3  scale
;4  xorigin
;5  yorigin
;6  xrite
;7  yup
;9 bubble  size multiplier
;**********************************************************
;The horizontal trajectory data file has the following format:
;x		y		md		wellid
;298370.00	3069225.00	0.00	      31101
;298369.98	3069225.05	76.00	      31101
;298369.97	3069225.17	169.00	31101
;298369.98	3069225.31	265.00	31101
;298369.93	3069225.45	357.00	31101
;298369.85	3069225.53	438.00	31101
;.....
;300069.79	3070624.32	4251.97	33201
;300069.85	3070624.39	4252.47	33201
;300069.91	3070624.47	4252.97	33201
;300069.97	3070624.55	4253.47	33201
;300070.03	3070624.62	4253.97	33201
;300070.09	3070624.70	4254.47	33201
;300070.16	3070624.78	4254.97	33201

;.......
;endfile          
;
;The request file for finding coordinates for md has 
;the following format 
;  wellid       md           len     azim   color 
;  41701       14480.00       500     45       1
;  72100       14300.00       300     122      1
;  12300       17000.00       700      76      1
;....
; endfile
;
;***********************************************************	
;Each well trace consists of at least two  points
;***********************************************************

 (defun FINDSTICKS ( / &xg &numberlist &err  &mazgal
    &linecnt &mashfile &robfile &outfile &nline &xor &yor &scale   &artmalik &corfile &wellfile &horfile &fcfile &coloropt &bubsize)
 ;**********************************************************
 ;*********************************************************
  (Defun *error* (msg)
 ;*******************************************************
 ;***AutoCAD error routine...
 ;*********************************************************
    (terpri)
    (princ msg)
    (princ)
   );end error..
 ;*******************************************************
 (defun READIT ( *argfile / ilk n i mi ilkflag ilkebay)
 ;*******************************************************
 ;***Procedure to read a line from input text file..
 ;*******************************************************
 ;+++++++++++++++++++++++++++++++++++++++++++++++
  (defun elimbl (  /  ie ne ilke)
 ;+++++++++++++++++++++++++++++++++++++++++++++++
 ;***Subprocedure to eliminate blanks..
 ;+++++++++++++++++++++++++++++++++++++++++++++++
   (setq ilkebay -10)
   (if (/= &xg nil)
    (progn
     (setq
       ie 1
       ne (strlen &xg)
     );end setq..
     (while (and (<= ie ne) (< ilkebay 0))
      (setq
       ilke (substr &xg ie 1)
      );end setq
      (if (/= ilke " ")
       (setq
        &xg (substr &xg ie)
        ilkebay 10
       );end setq..
      );end if..
      (setq ie (1+ ie))
     );end while..
    );end progn..
   );end if..
   (if (< ilkebay 0)
    (setq
      &linecnt (1+ &linecnt)
    );end setq..
   );end if..
   (if (> &linecnt &nline)
    (progn
      (terpri)
      (princ "Unexpected end of file or no endfile mark..")
      (setq &err (/ 1 nil))
    );end progn
  );end if..
 );end elimbl..
 ;+++++++++++++++++++++++++++++++++
  (setq
   &xg (read-line *argfile)
  );end setq..
  (elimbl)
  (if (> ilkebay 0)
   (setq
    ilk (substr &xg 1 1)
   );end setq..
   ;else
    (setq ilk "#")
  );end if..
  (while  (= ilk "#")
   (setq
    &xg (read-line *argfile)
   );end setq..
   (elimbl)
   (if (> ilkebay 0)
    (setq
     ilk (substr &xg 1 1)
    );end setq..
   ;else
    (setq ilk "#")
   );end if..
  );end while..
    (setq
      n (strlen &xg)
      i n
      ilkflag -10
      mi n
    );end setq
    (while ( and (> i 0) (< ilkflag 0))
       (setq
         ilk (substr &xg i 1)
      );end setq..
      (if (/= ilk " ")
        (setq
          ilkflag 10
          mi i
        );end setq..
       );end if..
       (setq
         i (1- i)
      );end setq..
    ); end while..
    (setq &xg (substr &xg 1 mi))
    (setq &xg (strcase &xg T))
 );end readit..
 ;*******************************************************
 (defun PROCURE ( /  ip np alfa number harf)
 ;*******************************************************
 ;***Procedure to parse input text line and extract
 ;number fields. &numberlist is the list of numbers...
 ;*******************************************************
  (setq
    &numberlist nil
    ip 1
    np (strlen &xg)
    alfa " "
    number nil
    harf (substr &xg 1 1)
  );end setq..
  (while (<= ip np)
   (while ( and (<= ip np) (= harf " "))
     (setq
       ip (1+ ip)
       harf (substr &xg ip 1)
      );end setq..
   );end while..
   (if (<= ip np)
   (progn
    (while (and (<= ip np)
               (/= harf " ")
               (/= harf ",")
          );end and..
     (setq
     sayi (ascii harf)
     );end setq..
     (if ( and (/= sayi 45) (/= sayi 46))
      (if (or (< sayi 48) (> sayi 57))
       (progn
        (terpri)
        (princ "Warning - Non-numeric character...")
        (terpri)
       );end progn..
      );end if..
     );end if (/= sayi 45)
     (setq
      alfa (strcat alfa harf)
      ip (1+ ip)
      harf (substr &xg ip 1)
     );end setq..
    ); end while..
    (setq
     number (atof alfa)
     alfa " "
     &numberlist (cons number &numberlist)
     ip (1+ ip)
     harf (substr &xg ip 1)
    );end setq..
   );end progn..
   );end if..
  );end while (<= ip np..
  (if (/= &numberlist nil)
   (progn
   (setq &numberlist (reverse &numberlist))
   );end progn..
  );end if..
 );end procure..
 
 ;******************************************************
 (defun OKUHOR (  mdlcz  fnolcz  lenlcz azimlcz colorlcz /  x1 y1 fno md mdflag mdp fnop x1p y1p satir delx delmd dlcz divdel muldel xlcz dely ylcz
tip1x tip1y tip2x tip2y sinazim cosazim halflen azimrad tip1xt tip1yt  tip2xt tip2yt fnoflag)
 ;******************************************************
 ;Procedure to find coordinates from md
 ;******************************************************
 (setq
   &linecnt 0
   &mashfile (open  &horfile  "r")
 );end setq..
 (setq mdflag 0)
  (readit &mashfile)
  (setq fnoflag 0)
 
  (procure)
  
   (setq
    x1p (nth 0 &numberlist)
    y1p (nth 1 &numberlist)
   ; x1p (/ (- x1p &xor) &scale)
   ; y1p (/ (- y1p &yor) &scale)
    mdp (nth 2 &numberlist)
    fnop  (fix (nth 3 &numberlist))
  );end setq..
 
  (readit &mashfile)
   
 (while  (and (/= &xg "endfile") (< mdflag 1))
   
   (procure)
   (setq
    x1 (nth 0 &numberlist)
    y1 (nth 1 &numberlist)
    md (nth 2 &numberlist)
    fno  (fix (nth 3 &numberlist))
   );end setq..
 
   
;////////////////////


(if ( and (= fnop fnolcz) (= fno fnolcz))
 (progn
   

   (if (and ( > mdlcz mdp)(<= mdlcz md))
    (progn
       
       (setq 
         delx (- x1 x1p)
         delmd (- mdp md)
         dlcz  (- mdlcz mdp)
         divdel (/ delx delmd)
         muldel (* divdel dlcz)
         xlcz (+ muldel  x1p)
 
         dely (- y1 y1p)
         delmd (- mdp md)
         dlcz  (- mdlcz mdp)
         divdel (/ dely delmd)
         muldel (* divdel dlcz)
         ylcz (+ muldel  y1p)
   
       );end setq..
       
  ; write results


;tip1x tip1y tip2x tip2y sinazim cosazim halflen azimrad 
;tip1xt tip1yt  tip2xt tip2yt

   (setq halflen (* lenlcz 0.5))
    (setq halflen (* halflen &bubsize))

    (setq azimrad (* azimlcz 3.14159))
    (setq azimrad (/ azimrad 180))

    (setq sinazim (sin azimrad))
    (setq cosazim (cos azimrad))
    (setq fnoflag 1)
; 
  
  (setq tip1x (+ xlcz (* halflen sinazim)))
  (setq tip1y (+ ylcz (* halflen cosazim)))
  (setq tip2x (- xlcz (* halflen sinazim)))
  (setq tip2y (- ylcz (* halflen cosazim))) 
   
(setq
  tip1xt (rtos tip1x 2 2)
  tip1yt (rtos tip1y 2 2)
  tip2xt (rtos tip2x 2 2)
  tip2yt (rtos tip2y 2 2)
  colortxt (rtos colorlcz 2 0)
 );end setq. 



  (setq
    satir (strcat  tip1xt "   " tip1yt "   " tip2xt "    "    tip2yt "     " colortxt)
    satir (strcat satir "   ")
  );end setq..  
          
(write-line satir wfile)


; end write results

    );end progn mdlcz
   );end if mdlcz
 );end progn  fno..
); end if fno..

 



(setq 
  mdp md
  fnop fno
  x1p x1
  y1p y1
);end setq..
   
;//////////////////////


    (readit &mashfile) 
   
  );end while..

(if (< fnoflag 1)
   (print (list "no such well or md.." fnolcz))  
 ); end if..


  (close &mashfile)
  );end okuhor..
;*********************************************************
;*********************************************************
;*********************************************************
;*********************************************************
(defun OKUVER (fnolcz lenlcz azimlcz colorlcz /  x1 y1 fno  fnoflag satir tip1x tip1y tip2x tip2y sinazim cosazim halflen azimrad tip1xt tip1yt  tip2xt tip2yt colortxt len azim color)

 ;******************************************************
 ;Procedure to find coordinates from
 ;******************************************************
 (setq
   &linecnt 0
   &mashfile (open  &wellfile  "r")
 );end setq..
 (setq fnoflag 0)
  (readit &mashfile)
  
(while  (and (/= &xg "endfile") (< fnoflag 1))

  (procure)
  
   (setq
    x1 (nth 0 &numberlist)
    y1 (nth 1 &numberlist)
    fno  (fix (nth 2 &numberlist))
    len (nth 3 &numberlist)
    azim (nth 4 &numberlist)
    color (fix (nth 5 &numberlist))
   );end setq..

  (if (= fno fnolcz)
     (progn
       
       (setq fnoflag 1)
   
; write results
;//////////////////////////

;tip1x tip1y tip2x tip2y sinazim cosazim halflen azimrad 
;tip1xt tip1yt  tip2xt tip2yt
    
    (setq halflen (* lenlcz 0.5))
    (setq halflen (* halflen &bubsize))
    (setq azimrad (* azimlcz 3.14159))
    (setq azimrad (/ azimrad 180))

    (setq sinazim (sin azimrad))
    (setq cosazim (cos azimrad))

; 
  
   (setq tip1x (+ x1 (* halflen sinazim)))
   (setq tip1y (+ y1 (* halflen cosazim)))   
   (setq tip2x (- x1 (* halflen sinazim)))
   (setq tip2y (- y1 (* halflen cosazim)))   

   

(setq
  tip1xt (rtos tip1x 2 2)
  tip1yt (rtos tip1y 2 2)
  tip2xt (rtos tip2x 2 2)
  tip2yt (rtos tip2y 2 2)
  colortxt (rtos colorlcz 2 0)
 );end setq. 

  (setq
    satir (strcat  tip1xt "   " tip1yt "   " tip2xt "    "      tip2yt "     " colortxt)

    satir (strcat satir "   ")
  );end setq..  
          
(write-line satir wfile)


; end write results





;//////////////////////////
  

  ); end progn
); end if






  (readit &mashfile)

); end while

 (if (< fnoflag 1)
   (print (list "no such well.." fnolcz))  
 ); end if..

  (close &mashfile)
 );end okuver..



;*********************************************************
;*********************************************************
 (defun OKULCZ (  / mdlcz fnolcz lenlcz azimlcz colorlcz)
 ;******************************************************
 ;Procedure to read MD well id and specs from lczfile
 ;******************************************************
 (setq
   &linecnt 0
   &robfile (open  &fcfile  "r")
 );end setq..
  (readit &robfile)
   
  (while  (/= &xg "endfile")
   (procure)
   (setq
    fnolcz  (fix (nth 0 &numberlist))
    mdlcz (nth 1 &numberlist)   
    lenlcz (nth 2 &numberlist)
    azimlcz  (nth 3 &numberlist)
    colorlcz (fix (nth 4 &numberlist))
  );end setq..

  

     (if (> mdlcz 1)
      (Okuhor mdlcz fnolcz lenlcz azimlcz colorlcz)
     ;else
      (okuver fnolcz lenlcz azimlcz colorlcz)
    );end if..  
    


  (readit &robfile)
 
 

 );end while..
 (close &robfile)
);end okulcz..


;**********************************************************
(defun OKUINP ( / )
    
  (setq &corfile "findsticks.inp")

   
  (setq &mashfile (open &corfile "r"))

  (readit &mashfile)
  (setq &wellfile &xg)   
  
  (readit &mashfile)
  (setq &horfile &xg)
   
  (readit &mashfile)
  (setq &fcfile &xg)

  (readit &mashfile)
  (setq &outfile &xg) 
  
 
  (readit &mashfile)
  (procure)
  (setq &scale (nth 0 &numberlist))
  

  (readit &mashfile)
  (procure)
  (setq &xor (nth 0 &numberlist))
  
  (readit &mashfile)
  (procure)
  (setq &yor (nth 0 &numberlist)) 
  (readit &mashfile)
  
   
  (procure)
  (setq &xrt (nth 0 &numberlist))
  
  
  (readit &mashfile)
  (procure)
  (setq &yup (nth 0 &numberlist))
  
 
  
  
  (readit &mashfile)
  (procure)
  (setq &bubsize (nth 0 &numberlist))
   
  (close &mashfile)
 ); end okuinp
;***********************************************************
  (setvar "cmdecho" 0)
  (setq &nline 1000)
  (terpri)

  (okuinp)


 (setq cfile (findfile  &wellfile))

   (if (= cfile nil)
    (progn
     (terpri)
     (print "No file..")
     (princ)
     (setq &err (/ 1 0))
   );end progn
  );end if


  (setq cfile (findfile  &horfile))

   (if (= cfile nil)
    (progn
     (terpri)
     (print "No file..")
     (princ)
     (setq &err (/ 1 0))
   );end progn
  );end if

  (setq cfile (findfile  &fcfile))

   (if (= cfile nil)
    (progn
     (terpri)
     (print "No file..")
     (princ)
     (setq &err (/ 1 0))
   );end progn
  );end if


 (setq cfile (findfile  &outfile))
 (setq ans "Yes")
 (if (/= cfile nil)
  (progn
   (terpri)
   (initget 1 "Yes No")
   (setq ans (getkword "File already exists. Replace? <Y/N>"))
  );end progn
 );end if
   
 (if (= ans "Yes")       
  (progn
   (setq
     wfile (open  &outfile "w")
   );end setq

   
   
   (okulcz)

   (setq satir "endfile")
   (write-line satir wfile)
    
   (close wfile)
  );end progn
 ); end if
   
 );end FINDSTICKS
 
 
 
 
 
 
 
 
 
 
 
 
 
 

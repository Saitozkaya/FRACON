;***********************************************************
;FINDHORPTS
;**********************************************************
;Program to draw wellbore traces as polylines
;Programmer: Ozkaya
;Date: July 2000
;**********************************************************
;Prepare FINDHORPTS.INP file. It should look like this 
;fnhortraj.prn
;mdludloss.prn
;cormudloss.prn
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
;  wellid       md           size     size   color 
;  41701       14480.00       5       3       1
;  72100       14300.00       5       3       1
;  12300       17000.00       5       3       1
;....
; endfile
;
;***********************************************************	
;Each well trace consists of at least two  points
;***********************************************************

 (defun FINDHORPTS ( / &xg &numberlist &err  &mazgal
    &linecnt &mashfile &robfile &outfile &nline &xor &yor &scale   &artmalik &corfile &horfile &lczfile &coloropt &bubsize)
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
 (defun OKUHOR (  mdlcz  fnolcz  sidelcz sizelcz colorlcz /  x1 y1 fno md mdflag mdp fnop x1p y1p xtxt ytxt sidetxt sizetxt colortxt satir delx delmd dlcz divdel muldel xlcz dey ylcz
)
 ;******************************************************
 ;Procedure to find coordinates from md
 ;******************************************************
 (setq
   &linecnt 0
   &mashfile (open  &horfile  "r")
 );end setq..
 (setq mdflag 0)
  (readit &mashfile)
  
 
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
  ;  x1 (/ (- x1 &xor) &scale)
  ;  y1 (/ (- y1 &yor) &scale)
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
   
         mdflag 1
       );end setq..
       
  ; write results

   (setq
    welltext (rtos fnolcz 2 0)
    xtxt (rtos xlcz 2 2)
    ytxt (rtos ylcz 2 2)
    sidetxt( rtos sidelcz 2 0)     
    sizetxt (rtos sizelcz 2 0)
    colortxt (rtos colorlcz 2 0)
    ); end setq


  (setq
    satir (strcat  xtxt "   " ytxt "   " welltext "    "    sidetxt "   "   sizetxt  "     " colortxt)
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

  (close &mashfile)
  );end okuhor..
;*********************************************************
;*********************************************************
;*********************************************************
;*********************************************************
 (defun OKULCZ (  / mdlcz fnolcz sidelcz sizelcz colorlcz )
 ;******************************************************
 ;Procedure to read MD well id and specs from lczfile
 ;******************************************************
 (setq
   &linecnt 0
   &robfile (open  &lczfile  "r")
 );end setq..
  (readit &robfile)
  (while  (/= &xg "endfile")
   (procure)
   (setq
    fnolcz  (fix (nth 0 &numberlist))
    mdlcz (nth 1 &numberlist)   
    sidelcz (fix (nth 2 &numberlist))
    sizelcz (fix (nth 3 &numberlist))
    colorlcz (fix (nth 4 &numberlist))
  );end setq..
  

      (Okuhor mdlcz fnolcz sidelcz sizelcz colorlcz)
     
    


  (readit &robfile)
 
 

 );end while..
 (close &robfile)
);end okulcz..


;**********************************************************
(defun OKUINP ( / )
    
  (setq &corfile "findhorpts.inp")

   
  (setq &mashfile (open &corfile "r"))

     
  (readit &mashfile)
  (setq &horfile &xg)
   
  (readit &mashfile)
  (setq &lczfile &xg)

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

 

  (setq cfile (findfile  &horfile))

   (if (= cfile nil)
    (progn
     (terpri)
     (print "No file..")
     (princ)
     (setq &err (/ 1 0))
   );end progn
  );end if

  (setq cfile (findfile  &lczfile))

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
   
  
 );end FINDHORPTS
 
 
 
 
 
 
 
 
 
 
 
 
 
 

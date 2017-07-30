;**********************************************************
;FINDVERPTS
;**********************************************************
;Program to draw wellbore traces as polylines
;Programmer: Ozkaya
;Date: July 2000
;**********************************************************
;Prepare FINDVERPTS.INP file. It should look like this 
;fnverloc.prn
;mdverpta
;corverpta.prn
;100
378000
2518000
384000
2526000 
;3
;
;rows
;1  Reference vertical well coordinates file name
;2  Request file name to find coordinates given well name and md
;3  Output file name with coordinates well name
;4  scale
;5  xorigin
;6  yorigin
;7  xrite
;8  yup
;10 buble size multiplier
;
;**********************************************************
;The reference vertical coordinate file has the following format:
;x		y		md		wellid
;298370.00	3069225.00	0.00	      31101
;298369.98	3069225.05	0.00       31101
;298369.97	3069225.17	0.00  	31101
;.....
;300069.79	3070624.32	 0.00		33201
;300069.85	3070624.39	 0.00  	33201
;300069.91	3070624.47	 0.00		33201
;300069.97	3070624.55	 0.00		33201
;.......
;endfile            
;
;The request file has the following format
;    well id   md      side  size   color
;   12400       0       0    5       3
;   29100       0       0    2       3
;   29500       0       0    3       3
;   32900       0       0    1       3
;......
; endfile
;side is polygon side 0 means circle
;
;***********************************************************	
;***********************************************************

 (defun FINDVERPTS( / &xg &numberlist &err  &mazgal
    &linecnt &mashfile &robfile &outfile &nline &xor &yor &scale   &artmalik  &wellfile &corfile &lczfile &coloropt  &bubsize)
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
 
;*********************************************************
;*********************************************************
(defun OKUVER (fnolcz sidelcz sizelcz colorlcz /  x1 y1 fno  fnoflag side size color xtxt ytxt welltxt sidetxt sizetxt colortxt satir)

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
    side (nth 3 &numberlist)
    size (nth 4 &numberlist)
    color (nth 5 &numberlist)
   );end setq..

  (if (= fno fnolcz)
     (progn
       
       (setq fnoflag 1)
   
; write results
   
  

   (setq welltxt (rtos fnolcz 2 0))
  
   (setq xtxt (rtos x1 2 2))
  

   (setq ytxt (rtos y1 2 2)) 

  

   (setq 
    sidetxt  (rtos sidelcz 2 0)    
    sizelcz (* sizelcz &bubsize) 
    sizetxt (rtos sizelcz 2 2)
    colortxt (rtos colorlcz 2 0)
    ); end setq

     

   
  (setq
    satir (strcat  xtxt "   " ytxt "   " welltxt "    "  sidetxt "   "   sizetxt  "     " colortxt)
    satir (strcat satir "   ")
  );end setq..  
  
     
(write-line satir wfile)

  ); end progn
); end if
; end write results


  (readit &mashfile)

); end while
  (close &mashfile)
 );end okuver..

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
    sizelcz (nth 3 &numberlist)
    colorlcz (fix (nth 4 &numberlist))
  );end setq..
    
    
   (okuver fnolcz sidelcz sizelcz colorlcz)



  (readit &robfile)
 
 

 );end while..
 (close &robfile)
);end okulcz..


;**********************************************************
(defun OKUINP ( / )
    
  (setq &corfile "findverpts.inp")

   
  (setq &mashfile (open &corfile "r"))

  (readit &mashfile)
  (setq &wellfile &xg)
    
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

 (setq cfile (findfile  &wellfile))

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
   
 );end FINDVERPTS
 
 
 
 
 
 
 
 
 
 
 
 
 
 

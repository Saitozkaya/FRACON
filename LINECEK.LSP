;*************************************************************
; LINECEK.LSP
; programmer ozkaya/Bahrain Geoscience Center
; Platform ACAD14+
;*************************************************************
; Extract lines and writes them to file
; This program can be used to output fracture corridors
; as sticks

; file format
; x11 y11 x12 y12 azimuth1 length1
; x21 y21 x22 y22 azimuth2 length2
; .....
;end file
;*************************************************************
;The program expects an input file must be prepared which looks like this
;yeniceri.plc
;1
;0
;0
;
;
;rows
;1  data file name
;2  borehole label
;3  scale
;4  xorigin
;5  yorigin
;
;
;*************************************************************
(defun LINECEK (  /   all nset i outfile cfile ans wfile xy satir 
 subent vertex verseq elem part tart zart gagalin p1 p2 verseq lnum
 sqnum   &scale &xor &yor &outfile &infile &mashfile)
;**************************************************************

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
 (defun READIT ( / ilk n i mi ilkflag ilkebay)
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
   &xg (read-line &mashfile)
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
    &xg (read-line &mashfile)
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

;**********************************************************
(defun OKUINP ( / )
    
  (setq &infile "linecek.inp")

   
  (setq &mashfile (open &infile "r"))
   
  (readit)
  (setq &outfile &xg)
  
  (readit)
  (procure)
  (setq &scale (nth 0 &numberlist))
   
  (readit)
  (procure)
  (setq &xor (nth 0 &numberlist))
  
  (readit)
  (procure)
  (setq &yor (nth 0 &numberlist)) 
 
  (close &mashfile)
 ); end okuinp


;*********************************************************
(defun yazpts ( py1 py2 / x1 y1 x2 y2 x1txt y1txt x2txt y2txt
      xtxt ytxt satir aci deg boy degtxt boytxt )

(setq boy (distance py1 py2))
(if (> boy 0.1) 
 (progn 
  (setq
    aci (angle py1 py2)
    deg (atof (angtos aci 0 2))

  );end setq..


   (setq deg (- 90 deg))


  (if (<  deg 0)
     (setq deg (+ 360 deg))
   );endif
   (if (> deg 180)
     (setq deg (- deg 180))
   );endif..


   (setq
    
    boy (distance py1 py2)
    boy (* boy &scale)
  );end setq




  (setq
    x1 (car py1)
    y1 (cadr py1)
    x2 (car py2)
    y2 (cadr py2)
    x1 (+ (* x1 &scale) &xor)
    y1 (+ (* y1 &scale) &yor)
    x2 (+ (* x2 &scale) &xor)
    y2 (+ (* y2 &scale) &yor)
  );end setq


   (setq
    degtxt (rtos deg 2 2)
    x1txt (rtos x1 2 2)
    y1txt (rtos y1 2 2)
    x2txt  (rtos x2 2 2)
    y2txt  (rtos y2 2 2)
    boytxt (rtos boy 2 2)
   ); end setq



  (setq
    satir (strcat x1txt "   " y1txt "   " x2txt "   " y2txt)
    satir (strcat satir "   ")

    satir (strcat satir "   " degtxt "   " boytxt)
;    satir (strcat satir "   " lnumtx )
   );end setq..



  (write-line satir wfile)
  );end progn
 ); endif
 );end yazpts..

;************************************************

(defun traceline ( / TargEnt TargLayer )
(setq TargEnt (car (entsel "\nSelect object on layer: ")))
(setq TargLayer (assoc 8 (entget TargEnt)))
  
(setq
     all (ssget "X"(list TargLayer))
     nset (sslength all)
   );end setq..
   
   (setq
     i 0
     lnum 0
   );end setq..
   (while  (< i nset)
    (setq
     elem (ssname all i)
     part (entget elem)
     tart (assoc 0 part)
     zart (cdr tart);entity name
    );end setq..
    (if (= zart "LINE")
     (progn
       (setq
         p1 (cdr (assoc 10 part))
         p2 (cdr (assoc 11 part))
 
       );end setq..

       (yazpts p1 p2)
 
     );end progn..
    );end if..
    (setq i (1+ i)) 	
   );end while..

 ); end traceline..
;*****************************************
;MAIN
;*****************************************

 (setvar "cmdecho" 0)
 (okuinp)
 
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

   (traceline)

   (setq satir "endfile")
   (write-line satir wfile)
    
   (close wfile)
  );end progn
 );end if..
) ;end linecek..    

    

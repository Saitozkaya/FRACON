;*************************************************************
; SPLINECEK.LSP
; programmer ozkaya/Bahrain Geoscience Center
; Platform ACAD14+
;*************************************************************
; Extract splines and writes them to file
; This program can be used to output manually drawn splines 
; file format
; the sequence number indicates which polyline the point belongs to..
; x1 y1  1
; x2 y2  1
; x3 y3  1
;.....
; x1 y1  2
; x2 y2  2
; x3 y3  2
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
(defun splinecek (  /   all nset i outfile cfile ans wfile xy satir 
 subent vertex verseq elem part tart zart gagalin p1 p2 verseq
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
    
  (setq &infile "splinecek.inp")

   
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

 (defun yazpts ( pyaz secnum / x y xtxt ytxt satir)
  (setq
    x (car pyaz)
    y (cadr pyaz)
    x (+ (* x &scale) &xor)
    y (+ (* y &scale ) &yor)
    xtxt (rtos x 2 3)
    ytxt (rtos y 2 3)
    satir xtxt
    satir (strcat satir " ")
    satir (strcat satir ytxt)
    satir  (strcat satir "  ")
    satir (strcat satir (rtos secnum 2 0))
   );end setq.. 
  
   
   (write-line satir wfile)
 );end yazpts..

;************************************************

(defun tracepoly ( / TargEnt TargLayer)


(setq TargEnt (car (entsel "\nSelect object on layer: ")))
(setq TargLayer (assoc 8 (entget TargEnt)))


   (setq 
     all (ssget "X" (list TargLayer))
     nset (sslength all)
   );end setq..
   (setq 
     i 0
     secnum 1
   );end setq..

;////////////////

   (while  (< i nset)

    (setq
     elem (ssname all i)
     part (entget elem)
     tart (assoc 0 part)
     zart (cdr tart)
    );end setq..
    (if (= zart "SPLINE")
     (progn
      (setq
        nsay 0
        ican 0
        ncanlar (length part)
      );end setq..
      (while (< ican ncanlar)
       (setq
         subent (nth ican part)
       );end setq..
       (if (= (car subent) 10)
         (progn
           (setq vertex (cdr subent))
           (setq p1 vertex)
           (setq verflag 1)
         (yazpts p1 secnum)
        ); end progn..
       );end if
       (setq ican (1+ ican))

      );end while..
     );end progn..
    );end if..
    (setq i (1+ i))
    (setq secnum (1+ secnum))

   );end while..

;////////////////

 ); end tracepoly..
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

   (tracepoly)

   (setq satir "endfile")
   (write-line satir wfile)
    
   (close wfile)
  );end progn
 );end if..
) ;end polycek14..    

    

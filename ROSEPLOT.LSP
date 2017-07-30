;************************************************************* 
;ROSEPLOT.LSP
;************************************************************* 
; programmer ozkaya/Bahrain Geoscience Center
; Platform ACAD14+
;*************************************************************
; Pgm to plot rose diagram at specified well location
;The program expects an input file must be prepared which looks 
;like this
;100
;m
;378000
;2518000
;384000
;2526000
;1
;2
;3
;10
;1
;2
;
;1 strike file name
;2 well file name
;3 well number
;4 scale
;5 unit
;6 xor
;7 yor
;8 xrt
;9 yrt
;10 xcol
;11 ycol
;12 zcol -well id col
;13 rose size
;14 rose color
;15 1 strike or 2 dip azimuth plot
;***********************************************************
; data file for STRIKE is a single column set of fracture 
;strikes (between 0 and 180 degrees)
;34
;56
;74
;...
;data for DIP AZIMUTH plot is also a single column. Values 
;range between 0 and 360 deg.
;24
;245
;311
;...
;Well locations file has the following format
;    xutm             yutmn     well no   size     side    color
;  382604.00      2523632.00      59       0       1       1
;  381740.00      2524470.00      63       0       1       1
;  382459.00      2524365.00      79       0       1       1
;.....
;This is the same as the well location file for  findwell and 
;dotplot routines
;
;
;***********************************************************
(defun ROSEPLOT ( / &numberlist &rosecol &roserad &scale &kuyukodu &rfile &xg &dosya &infile &linecnt &err &stlist &wellfile &birim &xor &yor &xrt &yup &xcol &ycol &column &sdflag)


;***********************************************************
 
 ;************************************************************
 (defun CLS (/ pl pu)
 ;***********************************************************
 ;***procedure to clear the AutoCAD drawing area
 ;***********************************************************
 ;pl,pu   :lower left and upper right corners of drawing area
 ;***********************************************************
  (setvar "cmdecho" 0)
  (command "vpoint" "0,0,0")
  (command "erase" "all" "")
  (princ)
 );end cls...

 (Defun PORTRAIT ( /  )
 ;***********************************************************
 ;***procedure to set the screen limits to A4 size portrait
 ;orientation...
 ;***********************************************************
 (setvar "cmdecho" 0)
 (COMMAND "LIMITS" "-2,-2"  "17.8,26.5")
 (COMMAND "GRID" 1)
 (COMMAND "SNAP" 0.2)
 (command "snap" "OFF")
 (command "zoom" "a")
 (princ)
 (princ)
 );END portrait
 ;***********************************************************
 (Defun LANDSCAPE ( / )
 ;***********************************************************
 ;***procedure to set the screen limits to A4 size landscape
 ;orientation.
 ;***********************************************************
 (setvar "cmdecho" 0)
 (COMMAND "LIMITS" "-2.0,-2.0"  "26.5,17.8")
 (COMMAND "GRID" 1)
 (COMMAND "SNAP" 0.2)
 (command "snap" "OFF")
 (command "zoom" "a")
 (princ)
 (princ)
 );END landscape
 ;************************************************************
 (defun FONT ()
 ;************************************************************
 ;***procedure to set font..
 ;***********************************************************
  (setvar "cmdecho" 0)
  (command "style" "standard" "romant" "0.0" "1.0" "0.0" "N" "N" "N")
  );end font
 ;***********************************************************
 
 ;+++++++++++++++++++++++++++++++++++++++++++++++
 (defun readit ( / ilk n i mi ilkflag ilkebay)
 ;+++++++++++++++++++++++++++++++++++++++++++++++
 ;local variables
 ;ilk                :
 ;n                  :
 ;i                  :
 ;mi                 :
 ;ilkflag            :
 ;+++++++++++++++++++++++++++++++++++++++++++++++
  (defun elimbl (  /  ie ne ilke)
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
   &xg (read-line &rfile)
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
    &xg (read-line &rfile)
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
 ;***********************************************************
 (defun PROCURE ( /  ip np alfa number harf lnum)
 ;***********************************************************
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

 ;////////////////////////////////////////////


 ;***********************************************************
 (defun OKUSTRIKE ( / strike ntotal total i bine binb bincnt strval fracbin size binbr biner x0 y0 x1 y1 x2 y2 x3 y3 x4 y4 pia fracmax sdlim)
 ;***********************************************************

 ;**********************************************************

  (setq  &rfile (open  &dosya  "r"))
  (setq fracmax -10)
  (setq ntotal 0)
  (setq total 0)   
  (setq pia 3.14159)
  (command "color" (fix &rosecol))
  (setq x0 &xutm)
  (setq y0 &yutm)
   (setq p1 (list x0 y0))
   (command "pline" p1 )
  (readit)
  (setq sdlim 179)
  (if (> &sdflag 1)
      (setq sdlim 359)
  );endif    

  (while (and &xg (/= (strcase &xg T) "endfile"))
   (procure)
   (if (/= &numberlist nil)
    (progn
     (setq &strike (nth 0 &numberlist))
     (setq &stlist (cons &strike &stlist))
     (setq ntotal (1+ ntotal))
     (setq total (1+ total))
    );end progn..
   );end if..
   (readit)
  );end while..
  
  (setq binb 0)
  (setq bine 10)
  

   
  (while (< binb sdlim)
   (setq bincnt 0)
   (setq i 0)
   (while (<= i ntotal)
    (setq strval (nth i &stlist))   
    
     (if (and 
             (>= strval binb)
             (< strval bine)
	  );end and   
      (setq bincnt (1+ bincnt))
     );endif 
    (setq i (1+ i))
   );wend
  
; operations!!!!!!!!!!!!!!!!!!!!
 
 (setq bincnt (float bincnt))
 (setq total (float total))

 (setq fracbin (float (/ bincnt total)))

  
  
  (setq size (float (* fracbin &roserad))) 

   

   (if (> fracbin 0)
    (progn
     
      (setq 
         binb (float binb)
         bine (float bine)
      );end setq 
     


     (setq binbr (float (* binb (/ pia 180))))
     (setq biner (float (* bine (/ pia 180))))
     
      
     (setq
        x1 (+ &xutm (* size (sin binbr)))
        y1 (+ &yutm (* size (cos binbr)))
        x2 (+ &xutm (* size (sin biner)))
        y2 (+ &yutm (* size (cos biner)))
        x3 (- &xutm (* size (sin binbr)))
        y3 (- &yutm (* size (cos binbr)))
        x4 (- &xutm (* size (sin biner)))
        y4 (- &yutm (* size (cos biner)))
     );end setq..           
    

      (if (> fracbin fracmax)
      (setq fracmax fracbin)
     );end if..
   

; plot line from x0 y0 to x1 y1
;plot line from x1 y1 to x2 y2
; plot line from x2 y2 to x0 y0
;  plot line from x0 yo to x3 y3
;plot line from x3 y3 to x4 y4
;  plot line from x4 y4 to x0 y0
  

      ;(setq p1 (list x0 y0))
      (setq p2 (list x1 y1))
     
       (command p2)
       (setq p2 (list x2 y2))
       (command  p2)
       (command p1)
      (if (= &sdflag 1)
        (progn
          (setq p2 (list x3 y3))
          (command  p2)
          (setq p2 (list x4 y4))
          (command p2)
          (command p1) 
       );end progn
     );endif


    );end progn
   );end if..
    
     


; end operations !!!!!!!!!!!!!!!!
   (setq binb bine)
   (setq bine (+ bine 10))
  );wend




;///////////////////////////////
  
  (command "")
  
  (command "color" 7)
    (close &rfile )
 );end okustrike...

 ;*************************************************************
 (defun OKUBOTH ( /  findbay wellno xcor ycor par numpts)
 ;***********************************************************
 
 (setq &rfile (open &wellfile "r"))

  (setq &kuyukodu (fix &kuyukodu))

 (setq findbay -1)
  
  (readit)
  (while (and &xg (/= (strcase &xg T) "endfile") (< findbay 0))

     (procure)
     (setq &wellid (nth &column &numberlist))  
     (setq &wellid (fix &wellid))   
     (setq  xcor (nth &xcol &numberlist))
     (setq ycor (nth &ycol &numberlist))

     
     
     
     
  (if (= &kuyukodu &wellid) 
   (progn
     
   (setq
      xcor (- xcor &xor)
      xcor (/ xcor &scale)
      ycor (- ycor &yor)
      ycor (/ ycor &scale)
      &xutm xcor
      &yutm ycor
      par (list xcor ycor)
      findbay 1
    );end setq..
   ; (command "insert" "arrow" par "" "" "")
   );end progn..
  );end if..
   (readit)
  );end while..
  (if (< findbay 0)
   (progn
    (print "well not in database")
     (setq &xutm 0)
     (setq &yutmn 0)
    (princ)
   );end progn..
  );end if..
  (close &rfile)
 
 );end okuboth..



;**********************************************************

(defun OKUINP ( / denek )
    
  (setq &infile "roseplot.inp")
  (setq denek (findfile &infile))
  (if (= denek nil)
   (progn
    (terpri)
    (princ "No file..")
    (princ)
    (princ)
   );end progn..
  ;else
   (progn
   ; (cls)
    (setq &linecnt 0)
    (setq &rfile (open &infile "r"))

    (readit)
    (setq &dosya &xg)

    (readit)
    (setq &wellfile &xg)

    


    (readit)
     (procure)

     (setq &kuyukodu (nth 0 &numberlist))
   

    (readit)
    (procure)
    (setq &scale (nth 0 &numberlist))
     

    (readit)
    (setq &birim &xg)
     
    (readit)
    (procure)
    (setq &xor (nth 0 &numberlist))
    
    (readit)
    (procure)
    (setq &yor (nth 0 &numberlist))
    (readit)
    (procure)
    (setq &xrt (nth 0 &numberlist)) 
    (readit)
    (procure)
    (setq &yup (nth 0 &numberlist))
     
     
    (readit)
    (procure)
    (setq &xcol (fix (nth 0 &numberlist)))
    (readit)
    (procure)
    (setq &ycol (fix (nth 0 &numberlist)))
    (readit)
    (procure)
    (setq &column (fix (nth 0 &numberlist)))
    
    (setq &colflag 10)
    (if (< &column 1)
     (setq &colflag -10)
    );end if..
   
    (setq
      &column (1- &column)
      &xcol (1- &xcol)
      &ycol (1- &ycol)
      &column (fix &column)
      &xcol (fix &xcol)
      &ycol (fix &ycol)
    );end setq..






     (readit)
     (procure)
     (setq &roserad (nth  0 &numberlist)) 
    
     (readit)
     (procure)
     (setq &rosecol (nth 0 &numberlist))
     (readit)
     (procure)
     (setq &sdflag (fix (nth 0 &numberlist)))


    
    (close &rfile)
   );end progn..
  );end if..

 ); end okuinp


 ;*********************************************************
 ;PLOT BASE MAIN..
 ;*********************************************************
   
   (setvar "cmdecho" 0)
   (setq &nline 1)
   ;(cls)
   (landscape)
   (font)
   
   (okuinp)
   (okuboth)
   (okustrike)
   
   (command "zoom" "a")
   (princ)
   (princ)
 );end ROSEPLOT
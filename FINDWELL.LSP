 (defun C:fw ( / &rfile &wellfile &nline &bukuyu &numberlist &scale
 &wellid &infile &linecnt &column &colflag &xcol &ycol
 &xor &yor &xrt &yup)
 
 
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
    &misval -1
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
        (setq &misval 1)
;       (princ "Warning - Non-numeric character...")
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
 ;*************************************************************
 (defun OKUBOTH ( /  findbay wellno xcor ycor par numpts)
 ;***********************************************************
  (setq findbay -1)
  
  (readit)
  (while (and &xg (/= (strcase &xg T) "endfile") (< findbay 0))

     (procure)
     (setq &wellid (nth &column &numberlist))  
      (setq &wellid (fix &wellid))
     
     (setq  xcor (nth &xcol &numberlist))
     (setq ycor (nth &ycol &numberlist))

     
     
     
    
     
  (if (= &bukuyu &wellid) 
   (progn
     
   (setq
      xcor (- xcor &xor)
      xcor (/ xcor &scale)
      ycor (- ycor &yor)
      ycor (/ ycor &scale)
      par (list xcor ycor)
      findbay 1
    );end setq..
     (command "insert" "arrow" par "" "" "")
   );end progn..
  );end if..
   (readit)
  );end while..
  (if (< findbay 0)
   (progn
    (print "well not in database")
    (princ)
   );end progn..
  );end if..
  (close &rfile)
 );end okuboth..
;************************************************
(defun OKUINP ( / denek )
    
  (setq &infile "findwell.inp")
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
    (setq &linecnt 0)
    (setq &rfile (open &infile "r"))
    (readit)
    (setq &wellfile &xg)
    (print &wellfile)
    (readit)
    (procure)
    (setq &scale (nth 0 &numberlist))
    (print &scale)      
     (readit)
    (setq &birim &xg)
    (print "birim is ")
    (print &birim)

    (readit)
    (procure)
    (setq &xor (nth 0 &numberlist))
    (print &xor)
    (readit)
    (procure)
    (setq &yor (nth 0 &numberlist))
    (print &yor)
    (readit)
    (procure)
    (setq &xrt (nth 0 &numberlist)) 
     (print &xrt)
    (readit)
    (procure)
    (setq &yup (nth 0 &numberlist))
    (print &yup)
     
    (readit)
    (procure)
    (setq &xcol (nth 0 &numberlist))
    (print &xcol)
    (readit)
    (procure)
    (setq &ycol (nth 0 &numberlist))
    (print &ycol)
    (readit)
    (procure)
    (setq &column (nth 0 &numberlist))
    
    (print &column)

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


 
    (close &rfile)
   );end progn..
   );endif..
 ); end okuinp



;*********************************************************
;main
;*********************************************************
 (setvar "cmdecho" 0)
  (setq &nline 1000)
  (okuinp)
  (setq &rfile (open &wellfile "r"))
   (terpri)
   (setq &bukuyu (getint "Well Number   "))
   (print &bukuyu)
   

  (okuboth)
 );end findwell..
 
 
 
 
 
 

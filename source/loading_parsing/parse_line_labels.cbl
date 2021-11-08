      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-04
      * Last Modified: 2021-11-08
      * Purpose: During loading, populates the potential line label 
      *          table. These are destinations used by GOTO or GOSUB.
      *          Seeing that line labels will look like implicit SUB
      *          calls, these entries are not 100% guaranteed to be 
      *          correct (for now).
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. parse-line-labels.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       local-storage section.
       
       01  ls-cur-line-num-disp      pic 9(5).        
       01  ls-num-line-labels-disp   pic 9(4).      


       01  ls-keyword-check-ret-code pic 9.

       01  ls-assignment-count       pic 9(4).

       linkage section.       

       01  l-src-code-str            pic x(1024). 

       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_line_label_boundary_table.cpy".

       procedure division using 
           l-src-code-str l-cur-line-num l-line-label-boundary-table.   

       main-procedure.

           if l-src-code-str = spaces then 
               goback 
           end-if 

           call "logger" using concat(
               "LOAD:PARSE-LINE-LABELS :: enter with: " 
               trim(l-src-code-str))
           end-call 


            *>   If RETURN, assume it was for the last label found.
           if upper-case(l-src-code-str(1:length(ws-return)))
               = ws-return 
           then                               
               move l-cur-line-num to l-label-end(l-num-line-labels) 

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-line-labels to ls-num-line-labels-disp               
               call "logger" using concatenate(
                   "LOAD:PARSE-LINE-LABELS :: Found RETURN GOSUB for: " 
                   trim(l-label-name(l-num-line-labels))
                   " : at: " 
                   ls-cur-line-num-disp
                   " : number of labels: " ls-num-line-labels-disp)                   
               end-call 
               goback 
           end-if 


      *>   If not RETURN, assume new label.
      *>   Check if line is a keyword or assignment. If not, assume label.           
           call "is-keyword" using 
               l-src-code-str 
               ls-keyword-check-ret-code
           end-call 

           if ls-keyword-check-ret-code > 0 then 
               call "logger" using "**** IS KEYWORD, SKIPPING..."
               goback 
           end-if 

           inspect l-src-code-str
               tallying ls-assignment-count for all "="
           
           if ls-assignment-count > 0 then 
               call "logger" using "**** ASSIGNMENT!"
               goback 
           end-if 
             
      
           add 1 to l-num-line-labels
            
           move l-cur-line-num to l-label-start(l-num-line-labels) 
           
           move trim(upper-case(l-src-code-str))
           to l-label-name(l-num-line-labels)

           move l-cur-line-num to ls-cur-line-num-disp
           move l-num-line-labels to ls-num-line-labels-disp               
           
           call "logger" using concatenate(
               "LOAD:PARSE-LINE-LABELS :: Found new LABEL. Name: "
               trim(l-label-name(l-num-line-labels))
               " : START at: " 
               ls-cur-line-num-disp
               " : Number of labels: " ls-num-line-labels-disp)
           end-call 
           
           goback.         
       

       end program parse-line-labels.

      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-04
      * Last Modified: 2021-11-04
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
       01  ls-num-subs-disp          pic 9(4).       

       linkage section.       

       01  l-src-code-str            pic x(1024). 

       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_sub_boundary_table.cpy".

       procedure division using 
           l-src-code-str l-cur-line-num l-sub-boundary-table.   

       main-procedure.

      *> TODO: IMPLEMENT THIS. This is currently a copy/paste of SUB parsing

      *>   Check if line is a sub, if so add to sub table.

      *>   Subroutines cannot be defined inside another sub, so no
      *>   nesting is expected during the declaration process.
           if upper-case(l-src-code-str(1:length(ws-sub)))
               = ws-sub 
           then 
               add 1 to l-num-subs
            
               move l-cur-line-num to l-sub-start(l-num-subs) 
               move zero to l-sub-cur-nest(l-num-subs)

               move trim(upper-case(l-src-code-str(length(ws-sub):)))
                   to l-sub-name(l-num-subs)

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-subs to ls-num-subs-disp               
               call "logger" using concatenate(
                   "LOAD:PARSE-LINE-LABELS :: Found new LABEL. Name: "
                   trim(l-sub-name(l-num-subs))
                   " : START at: " 
                   ls-cur-line-num-disp
                   " : Number of subs: " ls-num-subs-disp)
               end-call 
               goback 
           end-if 

           if upper-case(l-src-code-str(1:length(ws-end-sub)))
               = ws-end-sub 
           then                               
               move l-cur-line-num to l-sub-end(l-num-subs) 

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-subs to ls-num-subs-disp               
               call "logger" using concatenate(
                   "LOAD:PARSE-LINE-LABELS :: Found END SUB for: " 
                   trim(l-sub-name(l-num-subs))
                   " : at: " 
                   ls-cur-line-num-disp
                   " : number of subs: " ls-num-subs-disp)                   
               end-call 
               
           end-if 

           goback.

       end program parse-line-labels.

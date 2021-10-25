      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-25
      * Last Modified: 2021-10-25
      * Purpose: During loading, populates sub table with start and end
      *          line locations.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. parse-subs.

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
       
       01  ls-cur-line-num-disp      pic 9(10).
       01  ls-num-subs-disp          pic 9(10).       

       linkage section.       

       01  l-src-code-str            pic x(1024). 

       01  l-cur-line-num            pic 9(10) comp.

       01  l-sub-boundary-table.
           05  l-num-subs            pic 9(10) comp. 
           05  l-sub-data            occurs 0 to 1000 times
                                     depending on l-num-subs.   
               10  l-sub-name        pic x(32).                                                 
               10  l-sub-start       pic 9(10). *>TODO Make comp 
               10  l-sub-end         pic 9(10).  
               10  l-sub-cur-nest    pic 9(4) value 0.
               10  l-sub-last-call   pic 9(10) occurs 1000 times.
                                     *>idx of last call is cur nest. 

       procedure division using 
           l-src-code-str l-cur-line-num l-sub-boundary-table.   

       main-procedure.

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
                   "PARSE-SUBS :: Found new SUB. Name: "
                   trim(l-sub-name(l-num-subs))
                   " : START at: " 
                   ls-cur-line-num-disp
                   " : Number of subs: " ls-num-subs-disp)
               end-call 
           end-if 

           if upper-case(l-src-code-str(1:length(ws-end-sub)))
               = ws-end-sub 
           then                               
               move l-cur-line-num to l-sub-end(l-num-subs) 

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-subs to ls-num-subs-disp               
               call "logger" using concatenate(
                   "PARSE-SUBS :: Found END SUB for: " 
                   trim(l-sub-name(l-num-subs))
                   " : at: " 
                   ls-cur-line-num-disp
                   " : number of subs: " ls-num-subs-disp)                   
               end-call 
               
           end-if 

           goback.

       end program parse-subs.

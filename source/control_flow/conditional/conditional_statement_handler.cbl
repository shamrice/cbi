      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-20
      * Last Modified: 2021-11-18
      * Purpose: Handles a conditional statement which can potentially 
      *          contain multiple conditionals. 
      *          Each section is broken up and passed to the the 
      *          conditional-processor to be evaluated.
      *          Returns if true (1) or false (0).
      *   NOTE : Does not currently follow parenthesis!
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. conditional-statement-handler.

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

       01  ls-cur-statement              pic x(1024).

       01  ls-conditional-ret-val        pic 9.

       01  ls-position-pointer           pic 9(4) comp.
       
       linkage section.       

       01  l-statement                   pic x(1024). 
        
       01  l-return-code                 pic 9 value 0.
           88  l-return-code-false       value 0.
           88  l-return-code-true        value 1.


       procedure division using 
           l-statement l-return-code.   

       main-procedure.

           set l-return-code-false to true 
           
           call "logger" using concatenate(
               "CONDITIONAL-STATEMENT-HANDLER :: Evaluating statement: "
               trim(l-statement))
           end-call            

           move 1 to ls-position-pointer

           perform until ls-position-pointer > length(l-statement)

               unstring l-statement 
                   delimited by ws-and 
                   into ls-cur-statement
                   with pointer ls-position-pointer
               end-unstring


               call "conditional-processor" using 
                   ls-cur-statement                   
                   ls-conditional-ret-val
               end-call 
      
      *>  TODO : keep track of AND/OR. then compare previous with current
      *>  using and/or to determine return value. If single value, 
      *>  ls-conditional-ret-val should be returned.

               move ls-conditional-ret-val to l-return-code

           end-perform 

           goback. 

       end program conditional-statement-handler.

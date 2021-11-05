      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-05
      * Last Modified: 2021-11-05
      * Purpose: Handles a IF statements and moves current line number 
      *          to the correct location based on processing of the 
      *          statement.      
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. if-handler.

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
      
       01  ls-line-text                  pic x(1024).

       01  ls-line-text-temp             pic x(1024).

       01  ls-conditional-ret-val        pic 9.       
       
       linkage section.       

       01  l-src-code-str                pic x(1024). 

       01  l-cur-line-num                pic 9(5) comp.

       copy "copybooks/linkage_section/l_if_boundary_table.cpy".

       copy "copybooks/linkage_section/l_variable_table.cpy".
                   

       procedure division using 
           l-src-code-str l-cur-line-num 
           l-if-boundary-table l-variable-table.   

       main-procedure.

           move upper-case(trim(l-src-code-str))
               to ls-line-text

           call "logger" using concatenate(
               "IF-HANDLER :: Evaluating statement: "
               trim(ls-line-text))
           end-call

      *>   Remove ending "THEN" if exists...
           move reverse(ls-line-text) to ls-line-text-temp

           inspect ls-line-text-temp  
           replacing first "NEHT" by spaces            

           move reverse(ls-line-text-temp) to ls-line-text 

           evaluate true 
               when ls-line-text(1:length(ws-if)) 
                   = ws-if             
                   perform handle-if

               when ls-line-text(1:length(ws-elseif))
                   = ws-elseif 
                   perform handle-elseif

      *>       ELSE is handled by the fact that the line num hasn't
      *>       already jumped to the END IF from prev checks, 

           end-evaluate

           goback.

       
       handle-if.

           inspect ls-line-text 
           replacing first ws-if by spaces 

           call "conditional-statement-handler" using 
               ls-line-text
               l-variable-table
               ls-conditional-ret-val
           end-call 

      *> TODO: 
      *> If true, move to next line, mark if condition as handled. when 
      *> reaches next elseif or else, will jump to end-if 
      
      *> if false, move to next elseif or else (if exists), if not, 
      *> move to end if.

           exit paragraph. 


       handle-elseif.

           inspect ls-line-text 
           replacing first ws-elseif by spaces 

           call "conditional-statement-handler" using 
               ls-line-text
               l-variable-table
               ls-conditional-ret-val
           end-call 

      *> TODO: 
      *> If true, move to next line. mark if condition as handled. when 
      *> reaches next elseif or else, will jump to end if.

      *> if false, move to next elseif or else (if exists). If not, move
      *> to end if.
                          
           exit paragraph. 

           

       end program if-handler.

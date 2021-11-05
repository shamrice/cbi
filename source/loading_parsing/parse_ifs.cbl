      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-05
      * Last Modified: 2021-11-05
      * Purpose: During loading, populates if table 
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. parse-ifs.

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

       copy "copybooks/linkage_section/l_if_boundary_table.cpy".

       procedure division using 
           l-src-code-str l-cur-line-num l-if-boundary-table.   

       main-procedure.

      *>   TODO : placeholder.

           goback.

       end program parse-ifs.

      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-15
      * Last Modified: 2021-11-16
      * Purpose: Process CHR$ - get character for ASCII code passed in.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       function-id. ascii-code-to-char.

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
       
       01  ls-variable-temp-data.
           05  ls-var-name           pic x(16).
           05  ls-var-type           pic x(8).
           05  ls-var-value          pic x(1024).
           05  ls-var-value-num      pic 9(16).
           05  ls-var-ret-code       pic 9.


       01  ls-ascii-code             pic 9(3).
       01  ls-working-input-value    pic x(1024).

       linkage section.       

       01  l-input-value             pic x(1024). 

       copy "copybooks/linkage_section/l_variable_table.cpy".

       01  l-char-value              pic x.

       procedure division 
           using l-input-value l-variable-table
           returning l-char-value.

             
       main-procedure.

           move upper-case(l-input-value) to ls-working-input-value

      *     call "logger" using ls-working-input-value

           inspect ls-working-input-value
               replacing 
                   all ws-chr by spaces 
                   all "(" by spaces 
                   all ")" by spaces 
           
           
      *     call "logger" using trim(ls-working-input-value)

           if trim(ls-working-input-value) is numeric then 
               move trim(ls-working-input-value) to ls-ascii-code           
           else
               perform get-value-from-variable
           end-if 
           
      *>   Code 000, 127, and 255 are blanks. Everything above 255 also
      *>   return blank even though invalid.
           if ls-ascii-code = 0 or ls-ascii-code = 127 
               or ls-ascii-code >= 255 then 
               move spaces to l-char-value
               perform generate-log-line
               goback 
           end-if 

      *>   Extended ASCII table not supported, return unknown char.
           if ls-ascii-code >= 128 then 
               move "?" to l-char-value
               perform generate-log-line
               goback             
           end-if 

           evaluate ls-ascii-code

      *> TODO: Special mappings for non-displayable ASCII codes. Must
      *> also match INKEY$ return so that can check, for example, 13 CR.

               when 24
                   move "▒" to l-char-value

               when 33
                   move "!" to l-char-value
               
               when 34
                   move '"' to l-char-value
               
               when 35
                   move "#" to l-char-value
                              
               when 36
                   move "$" to l-char-value
                              
               when 37
                   move "%" to l-char-value
                              
               when 38
                   move "&" to l-char-value
                              
               when 39
                   move "'" to l-char-value
                              
               when 40
                   move "(" to l-char-value
                              
               when 41
                   move ")" to l-char-value
                              
               when 42
                   move "*" to l-char-value
                              
               when 43
                   move "+" to l-char-value
                              
               when 44
                   move "," to l-char-value
                              
               when 45
                   move "-" to l-char-value
                              
               when 46
                   move "." to l-char-value
                              
               when 47
                   move "/" to l-char-value
                              
               when 48
                   move "0" to l-char-value
                              
               when 49
                   move "1" to l-char-value
                              
               when 50
                   move "2" to l-char-value
                              
               when 51
                   move "3" to l-char-value
                              
               when 52
                   move "4" to l-char-value
                              
               when 53
                   move "5" to l-char-value
                              
               when 54
                   move "6" to l-char-value
                              
               when 55
                   move "7" to l-char-value
                              
               when 56
                   move "8" to l-char-value
                              
               when 57
                   move "9" to l-char-value
                              
               when 58
                   move ":" to l-char-value
                              
               when 59
                   move ";" to l-char-value
                              
               when 60
                   move "<" to l-char-value
                              
               when 61
                   move "=" to l-char-value
                              
               when 62
                   move ">" to l-char-value
                              
               when 63
                   move "?" to l-char-value
                              
               when 64
                   move "@" to l-char-value
                              
               when 65
                   move "A" to l-char-value
                              
               when 66
                   move "B" to l-char-value
                              
               when 67
                   move "C" to l-char-value
                              
               when 68
                   move "D" to l-char-value
                              
               when 69
                   move "E" to l-char-value
                              
               when 70
                   move "F" to l-char-value
                              
               when 71
                   move "G" to l-char-value
                              
               when 72
                   move "H" to l-char-value
                              
               when 73
                   move "I" to l-char-value
                              
               when 74
                   move "J" to l-char-value
                              
               when 75
                   move "K" to l-char-value
                              
               when 76
                   move "L" to l-char-value
                              
               when 77
                   move "M" to l-char-value
                              
               when 78
                   move "N" to l-char-value
                              
               when 79
                   move "O" to l-char-value
                              
               when 80
                   move "P" to l-char-value
                              
               when 81
                   move "Q" to l-char-value
                              
               when 82
                   move "R" to l-char-value
                              
               when 83
                   move "S" to l-char-value
                              
               when 84
                   move "T" to l-char-value
                              
               when 85
                   move "U" to l-char-value
                              
               when 86
                   move "V" to l-char-value
                              
               when 87
                   move "W" to l-char-value
                              
               when 88
                   move "X" to l-char-value
                              
               when 89
                   move "Y" to l-char-value
                              
               when 90
                   move "Z" to l-char-value
                                             
               when 91
                   move "[" to l-char-value
                              
               when 92
                   move "\" to l-char-value
                              
               when 93
                   move "]" to l-char-value
                              
               when 94
                   move "^" to l-char-value
                              
               when 95
                   move "_" to l-char-value
                              
               when 96
                   move "`" to l-char-value
                              
               when 97
                   move "a" to l-char-value
                              
               when 98
                   move "b" to l-char-value
                              
               when 99
                   move "c" to l-char-value
                              
               when 100
                   move "d" to l-char-value
                             
               when 101
                   move "e" to l-char-value
                              
               when 102
                   move "f" to l-char-value
                              
               when 103
                   move "g" to l-char-value
                              
               when 104
                   move "h" to l-char-value
                              
               when 105
                   move "i" to l-char-value
                              
               when 106
                   move "j" to l-char-value
                              
               when 107
                   move "k" to l-char-value
                              
               when 108
                   move "l" to l-char-value
                              
               when 109
                   move "m" to l-char-value
                              
               when 110
                   move "n" to l-char-value
                              
               when 111
                   move "o" to l-char-value
                              
               when 112
                   move "p" to l-char-value
                              
               when 113
                   move "q" to l-char-value
                              
               when 114
                   move "r" to l-char-value
                              
               when 115
                   move "s" to l-char-value
                              
               when 116
                   move "t" to l-char-value
                              
               when 117
                   move "u" to l-char-value
                              
               when 118
                   move "v" to l-char-value
                              
               when 119
                   move "w" to l-char-value
                              
               when 120
                   move "x" to l-char-value
                              
               when 121
                   move "y" to l-char-value
                              
               when 122
                   move "z" to l-char-value
                              
               when 123
                   move "{" to l-char-value
                              
               when 124
                   move "|" to l-char-value
                              
               when 125
                   move "}" to l-char-value
                              
               when 126
                   move "~" to l-char-value
               
               when other 
                   move "?" to l-char-value
           end-evaluate

           perform generate-log-line

           goback.
           



       generate-log-line.
           call "logger" using concatenate(
               "ASCII-CODE-TO-CHAR :: Input: " trim(l-input-value) 
               " : ASCII Code to check: " ls-ascii-code
               " : Return value: " l-char-value)
           end-call 
           exit paragraph.



       get-value-from-variable.
           move trim(ls-working-input-value) to ls-var-name 
           call "get-var-value" using 
               l-variable-table
               ls-var-name 
               ls-var-type 
               ls-var-value
               ls-var-ret-code
           end-call 

           if ls-var-ret-code > 0 and ls-var-type = "INTEGER" then 
               move ls-var-value to ls-ascii-code
           else 
               move "?" to l-char-value
               call "logger" using concatenate(
                   "ASCII-CODE-TO-CHAR :: Failed to get numeric value "
                   " to check from variable: " trim(ls-var-name) 
                   " : Return value was: " trim(ls-var-value) 
                   " : Returning char value of '?'")
               end-call 
               goback 
           end-if 

           exit paragraph.

       end function ascii-code-to-char.

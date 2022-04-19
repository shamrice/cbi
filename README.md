# CBI - COBOL BASIC Interpreter 
CBI is a BASIC Interpreter written in GnuCOBOL. In its current state, it is just a proof of concept. I can't imagine a "real world" purpose for this beyond being something I'm having fun creating. 


The BASIC syntax supported by this application is based off of Microsoft's QBasic.


The current list of implemented (most likely not fully) commands can be found in the [BASIC Keywords Copybook](https://github.com/shamrice/cbi/blob/main/source/copybooks/basic_keywords.cpy)


**Building**

To build, go to the ```source``` folder and run ```make```. This will create a ```cbi``` executable in the bin directory that you can test run some BASIC source code against.


**Running**

Below are the application usage information:

```
Usage: cbi [OPTION]... [FILE]
 
Options:
      --run - Run the program after loading without prompt. (Default if no parameters are entered.)
     --list - Print out program source code to display.
  --logging - Turns on interpreter logging while running and/or listing a program.
     --help - This help text.
 
  [FILE] - File name of BASIC program to list and/or run.
```


**Video Demo**

Below is a video demonstrating CBI (left) running some old QBasic games along side DOSBox running them in the QuickBasic 4.5 (right).

[![Video running some QB games in QuickBasic4.5 side by side wtih CBI](https://img.youtube.com/vi/EG0zFT60MBk/0.jpg)](https://youtu.be/EG0zFT60MBk)



This readme will mostly likely be updated with more detailed information as things progress. 

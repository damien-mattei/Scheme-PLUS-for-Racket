# Scheme-PLUS-for-Racket
Scheme+ for DrRacket by Damien Mattei.

The documentation is here:
https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/gh-pages/README.md


With Racket you do not need to define/declare a variable with the
Scheme+ operator <+. Because the binding of the variable is autodetect
you can simply set the variable with <- (or :=) and Scheme+ will do the job.


The Racket GUI can not display the line of error in a Scheme+ program.
If you have to debug your source code you must generate a Scheme file
from your Scheme+ file with curly-infix2prefix4racket and load the
result file in Racket GUI. Then you will have all the debug
information of Racket on the scheme file.


What's next? i'm working on a cleaner version internally that will use
better the scheme macro systeme, that will rely more on macro than on
the parser. That will be faster due to more work at expansion stage of
the macro using syntax transformers and less job in evaluation stage
at run-time. Also i will allow an additionnal syntax (only for Racket)
asked by an user.

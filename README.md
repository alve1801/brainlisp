# brainlisp
Brainfuck interpreter written in microlisp

Assuming prog is a lisp of valid brainfuck instructions, (brainfuck prog) returns the stack after it's done executing (it does not currently support I/O) (also I haven't tested if the (quote (++[>+<-])) works)

"Valid" here means that its brackets are balanced and it doesn't go into negative tape

Also in the code it's referred to as a stack instead of a tape, please excuse that



Update: microlisp didn't define arithmetics, so I made it use church numerals instead, you're welcome ;)

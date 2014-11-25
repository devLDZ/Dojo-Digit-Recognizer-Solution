Dojo-Digit-Recognizer-Solution
==============================

Script.fsx file contains implementation of proposed during dojo algorithm. It's also using ParallelSeq library to make some operations running on many PC cores at the same time

Main.fs file is source file compiling to console application. It's a bit more developed, implements also others distance measurements algorithms and allows for very easy testing of different settings ( I recommend NOT using IMED algorithm, it's not optimized, not tested, and is rather slow)

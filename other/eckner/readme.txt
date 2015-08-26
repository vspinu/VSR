// Copyright 2011, 2012 by Andreas Eckner
// License: Copying and distribution of this file, with or without modification,
//          are permitted in any medium without royalty provided the copyright
//          notice and this notice are preserved.


#########
# Linux #
#########

Step 0: Download the file 'ts_alg.zip' and extract the content, then open a
        terminal and go to the directory that contains the extracted files.


### Run the demo under Linux

Enter './test' to run the demo.



### Compile the demo under Linux via makefile ###

1.) Run 'make'
make

2.) Run demo:
./test

3.) Clean up
make clean



### Compile the demo under Linux via shared library ###

1.) Create shared library
gcc -c -fPIC -Wall operators.c		
gcc -shared operators.o -oliboperators.so
gcc -shared operators.o -Wl,-soname,liboperators.so -oliboperators.so

2.) Copy shared library to directory of libraries
sudo cp liboperators.so /usr/lib

3.) Compile the demo
gcc -L./ -lm -loperators test.c -o test

4.) Run the demo:
./test

5.) Remove shared library from directory of libraries
sudo rm /usr/lib/liboperators.so



####################
# Windows (64 bit) #
####################

### Rung the demo ####

1.) Download the file 'ts_alg.zip' and extract the content.
2.) Double click the extracted file 'test.exe' to run the demo.



### Compile the demo in C via shared library ###

1.) Install a C compiler, for example, "MinGW" from www.mingw.org

2.) Download the file 'ts_alg.zip' and extract the content.

3.) Start the Windows command prompt (in the start menu, select "Run ...",
enter "cmd", and click "Ok").

4.) Go to the directory that contains the extracted files. For example, by entering:
cd c:\ts_alg

5.) Set up the Windows path enironment for the C compiler by entering, for example:
path = %PATH%;c:\mingw\bin

6.) Create DLL file
x86_64-w64-mingw32-gcc -c -Wall operators.c -o operators.o
x86_64-w64-mingw32-gcc -shared -o operators.dll operators.o

7.) Compile and link test program
x86_64-w64-mingw32-gcc -L. -loperators test.c -o test

8.) Run the demo:
test



#### Compile the demo directly (without shared library) ###

1.) Repeat steps 1-5 from above.

2.) Compile and link the demo:
x86_64-w64-mingw32-gcc test.c operators.c -o test.exe

3.) Run the demo:
test
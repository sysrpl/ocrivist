echo g++ -fPIC -g -Wall -c tess4pas.cc -o tesspas_cc.o
g++ -fPIC -g -Wall -c tessintf.cc -o tessintf_cc.o
echo -----

echo gcc -fPIC -g -Wall -x c -c tessintf.h -o tessintf_c.o
gcc -fPIC -g -Wall -x c -c tessintf.h -o tessintf_c.o
echo -----

echo gcc -shared -Wl,-soname,libtessintf.so.1 -o libtessintf.so.1.0.1 tessintf_c.o tessintf_cc.o /usr/local/lib/libtesseract_api.so -lc 
gcc -shared -Wl,-soname,libtessintf.so.1 -o libtessintf.so.1.0.1 tessintf_c.o tessintf_cc.o /usr/local/lib/libtesseract_api.so -lc 
echo -----

echo done!; echo; echo


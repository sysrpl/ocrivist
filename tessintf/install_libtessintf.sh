# /bin/sh

cd `dirname $0`

if ! test -w /usr/bin ; then
	echo You need to be root to install libtessintf
	exit 0
	fi

#if [ -e /usr/lib/libdargui.so ]; then rm /usr/lib/libdargui.so ; fi

cp -fv libtessintf.so.1.0.1 /usr/lib/

ldconfig -nv /usr/lib | grep libtessintf

ln -s /usr/lib/libtessintf.so.1.0.1 /usr/lib/libtessintf.so

echo done!
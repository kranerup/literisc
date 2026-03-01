src='../../literisc_gemini/c-testsuite/tests/single-exec'

files=(
00124.c ?
00130.c
00133.c
00134.c
00135.c
00136.c must preprocess
00137.c must preprocess
00144.c
00149.c
00150.c
#00095.c
#00096.c
#00103.c
#00107.c
#00114.c
#00115.c
#00121.c
#00020.c
#00036.c
#00038.c
#00044.c
#00045.c
#00046.c
#00047.c
#00048.c
#00049.c
#00050.c
#00052.c
#00053.c
#00054.c
#00055.c
#00077.c
#00087.c
#00088.c
#00089.c
#00090.c
#00091.c
#00092.c
#00093.c)

for f in ${files[@]}; do
    echo F $f
    ln -s ${src}/$f
done

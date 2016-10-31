inp=$1
order=$2
outdir="$(dirname "$inp")"
bname="${inp##*/}"
fname="${bname%.*}"
ext=".lm"
out="$outdir/$fname$ext"
bin/i686-m64/ngram-count -unk -order $order -text $inp -lm $out -wbdiscount

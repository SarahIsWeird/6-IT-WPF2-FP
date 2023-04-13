files=$(find . -name '*.hs')

for file in $files
do
	pushd $(dirname $file)
	ghc $(basename $file)
	popd
done


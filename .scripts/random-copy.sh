size=0;
tries=0;
dirs=();

# The maximum size, change this to whatever you want
limit=10000000

## The maximum number of times we will try to get
## another dir if the current one is too big.
maxtries=20

## While we have not reached the size limit
while [[ $size -lt $limit ]] ; do

## Find a random, non-empty directory from the source.
## sort -R does a  random sort and head -n 1 prints the first line.
dir="$(find $1/*/  -not -empty -type d | sort -R | head -n 1)"

## Get this dir's size
dsize=$(du -s "$dir" | cut -f 1)

## If this dir does not make us pass the limit
if [[  $((size + $dsize)) -le $limit ]]; then
  echo "Copying $dir" 1>&2
  ## Copy it to target
  cp -r "$dir" $2
  ## Add its size to $size
  let size+=$dsize
  ## If this dir makes us pass the limit
  ## try $maxtries times to find another that does not
else
    let tries++;
    if [[ $tries -gt $maxtries ]]; then
    echo "" 1>&2;
    echo "Final size =  $size"
    break;
    fi
fi
done;


# TODO: Don't copy duplicates
# TODO: Take limit as parameter
# TODO: Search deep
# TODO: Progress bar

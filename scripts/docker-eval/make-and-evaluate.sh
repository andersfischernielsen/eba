echo "Running docker container.." 
docker run  -v "$(pwd)/$1":"/linux/$(dirname "$2")" --env "HASH=$1" --env "FILE=$2" ubuntu-linux

find "$1/" ! -name "$(basename "$2")" -type f -exec rm -f {} +

echo "Running EBA.."
./../../bin/eba --loop-limit=1 --branch-limit=1 --dUa "$1/$(basename "$2")" | tee "$1/$1.txt"
echo "Done. Results have been written to $1/$1.txt."
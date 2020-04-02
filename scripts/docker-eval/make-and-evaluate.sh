echo "Running docker container.." 
docker run  -v "$(pwd)/$1":"/linux/$(dirname "$2")" --env "HASH=$1" --env "FILE=$2" ubuntu-linux

echo "Removing output not supported by EBA.."
sed -i.bak "/^_Static_assert/ d" "$1/$(basename "$2")" && rm "$1/$(basename "$2").bak"
sed -i.bak "s/asm __inline volatile/asm volatile/g" "linux/$2" && rm "$1/$(basename "$2").bak"

find "$1/" ! -name "$(basename "$2")" -type f -exec rm -f {} +

echo "Running EBA.."
./../../bin/eba --dUa "$1/$(basename "$2")" > "$1/$1.txt"
echo "Done. Results have been written to $1/$1.txt."
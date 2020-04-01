if [ ! -d "$(pwd)/linux" ]; then
	echo "Cloning linux kernel repository.." && git clone https://github.com/torvalds/linux.git
fi

echo "Running docker container.." 
docker run  --env "HASH=$1" --env "FILE=$2" -v "$(pwd)/linux":/linux ubuntu-linux

echo "Removing output not supported by EBA.."
sed -i.bak "/^_Static_assert/ d" "linux/$2" && rm "linux/$2.bak"
sed -i.bak "s/asm __inline volatile/asm volatile/g" "linux/$2" && rm "linux/$2.bak"

echo "Running EBA.."
./../../bin/eba --dUa "linux/$2"
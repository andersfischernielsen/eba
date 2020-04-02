echo "Changing directory.."
cd linux
echo "Removing .git/index.lock.."
rm .git/index.lock
echo "Checking out $HASH.."
git checkout --force $HASH
echo "Executing 'make allyesconfig'.."
make allyesconfig
echo "Executing 'make $FILE'.."
make $FILE
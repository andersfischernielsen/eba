# EBA: An effective bug finder for C

*Note:* This is the development branch for the EBA bug finder. This is a work in progress with bleeding edge feature branches. Things might break. 

EBA is a bug finder for C based on side-effect analysis and model-checking.

For now, you can use it to find double-lock and double-unlock bugs in the Linux kernel, by:

    git clone --depth=1 https://github.com/torvalds/linux.git
    cd linux
    make allyesconfig
    make $FILENAME.i
    ./eba {--dunlockaut ; --dlockaut ; --dlock ; --dunlock} $FILENAME.i

See the `--help` EBA parameter for more options. 

### Hows does it work?

It combines side-effect analysis and model-checking, check the website for more info: [http://www.iagoabal.eu/eba/](http://www.iagoabal.eu/eba/)

### Does it really find bugs?

Yes, it really does, check the website for more info: [http://www.iagoabal.eu/eba/](http://www.iagoabal.eu/eba/)

## Installation

See the [installation instructions](INSTALL.md).

## Running the tests

If you want to run the tests you will need to install _cram_, for instance using _pip_:

    sudo apt-get install python-pip
    sudo pip install cram

You should place _eba_ somewhere in your _$PATH_:

    cram test/*.t

#!/usr/bin/env bash
set -eo >/dev/null

help (){
    echo ""
    echo "USAGE: ./love-fennel.sh [FLAG] [OPTIONS]"
    echo ""
    echo "Run love with fennel! Note wrap.fnl is your entry point."
    echo ""
    echo "    -t : Target Directory"
    echo "    -o : Output love file"    
    echo "    -f : Fennel Library Location"
    echo "    -l : Love executable"
    echo "    -d : Dont download"
    echo "    -c : Clear Cache"
    echo "    -h : Help"
    echo ""
    echo "Example: Run wrap.fnl in current directory"
    echo "    ./love-fennel.sh"
    echo ""
    echo "Example: Run wrap.fnl in target directory"
    echo "    ./love-fennel.sh -t dir"
    echo ""
    echo "Example: Run target.fnl in target directory"
    echo "    ./love-fennel.sh -t dir/target.fnl"
    echo ""
    echo "Example: Compile target directory to output"
    echo "    ./love-fennel.sh -t dir -o sample.love"
    echo ""
    echo "Example: Pass in your own love exe and fennel lib"
    echo "    ./love-fennel.sh -f fennel.lua -l love -t dir"
    echo ""
    echo "Example: Prevent any downloads"
    echo "    ./love-fennel.sh -d -t dir"        
}

LOVE_CACHE_DIR=${HOME}/.cache/love-release/love
FENNEL_CACHE_DIR=${HOME}/.cache/fennel

FENNEL_LIB=$FENNEL_CACHE_DIR/Fennel/fennel.lua
LOVE_EXE=$LOVE_CACHE_DIR/love-prepared/dest/love
FILE="wrap"
# requires git love and lua

fennelSource="https://github.com/bakpakin/Fennel.git"

main="fennel = require(\"lib.fennel\")\n\
table.insert(package.loaders, fennel.make_searcher({correlate=true}))\n\
pp = function(x) print(fennel.view(x)) end\n\
local make_love_searcher = function(env)\n\
   return function(module_name)\n\
      local path = module_name:gsub(\"%.\", \"/\") .. \".fnl\"\n\
      if love.filesystem.getInfo(path) then\n\
         return function(...)\n\
            local code = love.filesystem.read(path)\n\
            return fennel.eval(code, {env=env}, ...)\n\
         end, path\n\
      end\n\
   end\n\
end\n\
\n\
table.insert(package.loaders, make_love_searcher(_G))\n\
table.insert(fennel[\"macro-searchers\"], make_love_searcher(\"_COMPILER\"))\n\
local stdio = require(\"lib.stdio\")
stdio.start()
require(\"wrap\")"

stdio="(require \"love.event\")
(fn prompt [cont?]
  (io.write (if cont? \"..\" \">> \")) (io.flush) (.. (io.read) \"\n\"))
(fn looper [event channel]
  (match (channel:demand)
    [:write vals] (do (io.write (table.concat vals \"\t\"))
                      (io.write \"\n\"))
    [:read cont?] (love.event.push event (prompt cont?)))
  (looper event channel))
(match ...
  (event channel) (looper event channel))
{:start (fn start-repl []
          (let [code (love.filesystem.read \"lib/stdio.fnl\")
                luac (if code
                         (love.filesystem.newFileData
                          (fennel.compileString code) \"io\")
                         (love.filesystem.read \"lib/stdio.lua\"))
                thread (love.thread.newThread luac)
                io-channel (love.thread.newChannel)
                coro (coroutine.create fennel.repl)
                options {:readChunk (fn [{: stack-size}]
                                      (io-channel:push [:read (< 0 stack-size)])
                                      (coroutine.yield))
                         :onValues (fn [vals]
                                     (io-channel:push [:write vals]))
                         :onError (fn [errtype err]
                                    (io-channel:push [:write [err]]))
                         :moduleName \"lib.fennel\"}]
            (coroutine.resume coro options)
            (thread:start \"eval\" io-channel)
            (set love.handlers.eval
                 (fn [input]
                   (coroutine.resume coro input)))))}"

check_fennel(){
    if ! test -f ${FENNEL_LIB}; then
        echo "No lua file found at $FENNEL_LIB"
        echo ""
        echo "    Either pass in fennel.lua using the -f option,"
        echo "    or don't pass in -d (dont-download)"
        exit 1
    fi
}

prep_dir(){
    check_fennel
    directory=$(mktemp -d -t love-XXXXXXXXXX)
    mkdir -p $directory/lib/    
    echo -e $main | sed s/"wrap"/$FILE/g > $directory/main.lua
    echo -e $stdio > $directory/lib/stdio.fnl
    cp -rf $TARGET/* $directory/    
    cp $FENNEL_LIB $directory/lib/    
    cd $directory
    echo $directory
}

compile(){
    directory=$(prep_dir)
    OLDDIR=$(pwd)
    pushd $directory 1>/dev/null
    zip -r "$OLDDIR/$OUTPUT" * 1>/dev/null
    popd 1>/dev/null
    echo "## Compilation Successful"
    echo "## To test run: love $OLDDIR/$OUTPUT"
}

run_love(){
    directory=$(prep_dir)
    echo "Dir$ $directory"
    if type tree 1>/dev/null 2> /dev/null ; then
        tree $directory
    else
        ls $directory
    fi
    if ! test -f ${LOVE_EXE}; then
        echo "No love executable found at $LOVE_EXE"
        echo ""
        echo "    Either pass in love using the -l option,"
        echo "     or don't pass in -d (dont-download)"
        exit 1
    fi    
    $LOVE_EXE $directory 2>/dev/null
}

downloadFennel (){
    CACHE_DIR=$FENNEL_CACHE_DIR
    if [ ! -d $CACHE_DIR ]; then
        LASTDIR=$(pwd)
        mkdir $CACHE_DIR
        cd $CACHE_DIR
        git clone $fennelSource
        cd Fennel
        make # requires lua to be installed
        cd $LASTDIR
    fi
    if ! test -f $CACHE_DIR/Fennel/fennel.lua; then
        echo "No source found in $CACHE_DIR/Fennel"
        exit 1
    fi
}

downloadLove (){
    ARCH="$(uname -m)"
    
    VERSION="11.3"

    LOVE_TAR_URL=https://github.com/love2d/love/releases/download/${VERSION}/love-${VERSION}-linux-${ARCH}.tar.gz
    CACHE_DIR=$LOVE_CACHE_DIR
    LOVE_TAR=$CACHE_DIR/love-${VERSION}-${ARCH}.tar.gz

    if ! test -d ${CACHE_DIR}; then
        mkdir -p ${CACHE_DIR}
    fi

    if ! test -f ${LOVE_TAR}; then
        curl -L -C - -o ${LOVE_TAR} ${LOVE_TAR_URL}
    fi

    if ! test -d $CACHE_DIR/love-prepared; then
        rm -rf $CACHE_DIR/love-prepared
        mkdir $CACHE_DIR/love-prepared
        tar xf ${LOVE_TAR} -C $CACHE_DIR/love-prepared
    fi
    
    if ! test -f ${LOVE_TAR}; then
        echo "No tarball found for $VERSION in $LOVE_TAR"
        exit 1
    fi
}

clear_cache(){
    rm -rf $FENNEL_CACHE_DIR
    rm -rf $LOVE_CACHE_DIR
}

TARGET=.
OUTPUT=0
DOWNLOAD=1

while getopts ":hf:g:t:o:dc" option; do    
    case $option in
        h)
            help
            exit 0;;
        c)
            clear_cache
            exit;;
        o)
            OUTPUT=$OPTARG;;
        t)
            echo $TARGET
            TARGET=$OPTARG
            if [ -f ${TARGET} ]; then
                FILE=$(basename $TARGET   | \
                           sed s/.fnl$//g | \
                           sed s/.lua$//g)
                TARGET=$(dirname $TARGET)                
            fi;;
        f)
            FENNEL_LIB=$OPTARG;;
        g)
            LOVE_EXE=$OPTARG;;
        d)
            DOWNLOAD=0
    esac    
done

if [ $DOWNLOAD == 1 ] ; then
    if [ ! -f $FENNEL_LIB ]; then
        downloadFennel
        FENNEL_LIB=$FENNEL_CACHE_DIR/Fennel/fennel.lua
        
    fi
    
    if [ ! -f $LOVE_EXE ]; then
        downloadLove
        LOVE_EXE=$LOVE_CACHE_DIR/love-prepared/dest/love
    fi    
fi

if [[ $OUTPUT == "0" ]]; then
    run_love
else
    compile
fi

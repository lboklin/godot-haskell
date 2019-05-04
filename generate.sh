APIFILE=$(readlink -f $1)
if [ -f $APIFILE ] ; then \
    cd classgen && \
        stack run godot-haskell-classgen $APIFILE && \
        rsync -avhq src/ ../src --remove-source-files
else
    echo "Need path to api.json as argument."
fi

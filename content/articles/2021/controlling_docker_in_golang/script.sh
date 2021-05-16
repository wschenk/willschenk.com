if [ ! -f "output" ]; then
    date > output
    cat output
    exit 1
fi

cat output
exit 0

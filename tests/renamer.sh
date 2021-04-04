for file in *.mc; do
    mv "$file" "$(basename "$file" .mc).bug"
done
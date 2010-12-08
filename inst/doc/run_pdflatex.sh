for f in `ls texs`
do
    pdflatex -interaction nonstopmode -output-directory PDFs texs/${f}
done

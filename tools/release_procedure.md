# Procedures for releasing package to CRAN

1. Bump version in DESCRIPTION and commit the change.
2. Reset repo status running `rm -rf * && git checkout -- .`.
3. Bundle C++ source code running `bash tools/bundle_symengine_source.sh`.
4. Build R package running `R CMD build .`.
5. Check R package running `R CMD check --as-cran symengine_*.tar.gz`.
6. Submit R package to [Win Builder](https://win-builder.r-project.org/) to check the package on Windows.
7. Submit R package to [CRAN](https://cran.r-project.org/submit.html).
8. After acceptance, create a git tag running `git tag -a -m "CRAN submission v9.9.9" v9.9.9`.


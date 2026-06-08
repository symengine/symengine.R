# Release Procedure

## Bump Version

1. Stage changes and bump the version in `DESCRIPTION`, then commit.

## Clean & Bundle

2. Clean the workspace — removes any untracked or modified files:
   ```
   git clean -dfxi && git checkout -- .
   ```
   Be careful of not removing unstaged changes that you want to keep.

3. Bundle the C++ source (SymEngine upstream):
   ```
   bash tools/bundle_symengine_source.sh
   ```

   Always redo the cleanup and bundling before building the package. This is necessary to
   remove certain git-ignored paths like `src/upstream`, `tools/SYMENGINE_BUNDLED`.

## Build & Check

4. Build the R package:
   ```
   R CMD build .
   ```
5. Verify the package:
   ```
   R CMD check --as-cran symengine_*.tar.gz
   ```

## Submit

6. Submit to [Win Builder](https://win-builder.r-project.org/) to check on Windows.
7. Submit to [CRAN](https://cran.r-project.org/submit.html).

## Tag

8. After acceptance, tag the release:
   ```
   git tag -a -m "CRAN submission v9.9.9" v9.9.9
   ```


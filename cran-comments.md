
## Resubmission

The following changes have been made:

0. Use single quote for software names in DESCRIPTION.

1. Fix "A R" -> "An R" in DESCRIPTION and README.

2. No change was made to fix print()/cat() usages.

   Original comment:
   
   > You write information messages to the console that cannot be easily suppressed.
   > Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) if you really have to write text to the console.
   > (except for print() and summary() functions)
   > F.i.: double_visitor.R
   
   Explanation:
   
   All of the print()/cat() usages in the package are inside show() S4 method.
   I think it is intended for show() to print to console.
   
   For example, see the Matrix package:
   https://github.com/cran/Matrix/blob/2d247f4d063c7f1a801285666cfb3e92e0ea3b3a/R/MatrixFactorization.R#L11

3. Elaborate Description field in DESCRIPTION file.

4. Update all the Rd files to include more details and add \value sections.

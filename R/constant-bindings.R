

#' @include c-interface.R


## ActiveBindings  =============================================================

# TODO...

# The error is:
#     ** preparing package for lazy loading
#     Error in .Call("c_builtin_const", id) : 
#         "c_builtin_const" not resolved from current namespace (symengine)
#     ERROR: lazy loading failed for package ‘symengine’
#
# Not sure why...

if (FALSE) {
    
Consts <- new.env()

makeActiveBinding("zero"       , function() Constant("zero"        ), Consts)
makeActiveBinding("one"        , function() Constant("one"         ), Consts)
makeActiveBinding("minus_one"  , function() Constant("minus_one"   ), Consts)
makeActiveBinding("I"          , function() Constant("I"           ), Consts)
makeActiveBinding("pi"         , function() Constant("pi"          ), Consts)
makeActiveBinding("E"          , function() Constant("E"           ), Consts)
makeActiveBinding("EulerGamma" , function() Constant("EulerGamma"  ), Consts)
makeActiveBinding("Catalan"    , function() Constant("Catalan"     ), Consts)
makeActiveBinding("GoldenRatio", function() Constant("GoldenRatio" ), Consts)
makeActiveBinding("Inf"        , function() Constant("Inf"         ), Consts)
makeActiveBinding("NegInf"     , function() Constant("NegInf"      ), Consts)
makeActiveBinding("ComplexInf" , function() Constant("ComplexInf"  ), Consts)
makeActiveBinding("Nan"        , function() Constant("Nan"         ), Consts)

}

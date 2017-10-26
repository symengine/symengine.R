

#' @include c-interface.R


# The error is:
#     ** preparing package for lazy loading
#     Error in .Call("c_builtin_const", id) : 
#         "c_builtin_const" not resolved from current namespace (symengine)
#     ERROR: lazy loading failed for package ‘symengine’
#
# Not sure why...

if (FALSE) {
    
Consts <- new.env()

makeActiveBinding("zero"       , function() get_builtin_const("zero"        ), Consts)
makeActiveBinding("one"        , function() get_builtin_const("one"         ), Consts)
makeActiveBinding("minus_one"  , function() get_builtin_const("minus_one"   ), Consts)
makeActiveBinding("I"          , function() get_builtin_const("I"           ), Consts)
makeActiveBinding("pi"         , function() get_builtin_const("pi"          ), Consts)
makeActiveBinding("E"          , function() get_builtin_const("E"           ), Consts)
makeActiveBinding("EulerGamma" , function() get_builtin_const("EulerGamma"  ), Consts)
makeActiveBinding("Catalan"    , function() get_builtin_const("Catalan"     ), Consts)
makeActiveBinding("GoldenRatio", function() get_builtin_const("GoldenRatio" ), Consts)
makeActiveBinding("Inf"        , function() get_builtin_const("Inf"         ), Consts)
makeActiveBinding("NegInf"     , function() get_builtin_const("NegInf"      ), Consts)
makeActiveBinding("ComplexInf" , function() get_builtin_const("ComplexInf"  ), Consts)
makeActiveBinding("Nan"        , function() get_builtin_const("Nan"         ), Consts)

}

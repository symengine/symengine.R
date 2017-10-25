

#' @include c-interface.R


# The error is:
#     ** preparing package for lazy loading
#     Error in .Call("c_get_const", x) : 
#         "c_get_const" not resolved from current namespace (symengine)
#     ERROR: lazy loading failed for package ‘symengine’
#
# Not sure why

if (FALSE) {
    
Consts <- new.env()

makeActiveBinding("zero"       , function() api_get_const("zero"        ), Consts)
makeActiveBinding("one"        , function() api_get_const("one"         ), Consts)
makeActiveBinding("minus_one"  , function() api_get_const("minus_one"   ), Consts)
makeActiveBinding("I"          , function() api_get_const("I"           ), Consts)
makeActiveBinding("pi"         , function() api_get_const("pi"          ), Consts)
makeActiveBinding("E"          , function() api_get_const("E"           ), Consts)
makeActiveBinding("EulerGamma" , function() api_get_const("EulerGamma"  ), Consts)
makeActiveBinding("Catalan"    , function() api_get_const("Catalan"     ), Consts)
makeActiveBinding("GoldenRatio", function() api_get_const("GoldenRatio" ), Consts)
makeActiveBinding("Inf"        , function() api_get_const("Inf"         ), Consts)
makeActiveBinding("NegInf"     , function() api_get_const("NegInf"      ), Consts)
makeActiveBinding("ComplexInf" , function() api_get_const("ComplexInf"  ), Consts)
makeActiveBinding("Nan"        , function() api_get_const("Nan"         ), Consts)

}

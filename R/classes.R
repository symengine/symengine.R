
setClass("SymEnginePTR", slots = c(ptr = "externalptr"))

setClass("Basic", contains = "SymEnginePTR")

setClass("VecBasic", contains = "SymEnginePTR")

setClass("DenseMatrix", contains = "SymEnginePTR")

## Class for dispatch purpose (TODO: maybe use class inheritance instead of union)
setClassUnion("SymEngineDataType", c("Basic", "VecBasic", "DenseMatrix"))

setClassUnion("BasicOrVecBasic", c("Basic", "VecBasic"))

## A context is an environment where symbols in the expression may be substituted from

setClass("SymEnginePTRWithContext", contains = "SymEnginePTR", slots = c(context = "environment"))

setClass("BasicWithContext", contains = c("Basic", "SymEnginePTRWithContext"))

setClass("VecBasicWithContext", contains = c("Basic", "SymEnginePTRWithContext"))

setClass("DenseMatrixWithContext", contains = c("Basic", "SymEnginePTRWithContext"))

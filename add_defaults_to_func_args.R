

# add evaluated formals to function definition
# these are evaluated at function definition time
# rather than invocation time. Use to add constants
# that take a lot of time to compute
# Example:
# > with_formals(function(x){x+a},list(a=2))
#   function (x, a = 2) 
#   {
#     x + a
#   }
with_formals<-function(fun,form){
  for(i in setdiff(names(form),"")){
    formals(fun)[[i]]<-form[[i]]
  }
  fun
}


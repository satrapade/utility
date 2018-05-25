
# find which environment contains a name
where<-function (name, env = parent.frame()) {
  if(!is.character(name))return(NULL)
  if(length(name)!=1)return(NULL)
  if(class(env)!="environment")return(NULL)
  if (identical(env, emptyenv())) return(NULL)
  if (exists(name, env, inherits = FALSE)) return(env)
  where(name, parent.env(env))
}





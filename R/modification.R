modification <-
function(canevas,modifs)
{
  for (i in 1:dim(canevas)[1])
  {
    for (j in 1:dim(modifs)[1])
    {
      canevas[i,1]=str_replace(canevas[i,1],modifs[j,1],modifs[j,2])
    }
  }
  return(canevas)
}

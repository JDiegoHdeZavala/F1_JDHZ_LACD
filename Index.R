VFVenc = function(A, t, r){
  VF = A*((1+r)**t-1)/r
  return(VF)
}

PagoVFVenc =  function(VF, t, r){
  A =  r*VF/((1+r)**t-1)
  return(A)
}

TasaVFVenc = function(VF, A, t){
  aprox = VF + 1
  VF2 = 0
  i = 1
  while(VF/10000 < aprox){
    r = i/10000
    VF2 = A*((1+r)**t-1)/r
    aprox = abs(VF-VF2)
    i = i + 1
  }
  return(r)
}

PeriodosVFVenc = function(VF, A, r){
  t = log(1+r*VF/A)/log(1+r)
  return(t)
}

VAVenc = function(A, t, r){
  VA = A*(1-(1+r)**-t)/r
  return(VA)
}

PagoVAVenc =  function(VA, t, r){
  A =  r*VA/(1-(1+r)**-t)
  return(A)
}

TasaVAVenc = function(VA, A, t){
  aprox = VA + 1
  VA2 = 0
  i = 1
  while(VA/10000 < aprox){
    r = i/10000
    VA2 = A*(1-(1+r)**-t)/r
    aprox = abs(VA-VA2)
    i = i + 1
  }
  return(r)
}

PeriodosVAVenc = function(VA, A, r){
  t = -log(1-VA/A*r)/log(1+r)
  return(t)
}

VFAnt = function(A, t, r){
  VF = (1+r)*A*((1+r)**t-1)/r
  return(VF)
}

PagoVFAnt =  function(VF, t, r){
  A =  r*VF/(((1+r)**t-1)*(1+r))
  return(A)
}

TasaVFAnt = function(VF, A, t){
  aprox = VF + 1
  VF2 = 0
  i = 1
  while(VF/10000 < aprox){
    r = i/10000
    VF2 = (1+r)*A*((1+r)**t-1)/r
    aprox = abs(VF-VF2)
    i = i + 1
  }
  return(r)
}

PeriodosVFAnt = function(VF, A, r){
  t = log(1+r*VF/(A*(1+r)))/log(1+r)
  return(t)
}

VAAnt = function(A, t, r){
  VA = A*((1-(1+r)**-t)/r)*(1+r)
  return(VA)
}

PagoVAAnt =  function(VA, t, r){
  A =  r*VA/((1-(1+r)**-t)*(1+r))
  return(A)
}

TasaVAAnt = function(VA, A, t){
  aprox = VA + 1
  VA2 = 0
  i = 1
  while(VA/10000 < aprox){
    r = i/10000
    VA2 = A*((1-(1+r)**-t)/r)*(1+r)
    aprox = abs(VA-VA2)
    i = i + 1
  }
  return(r)
}

PeriodosVAAnt = function(VA, A, r){
  t = -log(1-r*VA/(A*(1+r)))/log(1+r)
  return(t)
}

VADif = function(A, r, t, G){
  VA= (A*(1-(1+r)**-t)/r)/(1+r)**G
  return(VA)
}

PagoVADif = function(VA, t, r){
  A = r*VA/(1-(1+r)**-t)
  return(A)
}


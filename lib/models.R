mo = list(
  const = ModelObject(
    name = 'const',
    expr = expression( b0 ),
    P = list(
      p0 = c(b0 = 0.08),
      lb = c(b0 = 0.02),
      ub = c(b0 = 0.10)
    )
  ),
  
  lin = ModelObject(
    name = 'lin',
    expr = expression( b1*x ),
    P = list(
      p0 = c(b1 = 0),
      lb = c(b1 = 0),
      ub = c(b1 = 0.1)
    )
  ),
  
  sat1 = ModelObject(
    name = 'sat1',
    expr = expression( a1 + (a0-a1)*exp(-k*(x-td)) ),
    P = list(p0 = c(a0=0.08, a1=0.3,  k=0.1, td=0),
             lb = c(a0=0,    a1=0.08, k=0,   td=0),
             ub = c(a0=0.1,  a1=1,    k=1,   td=5))
  ),
  
  exp = ModelObject(
    name = 'exp',
    expr = expression( y0*exp(u*(x-tl)) ),
    P = list(p0 = c(u=0.3,  y0=0.1, tl=10), 
             lb = c(u=0.01, y0=0.01,tl=0), 
             ub = c(u=1,    y0=0.5, tl=Inf))
  )
)

mo$lin2 = mix(mo$const, mo$lin)

mo = mo[sort(names(mo))]


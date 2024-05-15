plot.axioms = function(pxy, pyz, pxz, PTx, PTy,
                       do.reg=TRUE, do.mul=TRUE, do.ternary=TRUE, do.context=FALSE,
                       between=2,
                       label=c('x', 'y', 'z'),
                       grid=c(0.25,0.5,0.75),
                       binary.pch=1, ternary.pch=20,
                       reg.col='blue', mul.col='red',
                       one.ce.color = '#F0F0F0',
                       both.ce.color = '#E0E0E0',
                       border=NA,
                       panel.label=NULL) {
  
  # Derived probabilities
  PTz = (1-PTx-PTy)
  pyx = (1-pxy)
  pzy = (1-pyz)
  pzx = (1-pxz)
  
  # Set up empty plot with no frame (with grid that will be overwritten)
  tripl = triplot(frame=FALSE, grid=grid)
  text(0, -0.4, panel.label, pos=1)

  # Set up context effect region based on object whose tripleton probability is favoured
  if (between==1) pc = c(pxy, pxz) # c=x, a=y, b=z
  if (between==2) pc = c(pyz, pyx) # c=y, a=z, b=x
  if (between==3) pc = c(pzx, pzy) # c=z, a=x, b=y

  if (do.context) {
    # Generic computation for context effect regions
    pca = pc[1]
    pac = 1-pca
    pcb = pc[2]
    pbc = 1-pcb
    pac = 1-pca
    pbc = 1-pcb
    den = pac*pcb + pca*pbc + pca*pcb
    r1a = c(1, pac*pcb/den, pac, 1) # Region 1, context effect in one direction only
    r1b = c(0, pca*pbc/den, 0, 0)
    r1c = c(0, pca*pcb/den, pca, 0)
    r2a = c(0, pac*pcb/den, 0, 0)   # Region 2, context effect in other direction only
    r2b = c(1, pca*pbc/den, pbc, 1)
    r2c = c(0, pca*pcb/den, pcb, 0)
    r3a = c(pac, pac*pcb/den, 0, 0, pac) # Region 3, context effect in both directions
    r3b = c(0, pca*pbc/den, pbc, 0, 0)
    r3c = c(pca, pca*pcb/den, pcb, 1, pca)
    
    # Plotting, according to between/dissimilar object
    if (between==1) {
      polygon(tritrafo(r1c, r1a, r1b), col=one.ce.color, border=border)
      polygon(tritrafo(r2c, r2a, r2b), col=one.ce.color, border=border)
      polygon(tritrafo(r3c, r3a, r3b), col=both.ce.color, border=border)
    }
    if (between==2) {
      polygon(tritrafo(r1b, r1c, r1a), col=one.ce.color, border=border)
      polygon(tritrafo(r2b, r2c, r2a), col=one.ce.color, border=border)
      polygon(tritrafo(r3b, r3c, r3a), col=both.ce.color, border=border)
    }
    if (between==3) {
      polygon(tritrafo(r1a, r1b, r1c), col=one.ce.color, border=border)
      polygon(tritrafo(r2a, r2b, r2c), col=one.ce.color, border=border)
      polygon(tritrafo(r3a, r3b, r3c), col=both.ce.color, border=border)
    }
  }
  
  triframe(label=label)
  trigrid(grid)
  
  # Sides of triangle
  tripoints(pxy, pyx, 0.0, pch=binary.pch)
  tripoints(0.0, pyz, pzy, pch=binary.pch)
  tripoints(pxz, 0.0, pzx, pch=binary.pch)
  
  # Centre of triangle
  if (do.ternary) tripoints(PTx, PTy, PTz, pch=ternary.pch)
  
  # Regularity and multiplicative regions
  pmin.x = pxy*pxz
  pmax.x = min(pxy, pxz)
  pmin.y = pyx*pyz
  pmax.y = min(pyx, pyz)
  pmin.z = pzx*pzy
  pmax.z = min(pzx, pzy)
  
  cycle = pxy + pyz + pzx
  if (do.reg && cycle <= 2 && cycle >= 1) {
    trilines(c(pmax.x, pmax.x, 1-pmax.y-pmax.z, pmax.x),
             c(pmax.y, 1-pmax.x-pmax.z, pmax.y, pmax.y),
             c(1-pmax.x-pmax.y, pmax.z, pmax.z, 1-pmax.x-pmax.y), col=reg.col)
  }
  if (do.mul) {
    trilines(c(pmin.x, pmin.x, 1-pmin.y-pmin.z, pmin.x),
             c(pmin.y, 1-pmin.x-pmin.z, pmin.y, pmin.y),
             c(1-pmin.x-pmin.y, pmin.z, pmin.z, 1-pmin.x-pmin.y),
             col=mul.col)
  }
  tripl
}



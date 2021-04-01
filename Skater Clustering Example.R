library(spdep)

bh <- st_read(system.file("etc/shapes/bhicv.shp",
                          package="spdep")[1], quiet=TRUE)

st_crs(bh) <- "+proj=longlat +ellps=WGS84"

dpad <- data.frame(scale(as.data.frame(bh)[,5:8]))

summary(dpad)

### neighboorhod list
bh.nb <- poly2nb(bh)

### calculating costs
lcosts <- nbcosts(bh.nb, dpad)

### making listw
nb.w <- nb2listw(bh.nb, lcosts, style="B")

### find a minimum spanning tree
mst.bh <- mstree(nb.w,5)

### the mstree plot
par(mar=c(0,0,0,0))
plot(st_geometry(bh), border=gray(.5))
plot(mst.bh, coordinates(as(bh, "Spatial")), col=2, 
     cex.lab=.6, cex.circles=0.035, fg="blue", add=TRUE)

### three groups with no restriction
res1 <- skater(mst.bh[,1:2], dpad, 2)

### groups size
table(res1$groups)

### the skater plot
opar <- par(mar=c(0,0,0,0))
plot(res1, coordinates(as(bh, "Spatial")), cex.circles=0.035, cex.lab=.7)

### the skater plot, using other colors
plot(res1, coordinates(as(bh, "Spatial")), cex.circles=0.035, cex.lab=.7,
     groups.colors=heat.colors(length(res1$ed)))

### the Spatial Polygons plot
plot(st_geometry(bh), col=heat.colors(length(res1$edg))[res1$groups])


require(gpclib)

# Making up some polygon coordinates
pc1 <- round(matrix(c(0, 14.97691, 27, 19.76114, 0, -18.93525, -25, -14.60762, 18, 14.97691, 0, -19.76114, -29, -18.93525, 0, 14.60762), nrow=8, ncol=2),3)
pc2 <- round(matrix(c(0, 31.47414, 51, 34.95360, 0, -34.95360, -51, -31.46414, 40, 31.47414, 0, -34.95360, -48, -34.95360, 0, 31.47414), nrow=8, ncol=2),3)

# Distance to move polygon coordinates N-S or E-W
m.ns <- cbind(c(rep(0,8)), c(rep(50,8)))
m.ew <- cbind(c(rep(50,8)), c(rep(0,8)))

# Arranging neighboring trees (i.e moving trees N-S, or E-W)
n1 <- pc2+m.ns; n2 <- pc2+m.ew; n3 <- pc2-m.ns; n4 <- pc2-m.ew

# Converting each tree to polygon
ptr1 <- as(pc1, "gpc.poly"); pn1 <- as(n1, "gpc.poly"); pn2 <- as(n2, "gpc.poly"); pn3 <- as(n3, "gpc.poly"); pn4 <- as(n4, "gpc.poly") 

# Combining neighbors into single polygon
pn1a.comb <- append.poly(setdiff(pn1, pn2), pn2)
pn1b.comb <- append.poly(setdiff(pn3, pn4), pn4)
pn1.comb <- append.poly(setdiff(pn1a.comb, pn1b.comb), pn1b.comb)

a <- setdiff(pn1, pn2)
b <- setdiff(pn2, pn3)
c <- setdiff(pn3, pn4)
d <- setdiff(pn4, pn1)
comb1 <- append.poly(a,b)
comb2 <- append.poly(comb1,c)
comb3 <- append.poly(comb2,d)

#alt with union:
pa <- union(pn1,pn2)
pb <- union(pa,pn3)
pc <- union(pb,pn4)


# Plotting just to visualise
plot(pn1.comb)
plot(ptr1,add=T)

plot(NULL, xlim=c(-100,100), ylim=c(-100,100), asp=1)
plot(comb3,add=T)

plot(pb)

# Calculating intersect
ol.f1 <- area.poly(intersect(ptr1, pn1.comb)) 




###### Examples:
# Example to calculate matrix of polygon coordinates
#t1 <- c(26,23,20,13) # E, N, S, W
n <- 23
s <- 20
e <- 26
w <- 13
ne <- n*e/(sqrt(n^2+e^2))
nw <- n*w/(sqrt(n^2+w^2))
se <- s*e/(sqrt(s^2+e^2))
sw <- s*w/(sqrt(s^2+w^2))
N <- c(0,23)
NE <- c(ne, ne)
E <- c(26,0)
SE <- c(se, -se)
S <- c(0,-20)
SW <- c(-sw, -sw)
W <- c(-13,0)
NW <- c(-nw, nw)

t1 <- rbind(N, NE, E, SE, S, SW, W, NW)
t2 <- t1 # just as example (would usually be calculating again by itself)
d <- 50
t2 <- t1 + cbind(c(rep(0,8)), c(rep(d,8)))
t3 <- t1 + cbind(c(rep(d,8)), c(rep(0,8)))
t4 <- t1 - cbind(c(rep(d,8)), c(rep(0,8)))
t5 <- t1 - cbind(c(rep(0,8)), c(rep(d,8)))

# plot(t1, xlim = c(-100,100), ylim = c(-100,100), asp=1)
# par(new=T)
# plot(t2, pch = 16, xlim = c(-100,100), ylim = c(-100,100), asp=1)

p1 <- as(t1,"gpc.poly")
p2 <- as(t2,"gpc.poly")
p3 <- as(t3,"gpc.poly")
p4 <- as(t4,"gpc.poly")
p5 <- as(t5,"gpc.poly")

plot(t1, asp=1)

## Example (construct simple example with known areas etc)
require(gpclib)

# Make up data for five polygons (centre polygon and )
p1 <- cbind(c(1,5,10,15,20,15,10,5),c(10,5,1,5,10,15,20,15))
p2 <- p1+cbind(c(rep(10,8)), c(rep(0,8)))
p3 <- p1+cbind(c(rep(0,8)), c(rep(10,8)))
p4 <- p1-cbind(c(rep(10,8)), c(rep(0,8)))
p5 <- p1-cbind(c(rep(0,8)), c(rep(10,8)))

p1 <- cbind(c(1,5,10,15,20,15,10,5),c(10,5,1,5,10,15,20,15))
p2 <- p1+cbind(c(rep(5,8)), c(rep(0,8)))
p3 <- p1+cbind(c(rep(0,8)), c(rep(10,8)))
p4 <- p1-cbind(c(rep(5,8)), c(rep(0,8)))
p5 <- p1-cbind(c(rep(0,8)), c(rep(10,8)))

p1 <- cbind(c(1,5,10,15,20,15,10,5),c(10,5,1,5,10,15,20,15))
p2 <- p1+cbind(c(rep(8,8)), c(rep(0,8)))
p3 <- p1+cbind(c(rep(0,8)), c(rep(12,8)))
p4 <- p1-cbind(c(rep(8,8)), c(rep(0,8)))
p5 <- p1-cbind(c(rep(0,8)), c(rep(12,8)))

# This converts x y coordinates into polygons (I think data should be listed in anticlockwise circle and last point will be joined with first)
p1 <- as(p1,"gpc.poly")
p2 <- as(p2, "gpc.poly")
p3 <- as(p3, "gpc.poly")
p4 <- as(p4, "gpc.poly")
p5 <- as(p5, "gpc.poly")

# p1 <- as(p1,"gpc.poly.nohole")
# p2 <- as(p2, "gpc.poly.nohole")
# p3 <- as(p3, "gpc.poly.nohole")
# p4 <- as(p4, "gpc.poly.nohole")
# p5 <- as(p5, "gpc.poly.nohole")

# # Combine the neighboring trees into a single polygon (note that can't do more than two trees at once... and not sure if this is right, not sure what happening to the double overlap area)
# pcomb <- append.poly(p2,p3)
# pcomb <- append.poly(pcomb,p4)
# pcomb <- append.poly(pcomb,p5)
# This is the way to combine neighboring trees into a single polygon, counting once any overlapping areas among these trees
p.comb <- append.poly(setdiff(p2, p3), p3)
p.comb <- append.poly(setdiff(p.comb,p4),p4)
p.comb <- append.poly(setdiff(p.comb,p5), p5)
plot(p.comb)

# p.comb <- as(p.comb, "gpc.poly.nohole")
# area.poly(p.comb)
# plot(p.comb)
# plot(p.comb, poly.args = list(col = 2, border = 0))

plot(append.poly(p1,p.comb))
area.poly(p1)
area.poly(intersect(p1,p.comb))
area.poly(p1)-area.poly(intersect(p1,p.comb)) # this gives the area not overlapping for the middle polygon... maybe??

# area.poly(p2)+area.poly(p3)-(area.poly(intersect(p2,p3))*2)
# 190*2-32.65
# require(PBSmapping)


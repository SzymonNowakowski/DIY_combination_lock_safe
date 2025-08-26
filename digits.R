digits <- list(
  one=matrix(c(F, T, F, F, F, F, T, rep(T, 7), rep(F, 6), T), ncol=3),
  two=matrix(c(F, T, F, F, F, T, T, T, F, F, T, T, F, T, F, T, T, F, F, F, T), ncol=3),
  three=matrix(c(F, T, rep(F, 3), T, F, T, F, F, T, F, F, T, F, T,T, F, T, T, F), ncol=3),
  four=matrix(c(F, F, T, T, F, F, F, F, T, F, rep(T, 4), T, F, F, T, F, F, F), ncol=3),
  five=matrix(c(rep(T, 4), F, T, F, T, F, F, T, F, F, T, T, F, F, T, T, T, F), ncol=3),
  six=matrix(c(F, rep(T, 5), F, T, F, F, T, F, F, T, F, T, F, F, T, T, F), ncol=3),
  seven=matrix(c(T, rep(F, 4), T, T, T, F, F, T, T, F, F, rep(T, 3), rep(F, 4)), ncol=3),
  eight=matrix(c(F, T, T, F, T, T, F, T, F, F, T, F, F, T, F, T, T, F, T, T, F), ncol=3),
  nine=matrix(c(F, T, T, F, F, T, F, T, F, F, T, F, F, T, F, rep(T, 5), F), ncol=3),
  zero=matrix(c(F, rep(T, 5), F, T, rep(F, 5), T, F, rep(T, 5), F), ncol=3) )

# Function to plot a digit matrix
plot_digit <- function(mat, title="") {
  nr <- nrow(mat)
  nc <- ncol(mat)
  
  plot(
    1, type="n",
    xlim=c(0, nc), ylim=c(0, nr),
    xaxt="n", yaxt="n", xlab="", ylab="", bty="n",
    main=title, asp=1
  )
  
  for (i in 1:nr) {
    for (j in 1:nc) {
      xleft <- j-1
      ybottom <- nr-i
      xright <- j
      ytop <- nr-i+1
      
      rect(
        xleft, ybottom, xright, ytop,
        col = if (mat[i,j]) "black" else "white",
        border = "gray"
      )
    }
  }
}

png("digits.png", width=800, height=400)
# Plot all digits in one window
par(mfrow=c(2,5), mar=c(1,1,2,1))
for (nm in names(digits)) {
  plot_digit(digits[[nm]], title=nm)
}
dev.off()
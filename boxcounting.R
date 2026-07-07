#install.packages("imager")
require(imager)

## set working directory in Session tab above to where the image file
## is stored.
image <- load.image("./images/pica.jpeg")

plot(image)

#pixel values in matrix form
img<-image[,,1,1]

##code below has been taken out from the package fractD in R.

  box.size = c(1,2,4,8,16,32,64,128,256,512)  
  box <- c()
  dat <- NULL

  ## counts number of pixels that contains the image

    for (l in box.size) {
      
        #dividing the img into l*l blocks 
        nr <- ceiling(nrow(img)/l)
        nc <- ceiling(ncol(img)/l)
        
        #creating an empty matrix to contain the pixel data of the l*l blocks
        newM <- matrix(NA, nr*l, nc*l)
        newM[1:nrow(img), 1:ncol(img)] <- img
        
        #matrix(seq_len(nr*nc), nr, byrow = TRUE) assigns a number to each small block
        #div_k counts the number of non-zero pixel entries in the boxes
        div_k <- kronecker(matrix(seq_len(nr*nc), nr, byrow = TRUE), matrix(1, l, l))
        
        #treat the data as numerical value 
        div_k <- as.numeric(div_k)
        newM <- as.numeric(newM)
        
        imgs <- newM[order(div_k)]
        d <- gl(length(imgs)/(l*l),(l*l))
        ## Exercise: write what the command is doing ?
        
        n_count <- sum(tapply(imgs, d, prod, na.rm = T) ==0)
        box[paste(l)] <- n_count
        ## Exercise: write what the command is doing ?
      }
    

  dat <- rbind(dat, data.frame(box))
  Data <- data.frame(BoxSize=box.size, NumberofBoxes=dat$box)
  #colnames(Data)<-c("Box Size", "Number of Boxes")
  Data
  

  ## plot boxes versus box size
  boxes = Data$NumberofBoxes
  invsize = 1/Data$BoxSize
  plot (log(as.numeric(invsize)), log(as.numeric(boxes)), pch = 19, 
        xlab = "log-Size", ylab = "log-Number of boxes",  main = "Log-log plot")
  abline(lm(log(box)~ log(as.numeric(invsize))))

  ## R-estimate of Box Dimension
    BoxdimEst= lm(log(box)~ log(as.numeric(invsize)))$coefficients[2]
    BoxdimEst

        
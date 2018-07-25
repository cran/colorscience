

#   functions here are copied here from 'munsellinterpol'
#   because package 'colorscience' depends on them.



xyz2srgb <- function(XYZ){
if ((length(XYZ) %% 3) != 0)  stop('XYZ matrix must be n x 3')
if (is.null(dim(XYZ))) if (length(XYZ)>2) XYZ<-matrix(XYZ, ncol=3,byrow=TRUE)
M <- matrix(c(3.2406, -1.5372, -0.4986, -0.9689, 1.8758, 0.0415, 0.0557, -0.2040, 1.0570),3,3,byrow=TRUE)
RGB <- t(M %*% t(XYZ))
# START: lines added March 2013 to set out-of-gamut flag.  
# The out-of-gamut flag is a column vector of Boolean true/false values.  Each
# entry corresponds to one row of the input matrix XYZ.
NumberOfInputs <- dim(RGB)[1]
OutOfGamutFlag <- -99 * matrix(1,NumberOfInputs)
for (index in 1:NumberOfInputs){
if (RGB[index,1] < 0 || RGB[index,1] > 1 || RGB[index,2] < 0 || RGB[index,2] > 1 || RGB[index,3] < 0 || RGB[index,3] > 1) OutOfGamutFlag[index] = TRUE else OutOfGamutFlag[index] <- FALSE
}
# END: lines added March 2013 to set out-of-gamut flag
RGB[which(RGB<0)] <- 0
RGB[which(RGB>1)] <- 1
DACS = matrix(0,dim(XYZ)[1],3)
index <- RGB<=0.0031308
index[index==TRUE] <- 1
index[index==FALSE] <- 0
DACS <- DACS+index*(12.92*RGB)
DACS <- DACS+(1-index)*(1.055*RGB^(1/2.4)-0.055)
RGB <- ceiling(DACS*255)
RGB[which(RGB<0)] <- 0
RGB[which(RGB>255)] <- 255
return(list(Status.ind  = 1, sRGB=RGB, OutOfGamutFlag=OutOfGamutFlag))
}


#   for compatibility with package colorscience
#
#   MunsellName     character N-vector of Munsell notations
#
#   return value
#       Nx3 matrix of character strings
#
MunsellHVC  <- function( MunsellName )
    {
    out = strsplit( MunsellName, "[ ]+|/" )
    
    fixup <- function( vec )
        {
        if( vec[1]=='N'  &&  vec[length(vec)] != '0' )  vec = c( vec, '0' )
        
        if( length(vec) == 3 )
            return(vec)
        else
            return( rep(NA_character_,3) )
        }
        
    out = lapply( out, fixup )      #; print(out)
        
    n   = length(out)        
        
    out = unlist( out )

    #   if( length(out) != 3*n )  out = rep( NA_character_, 3*n )

    out = matrix( out, nrow=n, 3, byrow=TRUE )
    
    colnames(out) = c('H','V','C')
    
    return(out)

    # return( HVCfromMunsellName( MunsellName )  )
    }


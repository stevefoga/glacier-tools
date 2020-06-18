########################################################################################################
## script:             terminus_picker_v3.R
#
## Author:             Steve Foga
## Contact:            fogaste@ku.edu OR steve.foga@gmail.com
## Creation Date:      10 APR 2013
## Modification Date:  05 MAY 2014
#
#
## Purpose:            A semi-automated method of detecting the glacier terminus position on
##                     remotely-sensed imagery. Uses difference in brigthness of adjacent pixels
##                     in order to do guess the terminus location.
# 
#
## Recent updates:  1) Added "black to white" and "white to blue" color palettes for picker.
##                  2) Corrected MODIS naming conventions
##                  3) Added "previous good pick" to plot window
##                  4) Added binary "good image (1)/bad image (0) column to output CSV file
##
##
#
## Known issues:    1) (Minor) The "picked" pixel indicators are sometimes not easy to see, or change colors during a picking session.
##                  2) (Minor) The first preview image comes up as rainbow colors on the first run.
#
#
## Proposed updates: 1) (DONE) Add multiple color schemes for "Interactive Mode" (Currently Earth tones only.) 
##                   2) (DONE) Adding separate digitization tool for sediment plumes (i.e. Rink Isbrae). DONE
##                   3) Allowing the picker to handle more than one row value per column of data. (not necessary if picked correctly)
#
##

#
## Picker tools: A) "Interactive Mode"    Allows the user to edit and/or add picks. Close window by clicking the Windows "X" when finished.
##                    1) "Left-click" = add picks.
##                    2) "Right-click" = revert to previous pick. Can be done for multiple undos.
##               B) "Plume Dileniation"   Pick the border of a sediment plume. Tool then calculates the area and adds it to the output CSV. 
##                    1) "Left-click" = add points.
##                    2) "Right-click" = delete points.
##               C) "Melange Dileniation"   Pick the border of an ice melange. Tool works the same as the "plume dileniation" tool.
##               D) "SAVE Image and Continue"   If the picks are satisfactory, then continue to the next image.
##               E) "DISCARD Image"   If image is cloudy and/or has systematic errors, discard from terminus area calculation. Sets all picks to NA.
##               F) "Refresh"   Click to re-generate points on preview raster. 
##               G) "List"  Shows picks and their index number. For informational use only. 
#                
#
########## Load libraries ##########
library(sp)           # Base GIS package
library(raster)       # For raster I/O, algebra
library(rgdal)        # For additional GIS features not covered by 'sp' or 'raster'
#tclRequire("Tktable") # For loading table functionality
library(tkrplot)      # For plotting interactive point grid
library(tcltk2)       # Tcl/tk GUI 


########## User input ##########
## Specify year
#year <- 2009
year <- 2014

## Specify location: "east" or "west" Greenland? 
## (west = Rink, KG, Jakobshavn; east = Helheim, Kangerdlugssuaq)
region <- "east"
#region <- "west"

## Input directory
dir_in = "/home/chrx/Downloads/picker/images/"
#dir_in <- "C:/Users/fogaste/Desktop/2008_hhg_test/"
#dir_in  <- "C:/Users/fogaste/Documents/Projects/2012_08_21_Helheim_Sea_Ice_Cover/helheim_geotiff/2008_terra_merge_8bit3/" 
#dir_in <- 'C:/Users/fogaste/Documents/Projects/rink_isbrae/2012_terra_rink/'

## Output directory (dir. does not have to exist)
#dir_out <- "C:/Users/fogaste/Documents/Projects/2012_08_21_Helheim_Sea_Ice_Cover/helheim_geotiff/2009_terra_terminus_area_test/" 
dir_out <- '/home/chrx/Downloads/picker/out/'

## Input shapefile (bouding box for area calculation)
#shp_in <- "C:/Users/fogaste/Documents/Projects/rink_isbrae/shapefiles/rink_term_bound.shp"
shp_in <- "/home/chrx/Downloads/picker/kgl_term_box.shp"

## OPTIONAL: Use a mask to further clip down melange raster extent
#mel_shp_in <- "C:/Users/fogaste/Documents/Projects/rink_isbrae/shapefiles/rink_boundary.shp"
#mel_shp_in <- "C:/Users/fogaste/Documents/Projects/2012_08_21_Helheim_Sea_Ice_Cover/helheim_geotiff/Shapefiles/helheim_boundary_main.shp"

######################### Automated section #########################
## determine OS (for plotting)
ptf = .Platform$OS.type

## Check if output folder exists, if not, create it.
dir.create(dir_out,showWarnings=FALSE)
shp <- shapefile(shp_in)
if (exists("mel_shp_in")){
  mel_shp <- shapefile(mel_shp_in)
}

## Get files
fn_in <- list.files(path = dir_in, pattern = ".tif", full.names=TRUE) # Entire path + file names

## Find dates, slope and resulting terminus position of each image
im_date <- terminus <- term_y <- rep(NA,length(fn_in))
## Make NULL variable to dynamically store values
true_ct <- NULL
## Create an empty list to fill with x coordinates
xs <- fit <- xs_final <- rep(list(list()),length(fn_in))
## Define focal matrix
#kern <-     matrix(c(6,6,6,0,0,0,-1,-1,-1),nrow=3)
#kern <- matrix(c(0,-1,0,-1,5,-1,0,-1,0),nrow=3)
#sobel_x <-  matrix(c(-1,-2,-1,0,0,0,1,2,1),nrow=3)
#sobel_y <-  matrix(c(1,0,-1,2,0,-2,1,0,-1),nrow=3)
#sharp <-    matrix(c(0,-1,0,-1,8,-1,0,-1,0),nrow=3)
#emboss <-   matrix(c(-2,-1,0,-1,1,1,0,1,2),nrow=3)
#hipass <- matrix(c(-0.7,-1,-0.7,-1,4,-1,-0.7,-1,-0.7),nrow=3)
for (idx in 1:length(fn_in)){
  ## Find julian date of each image (YYYYDDD)
  im_name <- strsplit(fn_in[idx],split="/")
  im_name <- im_name[[1]][length(im_name[[1]])]  
  ## Find the date string and write to new variable
  strng <- regexpr(year,im_name)
  im_date[idx] <- substr(im_name,strng[1]+nchar(year),strng[1]+nchar(year)+2)
  ## Read in raster
  print(fn_in[idx])
  rin <- raster(fn_in[idx],band=1)
  ## make sure raster is 8 bit, else do conversion
  #if (dataType(rin) != "INT1U"){
  #  rin <- calc(rin, fun=function(x){((x - min(x)) * 255)/(max(x)- min(x)) + 0})
  #}
  ## Store original spatial resolution (for later calculations)\
  orig_sp_res <- res(rin)[1]
  ## Store dynamic spatial resolution (used for re-sampling; this value will change)
  sp_res <- res(rin)[1]
  ## If the raster resolution is finer than MODIS, then resample the pixels. Otherwise picking takes too long & images load too slowly.
  if (sp_res <= 10){
    resamp_grid <- raster(nrow=(nrow(rin)/10),ncol=(ncol(rin)/10),crs=crs(rin),xmn=xmin(rin),xmx=xmax(rin),ymn=ymin(rin),ymx=ymax(rin))
    rin <- resample(rin,resamp_grid, method='ngb')
  } else if (sp_res < 30 & sp_res >= 15){
    resamp_grid <- raster(nrow=(nrow(rin)/5),ncol=(ncol(rin)/5),crs=crs(rin),xmn=xmin(rin),xmx=xmax(rin),ymn=ymin(rin),ymx=ymax(rin))
    rin <- resample(rin,resamp_grid,method='ngb')
  } else if (sp_res <= 50 & sp_res >= 30) {
    resamp_grid <- raster(nrow=(nrow(rin)/3),ncol=(ncol(rin)/3),crs=crs(rin),xmn=xmin(rin),xmx=xmax(rin),ymn=ymin(rin),ymx=ymax(rin))
    rin <- resample(rin,resamp_grid,method='ngb')
  }
  ## Crop image to fjord shapefile
  #rin <- crop(rin,mel_shp)
  #rin <- mask(rin,mel_shp,snap='in')
  ## Determinine its spatial resolution
  #sp_res <- res(rin)[1]

  ## Set display rasters for the picker
  rast <- crop(rin,shp,snap='in')
  #rast <- mask(rast,shp)
  #rast <- rast^2
  ## Convert original raster into a matrix
  rast_mat <- matrix(rast[],nrow=rast@nrows,ncol=rast@ncols,byrow=TRUE)
  ## Find center of study area
  x_start_idx <- length(rast_mat[,1])
  ## Add column names to terminus position matrix
  name_col <- rep(NA,x_start_idx)
  for (cidx in 1:x_start_idx){
    name_col[cidx] <- paste("c",cidx,sep="")
  }
  ys <- 1:x_start_idx
  # Fill all of variable 'xs' with NA values
  xs[[idx]] <- rep(NA,length(ys))
  for (i in 1:length(ys)){
  ## If there is no max (NA values only) then pass through an NA...
    if (length(which.max(diff(rast_mat[i,1:dim(rast_mat)[2]]))) == 0){
      xs[[idx]][i] <- NA
    ## ...else, replace the index with the maximum value in the row
    }else{
      xs[[idx]][i] <- (which.max(diff(rast_mat[i,1:dim(rast_mat)[2]]))+1)
    }
  }
  ## Find median xs value
  med <- median(sort(xs[[idx]]))
  ## Eliminate any values that are 10 pixels or more away from the median
  xs[[idx]][which(xs[[idx]] <= med-15 | xs[[idx]] >= med+15)] <- NA
  ## Output count of values with NAs
  cat("Image ",im_date[idx],": ",length(xs[[idx]][is.na(xs[[idx]])])," NA values.\n",sep="")

######################### Picker section ######################### 
  ## Initialize bomb (counter that tells the for loop to break)
  bomb <- 0
  ## Initialize undo count (so multiple undos can be performed by user)
  undo_ct <- 0
  ## Create empty vectors for area calculations
  area_sq_km <- plume_sq_km <- melange_sq_km <- Pl.XX <- Pl.YY <- Me.XX <- Me.YY <- rep(NA,length(fn_in))
  bomb3_ct <- 0
  ## Initialize previous value variable (storage container for undo function & plume digitization)
  prev_value_x <- prev_value_y <- Pl.X <- Pl.Y <- Me.X <- Me.Y <- prev_value_plx <- prev_value_ply <- NULL
    
    if (length(which(is.na(xs[[idx]])==TRUE)) == length(ys)) { ## If all values are NA 
      next
    } else {
      rplot <- brick(rin)
      #rplot_c <- crop(rplot,shp,snap='in')
      #rplot_c <- mask(rplot_c,shp)
      tmp_rast <- rast <- rplot_c <- crop(rin,shp,snap='in')
      #tmp_rast <- rast <- mask(tmp_rast,shp)
      ## Re-create ys to match clipped raster
      ys <- 1:dim(rast)[1]
      const_rast_mat <- matrix(tmp_rast[],nrow=tmp_rast@nrows,ncol=tmp_rast@ncols,byrow=TRUE)
      ## For each xs value, ask if the user wants to change the value
      for (pidx in 1:length(xs[[idx]])+bomb3_ct){
        ## LEGACY: If the "back" command was called, load all data from previous iteration
        if (bomb == 3){ pidx <<- pidx-2 }
        if (bomb3_ct > 0){ pidx <- pidx-bomb3_ct }
        pt_mat <- pt_mat_disp <- matrix(data=NA,nrow=dim(rast)[1],ncol=dim(rast)[2])
        ## LEGACY: move rows manually; highlight row with different values
        for (ridx in 1:length(xs[[idx]])){
          if (pidx==ridx){
            pt_mat_disp[ys[ridx],xs[[idx]][ridx]] <- 201
            pt_mat[ys[ridx],xs[[idx]][ridx]] <- 0
          }else{
            pt_mat_disp[ys[ridx],xs[[idx]][ridx]] <- 200
            pt_mat[ys[ridx],xs[[idx]][ridx]] <- 255
          }
        }
        pt_rast <- raster(pt_mat,crs=rast@crs, xmn=rast@extent@xmin, ymn=rast@extent@ymin,
                          xmx=rast@extent@xmax, ymx=rast@extent@ymax)
        pt_rast_disp <- raster(pt_mat_disp,crs=rast@crs, xmn=rast@extent@xmin, ymn=rast@extent@ymin,
                               xmx=rast@extent@xmax, ymx=rast@extent@ymax)
        
        ## Display plume data (if available)
        pl_pt_mat <- matrix(data=NA,nrow=dim(rast)[1],ncol=dim(rast)[2])
        if (length(Pl.X) > 0){
          # Remove NA values
          if (any(is.na(Pl.X))){
            Pl.X <- Pl.X[-which(is.na(Pl.X))]
            Pl.Y <- Pl.Y[-which(is.na(Pl.Y))]
          }
          for (ridx in 1:length(Pl.X)){
            pl_pt_mat[Pl.Y[ridx],Pl.X[ridx]] <- 201
          }
        }
        pl_pt_rast <- raster(pl_pt_mat,crs=rast@crs, xmn=rast@extent@xmin, ymn=rast@extent@ymin,
                             xmx=rast@extent@xmax, ymx=rast@extent@ymax)
        
        ## Display melange data (if available)
        me_pt_mat <- matrix(data=NA,nrow=dim(rin)[1],ncol=dim(rin)[2])
        if (length(Me.X) > 0){
          if (any(is.na(Me.X))){
            Me.X <- Me.X[-which(is.na(Me.X))]
            Me.Y <- Me.Y[-which(is.na(Me.Y))]
          }
          for (ridx in 1:length(Me.X)){
            me_pt_mat[Me.Y[ridx],Me.X[ridx]] <- 50
          }
        }
        me_pt_rast <- raster(me_pt_mat,crs=rin@crs, xmn=rin@extent@xmin, ymn=rin@extent@ymin,
                             xmx=rin@extent@xmax, ymx=rin@extent@ymax)
        
        ## Re-plot the data
        graphics.off()
        ## Plot the entire study area
	if (ptf == "windows"){
	  windows(xpos=1000,ypos=0)
	  plot(rplot,stretch="hist")
          plot(shp,add=TRUE)
          ## Plot the high-contrast raster + the terminus points
          windows(xpos=1000,ypos=400)
	} else {
	  x11(xpos=1000,ypos=0)
          plot(rplot,stretch="hist")
          plot(shp,add=TRUE)
	  x11(xpos=1000,ypos=400)
	}  
        
        
        plot(rast,col=palette(gray(0:255/255)),xaxt='n',yaxt='n')
        bkpts <- c(0,50,200,201,250)
        colors <- c("black","yellow","green","red","black")
        plot(pt_rast_disp,breaks=bkpts,col=colors,legend=FALSE,add=TRUE)
        plot(t(pl_pt_rast),breaks=bkpts,col=colors,legend=FALSE,add=TRUE)
        #axis(1,at=seq(xmin(rast),xmax(rast)-sp_res,sp_res),cex.axis=0.8,las=2,tck=1,pos=ymin(rast),labels=1:dim(rast)[2])
        #axis(2,at=seq(ymin(rast),ymax(rast),sp_res),cex.axis=0.7,tck=1,las=2,labels=(dim(rast)[1]+1):1)
        ## Set to break loop if it is changed to 1
        if (bomb == 2 & idx == 2){
          xs[[1]] <- rep(NA,length(xs[[1]]))
        } else if (bomb == 2){
          xs[[idx]] <- rep(NA,x_start_idx)
        }
        ## Reset bomb to 0
        bomb <- 0
        ## Find all the rows that still have NA values
        na_rows <- which(is.na(xs[[idx]]))
        ## "Interactive" button: move to interactive menu, and l-click to change point position
        on.int <- function(){
          plotFxn <- function(){
            ## Set RGB palette for digitized points
            rgb.palette <- colorRampPalette(c("blue", "yellow"))
            test.pal <- colorRampPalette(c("#660000","#FF4D4D"))
            ## Turn digitized points into a raster
            for (pidx in 1:length(xs[[idx]])){
              pt_mat <- pt_mat_disp <- matrix(data=NA,nrow=dim(rast)[1],ncol=dim(rast)[2])
              for (ridx in 1:length(xs[[idx]])){
                if (pidx==ridx){
                  pt_mat_disp[ys[ridx],xs[[idx]][ridx]] <- 201
                  pt_mat[ys[ridx],xs[[idx]][ridx]] <- 0
                }else{
                  pt_mat_disp[ys[ridx],xs[[idx]][ridx]] <- 200
                  pt_mat[ys[ridx],xs[[idx]][ridx]] <- 255
                }
              }
            }
              pt_rast <- raster(pt_mat,crs=rast@crs, xmn=rast@extent@xmin, ymn=rast@extent@ymin,
                                xmx=rast@extent@xmax, ymx=rast@extent@ymax)
            ## Plot raster + digitized points
            par(mar=c(0,0,0,0))
            ## Plot image with terrain colors
            ## Plot image with black-and-white colors
            bw.pal <- colorRampPalette(c("black","white"))
            image(rast,main="",axes=FALSE,col=bw.pal(500))
            par(new=TRUE)
            image(pt_rast,main="",axes=FALSE)
            bkpts <- c(0,50,200,201,250)
            colors <- c("black","yellow","green","red","black")
          }
          require(tcltk2)
          tt2 <- tktoplevel()
          img <- tkrplot(tt2,fun=plotFxn,hscale=1,vscale=1)
          tkpack(img)
          tkgrid(img)
          ## Find dimensions of tk plot image (this is different than the raster dimensions)
          img_wid <- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
          img_hei <- as.numeric(tclvalue(tkwinfo("reqheight",img)))
          ## Find dimensions of raster image
          ras_wid <- dim(rast)[2]
          ras_hei <- dim(rast)[1]
          ## If pixel dimensions are not square (i.e. rectangular), find the dimensions of each one to adjust pixel count
          pix_hei <- res(rast)[2]
          pix_wid <- res(rast)[1]
          pix_wid <- pix_hei <- 1
          ## Find pixel size of tk image based upon raster dimensions. Use this to adjust all input clicks
          w_x_factor <- (img_wid/ras_wid)*pix_wid
          h_y_factor <- (img_hei/ras_hei)*pix_hei
          #cat("wx factor:",w_x_factor,'\n')
          #cat("hy factor:",h_y_factor,'\n')
          mvPoint <- function(xClick,yClick){
            ## All x and y-clicks are divided by a fixed factor, since each pixel is stretched to fit a coordinate grid.
            ## Store original value in variable (for undo function)
            if (length(prev_value_x) == 0){
              ## Rounding
              prev_value_x <<- xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)]
              prev_value_y <<- ceiling(as.numeric(yClick)/h_y_factor)
            } else {
              prev_value_x <<- append(prev_value_x,xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)])
              prev_value_y <<- append(prev_value_y,ceiling(as.numeric(yClick)/h_y_factor))
            }
            ## Rounding
            xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)] <<- ceiling(as.numeric(xClick)/w_x_factor)
            cat("x click:",xClick)
            cat("y click:",yClick)
            ## No rounding
            #xs[[idx]][(as.numeric(yClick)/h_y_factor)] <<- as.numeric(xClick)/w_x_factor
            cat("X var:",ceiling((as.numeric(xClick))/w_x_factor),'\n')
            cat("Y var:",ceiling((as.numeric(yClick))/h_y_factor),'\n')
          }
          ## Function to add points
          OnLeftClick <- function(x,y){
            xClick <- x
            yClick <- y
            ## Call the move point function
            mvPoint(xClick,yClick)
            ## Print the new coordinates in the console window
            print(xClick)
            print(yClick)
            ## Set the undo count back to zero
            undo_ct <<- 0
            ## Re-plot data after each click
            tkrreplot(img)
          }
          ## Function to undo last l-click action
          OnRightClick <- function(){
            if (length(prev_value_x) > 0){
              
              if (is.na(prev_value_y[length(prev_value_y)-undo_ct])){
                ## If the point didn't exist previously, make it disappear when 'undo' is called
                xs[[idx]][prev_value_y[length(prev_value_y)-undo_ct]] <<- NA
              }else{
                ## Replace the xs[[idx]] value with the previous value
                xs[[idx]][prev_value_y[length(prev_value_y)-undo_ct]] <<- prev_value_x[length(prev_value_x)-undo_ct]
              }
              ## Advance the undo count
              undo_ct <<- undo_ct + 1
              cat(paste("Undo count:",undo_ct,"\n"))
              cat(paste("X move to:",prev_value_x[length(prev_value_x)-undo_ct],"\n"))
              cat(paste("Y move to:",prev_value_y[length(prev_value_y)-undo_ct],"\n"))
            }
            tkrreplot(img)
          }
          ## Assign function to l-click mouse button
          tkbind(img,"<Button-1>",OnLeftClick)
          ## Assign undo function to r-click mouse button
          tkbind(img,"<Button-3>",OnRightClick)
          tkconfigure(img,cursor="hand2")
          if (exists("tt2")){ tkwait.window(tt2) }
        }
        
        ## "Interactive - previous picks" button: plot previous picks instead of predictive picks
        on.int.prev <- function(){
          plotFxn <- function(){
            ## Set RGB palette for digitized points
            #rgb.palette <- colorRampPalette(c("white", "black"))
            #test.pal <- colorRampPalette(c("#660000","#FF4D4D"))
            
            ## If any, determine which good previous picks to load
            if (length(true_ct) > 0 & any(true_ct == 1)){
              one_idx <- which(true_ct == 1)
              last_idx <- one_idx[length(one_idx)]
              xs[[idx]] <<- xs[[last_idx]]
              for (pidx in 1:length(xs[[last_idx]])){
                pt_mat <- pt_mat_disp <- matrix(data=NA,nrow=dim(rast)[1],ncol=dim(rast)[2])
                for (ridx in 1:length(xs[[last_idx]])){
                  if (pidx==ridx){
                    pt_mat_disp[ys[ridx],xs[[last_idx]][ridx]] <- 201
                    pt_mat[ys[ridx],xs[[last_idx]][ridx]] <- 0
                  }else{
                    pt_mat_disp[ys[ridx],xs[[last_idx]][ridx]] <- 200
                    pt_mat[ys[ridx],xs[[last_idx]][ridx]] <- 255
                  }
                }
              }
            } else {## Else, load predicted picks as normal
              for (pidx in 1:length(xs[[idx]])){
                pt_mat <- pt_mat_disp <- matrix(data=NA,nrow=dim(rast)[1],ncol=dim(rast)[2])
                for (ridx in 1:length(xs[[idx]])){
                  if (pidx==ridx){
                    pt_mat_disp[ys[ridx],xs[[idx]][ridx]] <- 201
                    pt_mat[ys[ridx],xs[[idx]][ridx]] <- 0
                  }else{
                    pt_mat_disp[ys[ridx],xs[[idx]][ridx]] <- 200
                    pt_mat[ys[ridx],xs[[idx]][ridx]] <- 255
                  }
                }
              }
            }            
            ## Turn digitized points into a raster
            pt_rast <- raster(pt_mat,crs=rast@crs, xmn=rast@extent@xmin, ymn=rast@extent@ymin,
                              xmx=rast@extent@xmax, ymx=rast@extent@ymax)
            ## Plot raster + digitized points
            par(mar=c(0,0,0,0))
            ## Plot image with black-and-white colors
            white.black.pal <- colorRampPalette(c("white","black"))
            image(rast,main="",axes=FALSE,col=white.black.pal(500))
            par(new=TRUE)
            image(pt_rast,main="",axes=FALSE)
            bkpts <- c(0,50,200,201,250)
            colors <- c("black","yellow","green","red","black")
          }
          require(tcltk2)
          tt2 <- tktoplevel()
          img <- tkrplot(tt2,fun=plotFxn,hscale=2,vscale=2)
          tkpack(img)
          tkgrid(img)
          ## Find dimensions of tk plot image (this is different than the raster dimensions)
          img_wid <- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
          img_hei <- as.numeric(tclvalue(tkwinfo("reqheight",img)))
          ## Find dimensions of raster image
          ras_wid <- dim(rast)[2]
          ras_hei <- dim(rast)[1]
          ## If pixel dimensions are not square (i.e. rectangular), find the dimensions of each one to adjust pixel count
          pix_hei <- res(rast)[2]
          pix_wid <- res(rast)[1]
          pix_wid <- pix_hei <- 1
          ## Find pixel size of tk image based upon raster dimensions. Use this to adjust all input clicks
          w_x_factor <- (img_wid/ras_wid)*pix_wid
          h_y_factor <- (img_hei/ras_hei)*pix_hei
          mvPoint <- function(xClick,yClick){
            ## All x and y-clicks are divided by a fixed factor, since each pixel is stretched to fit a coordinate grid.
            ## Store original value in variable (for undo function)
            if (length(prev_value_x) == 0){
              prev_value_x <<- xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)]
              prev_value_y <<- ceiling(as.numeric(yClick)/h_y_factor)
            } else {
              prev_value_x <<- append(prev_value_x,xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)])
              prev_value_y <<- append(prev_value_y,ceiling(as.numeric(yClick)/h_y_factor))
            }
            xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)] <<- ceiling(as.numeric(xClick)/w_x_factor)
            cat("x click:",xClick)
            cat("y click:",yClick)
            cat("X var:",ceiling((as.numeric(xClick))/w_x_factor),'\n')
            cat("Y var:",ceiling((as.numeric(yClick))/h_y_factor),'\n')
          }
          ## Function to add points
          OnLeftClick <- function(x,y){
            xClick <- x
            yClick <- y
            ## Call the move point function
            mvPoint(xClick,yClick)
            ## Print the new coordinates in the console window
            print(xClick)
            print(yClick)
            ## Set the undo count back to zero
            undo_ct <<- 0
            ## Re-plot data after each click
            tkrreplot(img)
          }
          ## Function to undo last l-click action
          OnRightClick <- function(){
            if (length(prev_value_x) > 0){
              
              if (is.na(prev_value_y[length(prev_value_y)-undo_ct])){
                ## If the point didn't exist previously, make it disappear when 'undo' is called
                xs[[idx]][prev_value_y[length(prev_value_y)-undo_ct]] <<- NA
              }else{
                ## Replace the xs[[idx]] value with the previous value
                xs[[idx]][prev_value_y[length(prev_value_y)-undo_ct]] <<- prev_value_x[length(prev_value_x)-undo_ct]
              }
              ## Advance the undo count
              undo_ct <<- undo_ct + 1
              cat(paste("Undo count:",undo_ct,"\n"))
              cat(paste("X move to:",prev_value_x[length(prev_value_x)-undo_ct],"\n"))
              cat(paste("Y move to:",prev_value_y[length(prev_value_y)-undo_ct],"\n"))
            }
            tkrreplot(img)
          }
          ## Assign function to l-click mouse button
          tkbind(img,"<Button-1>",OnLeftClick)
          ## Assign undo function to r-click mouse button
          tkbind(img,"<Button-3>",OnRightClick)
          tkconfigure(img,cursor="hand2")
          if (exists("tt2")){ tkwait.window(tt2) }
        }
        
        ## "Interactive - Blue to white" button: move to interactive menu, and l-click to change point position
        on.int.blue <- function(){
          plotFxn <- function(){
            ## Set RGB palette for digitized points
            rgb.palette <- colorRampPalette(c("blue", "yellow"))
            test.pal <- colorRampPalette(c("#660000","#FF4D4D"))
            ## Turn digitized points into a raster
            for (pidx in 1:length(xs[[idx]])){
              pt_mat <- pt_mat_disp <- matrix(data=NA,nrow=dim(rast)[1],ncol=dim(rast)[2])
              for (ridx in 1:length(xs[[idx]])){
                if (pidx==ridx){
                  pt_mat_disp[ys[ridx],xs[[idx]][ridx]] <- 201
                  pt_mat[ys[ridx],xs[[idx]][ridx]] <- 0
                }else{
                  pt_mat_disp[ys[ridx],xs[[idx]][ridx]] <- 200
                  pt_mat[ys[ridx],xs[[idx]][ridx]] <- 255
                }
              }
            }
            pt_rast <- raster(pt_mat,crs=rast@crs, xmn=rast@extent@xmin, ymn=rast@extent@ymin,
                              xmx=rast@extent@xmax, ymx=rast@extent@ymax)
            ## Plot raster + digitized points
            par(mar=c(0,0,0,0))
            ## Plot image with terrain colors
            ## Plot image with black-and-white colors
            blue.white.pal <- colorRampPalette(c("blue","white"))
            image(rast,main="",axes=FALSE,col=blue.white.pal(500))
            par(new=TRUE)
            image(pt_rast,main="",axes=FALSE)
            bkpts <- c(0,50,200,201,250)
            colors <- c("black","yellow","green","red","black")
          }
          require(tcltk2)
          tt2 <- tktoplevel()
          img <- tkrplot(tt2,fun=plotFxn,hscale=2,vscale=2)
          tkpack(img)
          tkgrid(img)
          ## Find dimensions of tk plot image (this is different than the raster dimensions)
          img_wid <- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
          img_hei <- as.numeric(tclvalue(tkwinfo("reqheight",img)))
          ## Find dimensions of raster image
          ras_wid <- dim(rast)[2]
          ras_hei <- dim(rast)[1]
          ## If pixel dimensions are not square (i.e. rectangular), find the dimensions of each one to adjust pixel count
          pix_hei <- res(rast)[2]
          pix_wid <- res(rast)[1]
          pix_wid <- pix_hei <- 1
          ## Find pixel size of tk image based upon raster dimensions. Use this to adjust all input clicks
          w_x_factor <- (img_wid/ras_wid)*pix_wid
          h_y_factor <- (img_hei/ras_hei)*pix_hei
          mvPoint <- function(xClick,yClick){
            ## All x and y-clicks are divided by a fixed factor, since each pixel is stretched to fit a coordinate grid.
            ## Store original value in variable (for undo function)
            if (length(prev_value_x) == 0){
              prev_value_x <<- xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)]
              prev_value_y <<- ceiling(as.numeric(yClick)/h_y_factor)
            } else {
              prev_value_x <<- append(prev_value_x,xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)])
              prev_value_y <<- append(prev_value_y,ceiling(as.numeric(yClick)/h_y_factor))
            }
            xs[[idx]][ceiling((as.numeric(yClick))/h_y_factor)] <<- ceiling(as.numeric(xClick)/w_x_factor)
            cat("x click:",xClick)
            cat("y click:",yClick)
            cat("X var:",ceiling((as.numeric(xClick))/w_x_factor),'\n')
            cat("Y var:",ceiling((as.numeric(yClick))/h_y_factor),'\n')
          }
          ## Function to add points
          OnLeftClick <- function(x,y){
            xClick <- x
            yClick <- y
            ## Call the move point function
            mvPoint(xClick,yClick)
            ## Print the new coordinates in the console window
            print(xClick)
            print(yClick)
            ## Set the undo count back to zero
            undo_ct <<- 0
            ## Re-plot data after each click
            tkrreplot(img)
          }
          ## Function to undo last l-click action
          OnRightClick <- function(){
            if (length(prev_value_x) > 0){
              
              if (is.na(prev_value_y[length(prev_value_y)-undo_ct])){
                ## If the point didn't exist previously, make it disappear when 'undo' is called
                xs[[idx]][prev_value_y[length(prev_value_y)-undo_ct]] <<- NA
              }else{
                ## Replace the xs[[idx]] value with the previous value
                xs[[idx]][prev_value_y[length(prev_value_y)-undo_ct]] <<- prev_value_x[length(prev_value_x)-undo_ct]
              }
              ## Advance the undo count
              undo_ct <<- undo_ct + 1
              cat(paste("Undo count:",undo_ct,"\n"))
              cat(paste("X move to:",prev_value_x[length(prev_value_x)-undo_ct],"\n"))
              cat(paste("Y move to:",prev_value_y[length(prev_value_y)-undo_ct],"\n"))
            }
            tkrreplot(img)
          }
          ## Assign function to l-click mouse button
          tkbind(img,"<Button-1>",OnLeftClick)
          ## Assign undo function to r-click mouse button
          tkbind(img,"<Button-3>",OnRightClick)
          tkconfigure(img,cursor="hand2")
          if (exists("tt2")){ tkwait.window(tt2) }
        }  
        
        ## Plot shape around sediment plume area
        on.plume <- function(){
          plotFxn <- function(){
            ## Set RGB palette for digitized points
            rgb.palette <- colorRampPalette(c("blue", "yellow"))
            test.pal <- colorRampPalette(c("#660000","#FF4D4D"))
            ## Turn digitized points into a raster
            pl_pt_mat <- matrix(data=NA,nrow=dim(rast)[1],ncol=dim(rast)[2])
            if (length(Pl.X) > 0){
              if (any(is.na(Pl.X))){
                Pl.X <- Pl.X[-which(is.na(Pl.X))]
                Pl.Y <- Pl.Y[-which(is.na(Pl.Y))]
              }
              for (ridx in 1:length(Pl.X)){
                  pl_pt_mat[Pl.Y[ridx],Pl.X[ridx]] <- 201
              }
            }
            pl_pt_rast <- raster(pl_pt_mat,crs=rast@crs, xmn=rast@extent@xmin, ymn=rast@extent@ymin,
                                 xmx=rast@extent@xmax, ymx=rast@extent@ymax)
            pl_pt_rast <- t(pl_pt_rast)
            ## Plot raster + digitized points
            par(mar=c(0,0,0,0))
            image(rast,main="",axes=FALSE,col=terrain.colors(500))
            par(new=TRUE)
            if (length(Pl.X) > 0){
              image(t(pl_pt_rast),main="",axes=FALSE)
              bkpts <- c(0,50,200,201,250)
              colors <- c("black","yellow","green","red","black")
            }
          }
          require(tcltk2)
          tt2 <- tktoplevel()
          img <- tkrplot(tt2,fun=plotFxn,hscale=2,vscale=2)
          tkpack(img)
          tkgrid(img)
          ## Find dimensions of tk plot image (this is different than the raster dimensions)
          img_wid <- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
          img_hei <- as.numeric(tclvalue(tkwinfo("reqheight",img)))
          ## Find dimensions of raster image
          ras_wid <- dim(rast)[2]
          ras_hei <- dim(rast)[1]
          ## If pixel dimensions are not square (i.e. rectangular), find the dimensions of each one to adjust pixel count
          pix_hei <- res(rast)[2]
          pix_wid <- res(rast)[1]
          #if (pix_wid == pix_hei){
          pix_wid <- pix_hei <- 1
          #}
          ## Find pixel size of tk image based upon raster dimensions. Use this to adjust all input clicks
          w_x_factor <- (img_wid/ras_wid)*pix_wid
          h_y_factor <- (img_hei/ras_hei)*pix_hei
          mvPoint <- function(xClick,yClick){
            ## All x and y-clicks are divided by a fixed factor, since each pixel is stretched to fit a coordinate grid.
            ## Store original value in variable (for undo function)
            if (length(prev_value_plx) == 0){
              prev_value_plx <<- ceiling(as.numeric(xClick)/w_x_factor)
              prev_value_ply <<- ceiling(as.numeric(yClick)/h_y_factor)
              } else {
              prev_value_plx <<- append(prev_value_plx,ceiling(as.numeric(xClick)/w_x_factor))
              prev_value_ply <<- append(prev_value_ply,ceiling(as.numeric(yClick)/h_y_factor))
            }
            if (length(Pl.X) != 0){
              Pl.X <<- append(Pl.X,ceiling(as.numeric(xClick)/w_x_factor))
              Pl.Y <<- append(Pl.Y,ceiling((as.numeric(yClick))/h_y_factor))
            } else {
              Pl.X <<- ceiling(as.numeric(xClick)/w_x_factor)
              Pl.Y <<- ceiling((as.numeric(yClick))/h_y_factor)
            }
            cat("X var:",ceiling((as.numeric(xClick))/w_x_factor),'\n')
            cat("Y var:",ceiling((as.numeric(yClick))/h_y_factor),'\n')
          }
          ## Function to add points
          OnLeftClick <- function(x,y){
            xClick <- x
            yClick <- y
            ## Call the move point function
            mvPoint(xClick,yClick)
            ## Print the new coordinates in the console window
            print(xClick)
            print(yClick)
            ## Set the undo count back to zero
            undo_ct <<- 0
            ## Re-plot data after each click
            tkrreplot(img)
          }
          ## Function to undo last l-click action
          OnRightClick <- function(){
            if (length(prev_value_plx) > 0){
                Pl.X[length(Pl.X)-undo_ct] <<- NA
                Pl.Y[length(Pl.Y)-undo_ct] <<- NA
                ## Advance the undo count
                undo_ct <<- undo_ct + 1
                cat(paste("Undo count:",undo_ct,"\n"))
            }
            tkrreplot(img)
          }
          ## Assign function to l-click mouse button
          tkbind(img,"<Button-1>",OnLeftClick)
          ## Assign undo function to r-click mouse button
          tkbind(img,"<Button-3>",OnRightClick)
          tkconfigure(img,cursor="hand2")
          if (exists("tt2")){ tkwait.window(tt2) }
        }        
        
        ## Plot shape around ice melange area
        on.melange <- function(){
          plotFxn <- function(){
            ## Set RGB palette for digitized points
            rgb.palette <- colorRampPalette(c("blue", "yellow"))
            test.pal <- colorRampPalette(c("#660000","#FF4D4D"))
            ## Turn digitized points into a raster
            me_pt_mat <- matrix(data=NA,nrow=dim(rin)[1],ncol=dim(rin)[2])
            if (length(Me.X) > 0){
              if (any(is.na(Me.X))){
                Me.X <- Me.X[-which(is.na(Me.X))]
                Me.Y <- Me.Y[-which(is.na(Me.Y))]
              }
              for (ridx in 1:length(Me.X)){
                me_pt_mat[Me.Y[ridx],Me.X[ridx]] <- 201
              }
            }
            # Create a plottable raster from the resulting matrix
            me_pt_rast <- raster(me_pt_mat,crs=rin@crs, xmn=rin@extent@xmin, ymn=rin@extent@ymin,
                                 xmx=rin@extent@xmax, ymx=rin@extent@ymax)
            # Transpose the raster
            me_pt_rast <- t(me_pt_rast)            
            ## Plot raster + digitized points
            par(mar=c(0,0,0,0))
            image(rin,main="",axes=FALSE,col=terrain.colors(500))
            par(new=TRUE)
            if (length(Me.X) > 0){
              image(t(me_pt_rast),main="",axes=FALSE)
              bkpts <- c(0,50,200,201,250)
              colors <- c("black","yellow","green","red","black")
            }
          }
          require(tcltk2)
          # Plot up the images on a new tk window
          tt2 <- tktoplevel()
          img <- tkrplot(tt2,fun=plotFxn,hscale=2,vscale=2)
          tkpack(img)
          tkgrid(img)
          ## Find dimensions of tk plot image (this is different than the raster dimensions)
          img_wid <- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
          img_hei <- as.numeric(tclvalue(tkwinfo("reqheight",img)))
          ## Find dimensions of raster image
          ras_wid <- dim(rin)[2]
          ras_hei <- dim(rin)[1]
          ## If pixel dimensions are not square (i.e. rectangular), find the dimensions of each one to adjust pixel count
          pix_hei <- res(rin)[2]
          pix_wid <- res(rin)[1]
          pix_wid <- pix_hei <- 1
          ## Find pixel size of tk image based upon raster dimensions. Use this to adjust all input clicks
          w_x_factor <- (img_wid/ras_wid)*pix_wid
          h_y_factor <- (img_hei/ras_hei)*pix_hei
          mvPoint <- function(xClick,yClick){
            ## All x and y-clicks are divided by a fixed factor, since each pixel is stretched to fit a coordinate grid.
            ## Store original value in variable (for undo function)
            if (length(prev_value_plx) == 0){
              ## Rounding
              prev_value_plx <<- ceiling(as.numeric(xClick)/w_x_factor)
              prev_value_ply <<- ceiling(as.numeric(yClick)/h_y_factor)
            } else {
              ## Rounding
              prev_value_plx <<- append(prev_value_plx,ceiling(as.numeric(xClick)/w_x_factor))
              prev_value_ply <<- append(prev_value_ply,ceiling(as.numeric(yClick)/h_y_factor))
            }
            ## Rounding
            if (length(Me.X) != 0){
              Me.X <<- append(Me.X,ceiling(as.numeric(xClick)/w_x_factor))
              Me.Y <<- append(Me.Y,ceiling((as.numeric(yClick))/h_y_factor))
            } else {
              Me.X <<- ceiling(as.numeric(xClick)/w_x_factor)
              Me.Y <<- ceiling((as.numeric(yClick))/h_y_factor)
            }
            cat("X var:",ceiling((as.numeric(xClick))/w_x_factor),'\n')
            cat("Y var:",ceiling((as.numeric(yClick))/h_y_factor),'\n')
          }
          ## Function to add points
          OnLeftClick <- function(x,y){
            xClick <- x
            yClick <- y
            ## Call the move point function
            mvPoint(xClick,yClick)
            ## Print the new coordinates in the console window
            print(xClick)
            print(yClick)
            ## Set the undo count back to zero
            undo_ct <<- 0
            ## Re-plot data after each click
            tkrreplot(img)
          }
          ## Function to undo last l-click action
          OnRightClick <- function(){
            if (length(prev_value_plx) > 0){
              Me.X[length(Me.X)-undo_ct] <<- NA
              Me.Y[length(Me.Y)-undo_ct] <<- NA
              ## Advance the undo count
              undo_ct <<- undo_ct + 1
              cat(paste("Undo count:",undo_ct,"\n"))
              #cat(paste("X move to:",prev_value_plx[length(prev_value_plx)-undo_ct],"\n"))
              #cat(paste("Y move to:",prev_value_ply[length(prev_value_ply)-undo_ct],"\n"))
            }
            tkrreplot(img)  
          }
          ## Assign function to l-click mouse button
          tkbind(img,"<Button-1>",OnLeftClick)
          ## Assign undo function to r-click mouse button
          tkbind(img,"<Button-3>",OnRightClick)
          tkconfigure(img,cursor="hand2")
          if (exists("tt2")){ tkwait.window(tt2) }
        }
        
        
        ## "Refresh" button: re-load current images
        on.ref <- function(){
          ## Close the tcltk window
          tkdestroy(tt)
        }
        
        
        ## "Save & Next Image" button: skips entire image (everything looks OK)
        on.skipall <- function(){
          ## Close the tcltk window
          tkdestroy(tt)
          ## Leave the loop and start the next iteration on the outer loop
          ## This is not recommended programming practice, but it works!
          bomb <<- 1
        } 
        
        
        ## "DISCARD" button: removes raster from final output plot
        on.discard <- function(){
          ## Close the tcltk window
          tkdestroy(tt)
          if (exists("tt2")){ tkdestroy("tt2")}
          ## Leave the loop using the bomb (see above)
          bomb <<- 2
        }
        
        ## Initialize window
        tt<-tktoplevel()
        
        ## First window
        Name <- tclVar(xs[[idx]][pidx])
        entry.Name <-tkentry(tt,width="20",textvariable=Name)
        ## Set heading for options window
        fn_split <- strsplit(fn_in[idx],split="/")
        fn_split <- strsplit(fn_split[[1]][length(fn_split[[1]])],split="_")
        if (year <= 2008){
          ## Split values that are separated by a period, then select the first part of the split value
          fn_nums <- strsplit(fn_split[[1]],split="\\.")[[1]][1]
        } else {
          fn_num <- as.numeric(fn_split[[1]])
          ## Eliminate any values which are not numeric 
          fn_nums <- fn_num[which(!is.na(fn_num))]
          ## If applicable, eliminate any values that are the year string
          fn_nums <- fn_nums[which(fn_nums != year)]
        }
        tkgrid(tklabel(tt,text=paste("Day ",im_date[idx],sep=" ")))
        tkgrid(tklabel(tt,text=paste("Row",pidx,": Enter new column value:")))
        tkgrid(tklabel(tt,text=paste("Rows with NA values:",paste(length(na_rows)),sep=" ")))
        tkgrid(entry.Name)
        ## Set button names and appearance
        int.but <- tkbutton(tt,text="   TERMINUS Digitization Mode (White to Black)  ",command=on.int)
        int.blu.but <- tkbutton(tt,text="   TERMINUS Digitization Mode (Blue to White)  ",command=on.int.blue)
        int.prev.but <- tkbutton(tt,text=" APPLY Previous Good TERMINUS Position (White to Black)  ",command=on.int.prev)
        plu.but <- tkbutton(tt,text="   Plume Delineation   ",command=on.plume)
        mel.but <- tkbutton(tt,text="   Melange Delineation   ",command=on.melange)
        #back.but <- tkbutton(tt,text="   Back   ",command=on.back)
        ref.but <- tkbutton(tt,text="    Refresh Window  ",command=on.ref)
        skipall.but <- tkbutton(tt,text="   SAVE Image & Continue  ",command=on.skipall)
        discard.but <- tkbutton(tt,text="   **DISCARD Image**   ",command=on.discard)
        ## Set original xs value to a different variable
        xs_old <- xs[[idx]][pidx]
        
        ##### Set buttons #####
        ## List edit button
        #tkbind(entry.Name,on.list)
        #tkgrid(list.but)
        ## Interactive previous button
        tkbind(entry.Name,on.int.prev)
        tkgrid(int.prev.but)
        ## Interactive Menu button (white to black)
        tkbind(entry.Name,on.int)
        tkgrid(int.but)
        ## Interactive Menu button (blue to white)
        tkbind(entry.Name,on.int.blue)
        tkgrid(int.blu.but)
        ## Plume digitizer button
        tkbind(entry.Name,on.plume)
        tkgrid(plu.but)
        ## Melange digitizer button
        tkbind(entry.Name,on.melange)
        tkgrid(mel.but)
        ## Skip button
        tkbind(entry.Name,"<s>",on.ref)
        tkgrid(ref.but)
        ## Skip All button
        tkbind(entry.Name,"<q>",on.skipall)
        tkgrid(skipall.but)
        ## Discard button
        tkbind(entry.Name,on.discard)
        tkgrid(discard.but)
        #tkbind(entry.Name, "<Return>",OnOK)
        tkfocus(tt)
        ## Bring window to front of screen
        tkraise(tt)
        ## Stop the loop until the options window is closed
        tkwait.window(tt)
        ## Break program if bomb was set to 1
        if (bomb == 1) { ## Bomb 1 is SAVE
          true_ct <<- append(true_ct,1)
          break
        } else if (bomb == 2){ ## Bomb 2 is DISCARD
          ## If the 'discard' bomb flag was set, then make all the digitized values NA. 
          xs[[idx]] <- rep(NA,x_start_idx)
          true_ct <<- append(true_ct,0)
          #area_sq_km[idx] <<- NA
          break
        } else if (bomb == 3){ ## Bomb 3 is LEGACY
          next
          bomb3_ct <- bomb3_ct + 1
        }
      }
      
      ##### Define area of glacier #####
      ## If the raster was not discarded...
      if (!all(is.na(xs[[idx]]))){               
      ## Create a dynamic vector for counting NA pixels
        pixel_ct <- rep(NA,length(xs[[idx]]))
        for (nidx in 1:length(xs[[idx]])){
          ## If the current data value isn't already NA...
          if (!is.na(xs[[idx]][nidx])){
            ## ...count up the number of NA values in the given row...
            na_idx <- which(is.na(rast[nidx,]))
            ## If pick exists within an NA range already, set the value to NA.
            if (any(na_idx==xs[[idx]][nidx])){
              pixel_ct[nidx] <- NA
            } else {
            ## ...and count the number of non-NA pixels in that given row (from that column index to the beginning/end of the raster, 
            ## depending on region selected.)
              if (region == "east"){
                pixel_ct[nidx] <- xs[[idx]][nidx] - length(which(na_idx <= xs[[idx]][nidx]))
              } else if (region == "west"){
                pixel_ct[nidx] <- xs[[idx]][nidx] - length(which(na_idx >= xs[[idx]][nidx]))
              } else {
                cat("Incorrect region selected! Area cannot be calculated!\n\n\n\n")
                break
              }
            }
          }
        }
        if (length(pixel_ct) == length(xs[[idx]])){
          temp <- xs[[idx]]
          xs_final[[idx]] <- pixel_ct
        }
        area_sq_km[idx] <- (sum(pixel_ct[!is.na(pixel_ct)])*sp_res*sp_res)/1000000
    }
      ##### Find area of plume and/or melange #####
      if (length(Pl.X) == 0){
        plume_sq_km[idx] <- NA
      } else {
        ## Points must be in clockwise order to create whole polygon for area calculation
        ## Solution found at: http://stackoverflow.com/questions/2855189/sort-latitude-and-longitude-coordinates-into-clockwise-ordered-quadrilateral
        plume_area <- function(Pl.X,Pl.Y){
          # Put input vars into new variables (so they can be manipulated later)
          xx <- Pl.X
          yy <- Pl.Y
          # Find max y value
          ymi <- which.max(yy)
          # Put top point into coordinate pair
          mpair <- c(xx[ymi],yy[ymi])
          # Remove top variable from input coordinates
          xx <- xx[-ymi]
          yy <- yy[-ymi]
          # Remove NA values from xx and yy
          xx <- xx[-which(is.na(xx))]
          yy <- yy[-which(is.na(yy))]
          ## Iterate through the remaining coordinates and add each one in clockwise order to a new variable
          ## Clockwise pairing is ordered by most positive to most negative slope
          slp <- rep(NA,length(xx))
          for (si in 1:length(xx)){
            slp[si] <- (mpair[2] - yy[si])/(mpair[1] - xx[si])
          }
          # Put all the slope values in reverse order (largest to smallest) and add origin point to beginning and end of coordinates
          slp_ord <- order(slp,decreasing=TRUE)
          xx <- c(mpair[1],xx[slp_ord],mpair[1])
          yy <- c(mpair[2],yy[slp_ord],mpair[2])
          # Transform the points into a polygon
          pts <- coordinates(as.data.frame(cbind(xx,yy)))
          spp <- SpatialPolygons(list(Polygons(list(Polygon(pts)),1)),proj4string=rast@crs)
          a_spp <- area(spp)
          outList <- list(a_spp,xx,yy)
          return(outList)
          }
          plume_vars <- plume_area(Pl.X,Pl.Y)
          plume_sq_km[idx] <- unlist(plume_vars[[1]])
          cat("Plume area: ",plume_sq_km[idx]," square kilometers.\n")
          Pl.XX[idx] <- unlist(plume_vars[[2]])
          Pl.YY[idx] <- unlist(plume_vars[[3]])
        }
        ##### Find area of plume and/or melange #####
        if (length(Me.X) == 0){
          melange_sq_km[idx] <- NA
        } else {
          ## Points must be in clockwise order to create whole polygon for area calculation
          ## Solution found at: http://stackoverflow.com/questions/2855189/sort-latitude-and-longitude-coordinates-into-clockwise-ordered-quadrilateral
          plume_area <- function(Pl.X,Pl.Y){
            # Put input vars into new variables (so they can be manipulated later)
            xx <- Pl.X
            yy <- Pl.Y
            # Find max y value
            ymi <- which.max(yy)
            # Put top point into coordinate pair
            mpair <- c(xx[ymi],yy[ymi])
            # Remove top variable from input coordinates
            xx <- xx[-ymi]
            yy <- yy[-ymi]
            # Remove NA values from xx and yy
            if (any(is.na(xx))){
              xx <- xx[-which(is.na(xx))]
              yy <- yy[-which(is.na(yy))]
            }
            ## Iterate through the remaining coordinates and add each one in clockwise order to a new variable
            ## Clockwise pairing is ordered by most positive to most negative slope
            slp <- rep(NA,length(xx))
            for (si in 1:length(xx)){
              slp[si] <- (mpair[2] - yy[si])/(mpair[1] - xx[si])
            }
            # Put all the slope values in reverse order (largest to smallest) and add origin point to beginning and end of coordinates
            slp_ord <- order(slp,decreasing=TRUE)
            xx <- c(mpair[1],xx[slp_ord],mpair[1])
            yy <- c(mpair[2],yy[slp_ord],mpair[2])
            # Transform the points into a polygon
            pts <- coordinates(as.data.frame(cbind(xx,yy)))
            spp <- SpatialPolygons(list(Polygons(list(Polygon(pts)),1)),proj4string=rast@crs)
            a_spp <- area(spp)/10 # Divide by 10 to convert to kilometers
            outList <- list(a_spp,xx,yy)
            return(outList)
          }
          melange_vars <- plume_area(Me.X,Me.Y)
          melange_sq_km[idx] <- unlist(melange_vars[[1]])
          cat("Ice melange area: ",melange_sq_km[idx]," square kilometers.\n")
          Me.XX[idx] <- unlist(melange_vars[[2]])
          Me.YY[idx] <- unlist(melange_vars[[3]])
        }
      }
      
      ##### Plot raster #####
      ## Re-read rasters with all bands; make final plots with PlotRGB
      if (!is.na(area_sq_km[idx])){
        #rin  <- raster(fn_in[idx])
        #rast <- crop(rin,shp,snap="in")
        graphics.off()
	if (ptf == "windows"){
	  windows()
	} else {
          x11()
	}
        ## Check to see if raster conforms to 0-255 RGB colors; if not, re-stretch the values to fit that scheme
        if (max(rast[!is.na(rast)[]]) > 255 | min(rast[!is.na(rast)[]]) < 0){
          #plotRGB(stack(rast,rast,rast),stretch="hist")
	  plot(rast)
        } else if (nlayers(rast) == 1){
          #plotRGB(stack(rast,rast,rast))
	  plot(rast)
        } else if (year <= 2008){ # Images that were created from HDF files
          plot(rast)
	  #plotRGB(rast)
        } else {
	  plot(rast)
          #plotRGB(rast,3,2,1,stretch='hist')
        }   
        
        ## Change any values that are within an NA region to 0, else leave them be. 
        ## If all of the x axis values are already NA, skip this step
        if (all(is.na(xs[[idx]]))){
          }else{
          for (ridx in 1:length(xs[[idx]])){
            ## If the row value is already set to zero...
            if (length(const_rast_mat[ridx,xs[[idx]][ridx]]) > 0){
              ## If the raster value is NA...
              if (is.na(const_rast_mat[ridx,xs[[idx]][ridx]])){
                ## ...then set it to zero
                xs_final[[idx]][ridx] <- 0
              }
            }
          }
        }
          pt_mat[ys[ridx],xs[[idx]][ridx]] <- 200
        }
        pt_rast <- raster(pt_mat,crs=rast@crs, xmn=rast@extent@xmin, ymn=rast@extent@ymin,
                          xmx=rast@extent@xmax, ymx=rast@extent@ymax)
        plot(pt_rast,legend=FALSE,add=TRUE)
        plot(pl_pt_rast,legend=FALSE,add=TRUE)
        ## Create new filename, output image + plot as a PNG
        fn_split <- strsplit(fn_in[idx],split="/")
        fn_split <- strsplit(fn_split[[1]][length(fn_split[[1]])],split="_")
        if (year <= 2008){
          fn_nums <- strsplit(fn_split[[1]],split="\\.")[[1]][1]
        } else {
          fn_num <- as.numeric(fn_split[[1]])
          fn_nums <- fn_num[which(!is.na(fn_num))]
          sort_dates <- sort(fn_nums)
          fn_nums <- sort_dates[length(fn_nums)]
          if (length(fn_nums) > 2){
            fn_nums <- fn_nums[length(fn_nums)]
          }
        }
        png_out <- paste(dir_out,year,im_date[idx],".png",sep="")
        csv_out <- paste(dir_out,year,im_date[idx],".csv",sep="")
        dev.copy(png,png_out,height=960,width=960,res=100)
        graphics.off()
        ## Write a single csv file with the data (helpful if the program crashes)
        single_csv <- cbind(area_sq_km[idx],plume_sq_km[idx],melange_sq_km[idx],xs[[idx]][1:x_start_idx],Pl.XX[idx],Pl.YY[idx])
        write.csv(single_csv,file=csv_out)
        cat("Terminus position image",idx,"of",length(fn_in),"plotted.\n",sep=" ")
      }


  ##### Write new values to a table #####
  yr <- rep(year,length(fn_in))
  doy <- im_date
  ## Set dec_doy according to leap year
  if (any(year == seq(2100,0,-4))){
    dec_doy <- yr + (as.integer(doy)/366)
  } else {
    dec_doy <- yr + (as.integer(doy)/365)
  }
  
  ## Create matrix of all terminus position values
  term_pos <- matrix(NA,length(xs_final),x_start_idx)
  for (xidx in 1:length(xs_final)){
    if (length(xs_final[[xidx]]) == 0){
      xs_final[[xidx]] <- rep(NA,x_start_idx)
    }
    term_pos[xidx,] <- xs_final[[xidx]]
    area_sq_km[xidx] <- (sum(xs_final[[xidx]][!is.na(xs_final[[xidx]])])*sp_res*sp_res)/1000000
    if (area_sq_km[xidx] == 0){
      area_sq_km[xidx] <- NA
    }
  }
  
  ## Create a column that marks whether or not an image was discarded
  image_good <- rep(NA,length(area_sq_km))
  for (i in 1:length(image_good)){
    if (is.na(area_sq_km[i])){
      image_good[i] <- 0
    } else {
      image_good[i] <- 1
    }
  }
  ## Make all area_sq_km values NA where all the of the picks are NA
  #area_sq_km_backup <- area_sq_km
  #area_sq_km[which(is.na(term_pos[,2]))] <- NA
  
  ## Make sure all area measurement variables are the same length as the terminus position values.
  ## Otherwise, there will be write issues with the output CSV.
#   if (length(area_sq_km) < length(term_pos[,1])){
#     ldiff <- abs(length(area_sq_km) - length(term_pos[,1]))
#     area_sq_km <- append(area_sq_km,rep(NA,ldiff))
#   } else if (length(area_sq_km) > length(term_pos[,1])){
#     ldiff <- (abs(length(area_sq_km) - length(term_pos[,1])))-1
#     area_sq_km <- area_sq_km[-((length(area_sq_km)-ldiff):length(area_sq_km))]
#   }
#   
#   if (length(plume_sq_km) < length(term_pos[,1])){
#     ldiff <- abs(length(plume_sq_km) - length(term_pos[,1]))
#     plume_sq_km <- append(plume_sq_km,rep(NA,ldiff))
#   } else if (length(plume_sq_km) > length(term_pos[,1])){
#     ldiff <- (abs(length(plume_sq_km) - length(term_pos[,1])))-1
#     plume_sq_km <- plume_sq_km[-((length(plume_sq_km)-ldiff):length(plume_sq_km))]
#   }
#   if (length(melange_sq_km) < length(term_pos[,1])){
#     ldiff <- abs(length(melange_sq_km) - length(term_pos[,1]))
#     melange_sq_km <- append(melange_sq_km,rep(NA,ldiff))
#   } else if (length(melange_sq_km) > length(term_pos[,1])){
#     ldiff <- (abs(length(melange_sq_km) - length(term_pos[,1])))-1
#     melange_sq_km <- melange_sq_km[-((length(melange_sq_km)-ldiff):length(melange_sq_km))]
#   }
  
  ## Combine data column-wise
  output_table <- cbind(yr,doy,dec_doy,area_sq_km,plume_sq_km,melange_sq_km,image_good)
  
  #term_pos <- matrix(unlist(xs),byrow=TRUE,ncol=x_start_idx)
  colnames(term_pos) <- name_col
  ## Write data to a CSV file
  output_table <- cbind(output_table,term_pos)
  write.csv(output_table,file=paste(dir_out,year,"_","terminus_area.csv",sep=""))


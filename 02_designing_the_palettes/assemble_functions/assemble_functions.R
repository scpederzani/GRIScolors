# list palettes

GrisPalettes <- list(
  "Gris" = list(c("#354B7E", "#6680A3", "#DFE0E1", "#C7A459", "#6E2F35"), c(1:5), colorblind = FALSE), 
  "Denial" = list(c("#DCDADB", "#8D8E93", "#6A676B", "#2E3033", "#707374"), c(1:5), colorblind = TRUE),
  "DesertPath" = list(c("#F1DFD6", "#EB8D8D", "#D14341", "#A81F21", "#4D0E12"), c(1:5), colorblind = TRUE), 
  "Windswept" = list(c("#835D60", "#F2BDB9", "#E2676F", "#A05B53", "#7B3538"), c(1:5), colorblind = FALSE), 
  "GiantTree" = list(c("#86A19A", "#677173", "#4B3938", "#B84753", "#963B43"), c(1:5), colorblind = FALSE), 
  "DoubleJump" = list(c("#773B3C", "#D18587", "#F7F1ED", "#88A9A3", "#79918F"), c(1:5), colorblind = FALSE), 
  "ForestFriend" = list(c("#3D483E", "#D5B3A1", "#AC7372", "#A2374C", "#BA5E4B"), c(1:5), colorblind = FALSE),
  #"WaterDrops" = list(c("#596695", "#0F294C", "#5EA2C5", "#CEF1F9", "#527384"), c(1:5), colorblind = FALSE), 
  "TunnelChase" = list(c("#020406", "#062C53", "#1B7DA1", "#3BAED5", "#54D1E8"), c(1:5), colorblind = FALSE), 
  #"CrystalCave" = list(c("#9BDEF7", "#43B5DF", "#156999", "#053C5A", "#175E73"), c(1:5), colorblind = TRUE), 
  "TurtleLight" = list(c("#38336E", "#7483B8", "#7CACCC", "#793B6C", "#BD90BD"), c(1:5), colorblind = FALSE), 
  "ShiningTree" = list(c("#0C1011", "#3E5F54", "#5F8072", "#e2e288", "#759988"), c(1:5), colorblind = FALSE), 
  "LightGuide" = list(c("#2B294F", "#2E6576", "#1DA4B8", "#118BA6", "#A1E2C5"), c(1:5), colorblind = FALSE),
  "FlowerBridge" = list(c("#CCC6C6", "#8F8EA3", "#7C748C", "#2E1A37", "#D9647A"), c(1:5), colorblind = FALSE), 
  "Starlight" = list(c("#33122A", "#6D3847", "#E4B476", "#879799", "#554D65"), c(1:5), colorblind = FALSE),
  #"StarLight2" = list(c("#554D65", "#879498", "#E4B476", "#652330", "#33122A"), c(1:5), colorblind = FALSE),
  #"Moon1" = list(c("#755275", "#2E1F42", "#378AA9", "#34A8BF", "#F7F5DB"), c(1:5), colorblind = FALSE), 
  "Moonbeam" = list(c("#EED9C8", "#827A9C", "#7D285B", "#360830", "#98B8AE"), c(1:5), colorblind = FALSE), 
  "FowlSong" = list(c("#06324B", "#047183", "#14BABC", "#045F62", "#A95C9A"), c(1:5), colorblind = FALSE),
  #"Fowl2" = list(c("#C44257", "#321D3A", "#f3c87a", "#64768E", "#9092A5"), c(1:5), colorblind = FALSE),
  "CloudPath" = list(c("#5991D2", "#DDD6EE", "#AFAEDA", "#A777AC", "#9752A9"), c(1:5), colorblind = FALSE),
  "BirdFlight" = list(c("#487692", "#74AFCF", "#82D5D0", "#DAAFBA", "#D9647A"), c(1:5), colorblind = FALSE), 
  "Healing" = list(c("#52AE9A", "#77C8B8", "#D6C8BA", "#DB8F50", "#C54652"), c(1:5), colorblind = FALSE)
)


# make palette

grisbrewer <- function(palette_name, n, type = c("discrete", "continuous"), direction = c(1, -1), override.order = FALSE) {
  
  `%notin%` <- Negate(`%in%`)
  
  palette <- GrisPalettes[[palette_name]]
  
  if (is.null(palette)|is.numeric(palette_name)){
    stop("Palette does not exist.")
  }
  
  if (missing(n)) {
    n <- length(palette[[1]])
  }
  
  if (missing(direction)) {
    direction <- 1
  }
  
  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }
  
  if (missing(type)) {
    if(n > length(palette[[1]])){type <- "continuous"}
    else{type <- "discrete"}
  }
  
  type <- match.arg(type)
  
  
  if (type == "discrete" && n > length(palette[[1]])) {
    stop("Number of requested colors greater than what discrete palette can offer, \n use continuous instead.")
  }
  
  continuous <-  if(direction==1){grDevices::colorRampPalette(palette[[1]])(n)
  }else{
    grDevices::colorRampPalette(rev(palette[[1]]))(n)}
  
  discrete <- if(direction==1 & override.order==FALSE){
    palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
  }else if(direction==-1 & override.order==FALSE){
    rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
  } else if(direction==1 & override.order==TRUE){
    palette[[1]][1:n]
  } else{
    rev(palette[[1]])[1:n]
  }
  
  out <- switch(type,
                continuous = continuous,
                discrete = discrete
  )
  structure(out, class = "palette", name = palette_name)
  
}






print_palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.92, n + 1, 1.08, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 2.5, family = "serif")
}


display_all <- function(n, sequential = FALSE, colorblind_only = FALSE, direction = 1, override.order=FALSE){
  if(colorblind_only){
    N = length(colorblind_palettes)
    pal_names = colorblind_palettes
  }else{
    N = length(GrisPalettes)
    pal_names = names(GrisPalettes)
  }
  
  orig_pars <- par()
  
  plot_palette = function(name,n){
    par(mar = c(0.1,0.1,1,0.1))
    nn = ifelse(missing(n), length(grisbrewer(name)), n)
    plot(0,type='n',bty='n',xaxt='n',yaxt='n',xlab='',ylab='',
         ylim = c(0,1),xlim=c(0,nn), main = name)
    for(j in 1:nn){
      polygon(x = c(j-1,j-1,j,j),
              y = c(0,1,1,0),
              border = NA,
              col = grisbrewer(name, nn, direction= direction,override.order=override.order)[j])
    }
  }
  
  if(sequential){
    for(i in 1:N){
      
      if(missing(n)){
        
        plot_palette(pal_names[i])
        if(i < N) cat("Hit 'Enter' for next palette");readline()
        
      }else{
        
        plot_palette(pal_names[i],n)
        if(i < N) cat("Hit 'Enter' for next palette");readline()
      }
    }
  }else{
    
    if(missing(n)){
      
      if(colorblind_only){
        
        layout(matrix(1:N,N/2,2))
        for(i in 1:N) plot_palette(pal_names[i],n)
        
      }else{
        
        layout(matrix(1:N,N/2,2))
        for(i in 1:N) plot_palette(pal_names[i],n)
      }
      
    } else{
      
      if(colorblind_only){
        
        layout(matrix(1:N,N/2,2))
        for(i in 1:N) plot_palette(pal_names[i],n)
        
      }else{
        
        layout(matrix(1:N,N/2,2))
        for(i in 1:N) plot_palette(pal_names[i],n)
        
      }
      
    }
    
    layout(matrix(1,1,1))
    par(mar = orig_pars$mar)
    
  }
}

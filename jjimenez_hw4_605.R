library(imager)

#get the list of image files in the 'jpg' folder

shoe_paths = list.files(path = "./jpg", pattern = "\\.(jpg|jpeg)$", full.names = TRUE)

#read images into list
shoes_list = lapply(shoe_paths, function(file) load.image(file))

#check dims of the first image to set up the data matrix

first_shoe = shoes_list[[1]]

#remove extra dimension

first_shoe = drop(first_shoe)  
dims = dim(first_shoe)
n_rows = dims[1] * dims[2] * dims[3]

# make empty matrix to store flattened images

shoe_data = matrix(nrow = n_rows, ncol = length(shoes_list))

# flatten each image and store it as a column in the matrix

for (i in seq_along(shoes_list)) {
  img = shoes_list[[i]]
  img = drop(img)  
  img_dims = dim(img)
  
  # Check if the image has the same dimensions as the first image
  
  if (all(img_dims == dims)) {
    shoe_data[, i] = as.vector(img)
  } else {
    warning(paste("Skipping image at index", i, "due to different dimensions."))
  }
}

# mean center the data

mean_shoes = rowMeans(shoe_data, na.rm = TRUE)
centered_shoe_data = sweep(shoe_data, 1, mean_shoes)


# perform pca

pca_result = prcomp(t(centered_shoe_data), center = FALSE)

# calc cumulative proportion of variance explained

variance_explained = cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)

# find # of components accounting for 80% of variance

num_components = which.max(variance_explained >= 0.8)

# get top eigenvectors (eigenshoes)
eigenimages = pca_result$rotation[, 1:num_components]



# creat a data frame for plotting num of components vs variance

plot_data = data.frame(
  Num_Components = 1:length(variance_explained),
  Variance_Explained = variance_explained
)

#plot
library(ggplot2)

ggplot(plot_data, aes(x = Num_Components, y = Variance_Explained)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  geom_vline(xintercept = num_components, linetype = "dashed", color = "red") +
  labs(title = "Cumulative Variance Explained by Principal Components",
       x = "Number of Components",
       y = "Cumulative Variance Explained") +
  theme_minimal()




# reshape and visualize first 7 eigenshoes

for (i in 1:min(7, num_components)) {
  eigenimage_vector = eigenimages[, i]
  
  #reshape back to the original dims
  
  eigenimage_array = array(eigenimage_vector, dim = dims)
  
  # make an imager object from the array
  
  eigenimage_im = as.cimg(eigenimage_array)
  
  # Visualize the eigenimage
  
  plot(eigenimage_im)
}


#Create morris test design
#factors is the number of varying parameters (25) in the model
#binf is a vector of lower bounds for input parameters
#bsuf is a vector of upper bounds for input parameters
#r, levels and grid.jump are morris test options
Morris <- morris(model = NULL, factors=25, r=4,
                 design = list(type = "oat", levels = 5, grid.jump = 3),
                 binf = c(0.201, 3.4, 8.585, 55.25, 5.185, 5.015, 11.9, 11.9, 21.25,
                          12.07, 12.75, 38.93, 27880, 0.646, 0.315, 0.68, 0.646, 0.646,
                          0.111, 0.68, 0.646, 5.355, 1284.35, 0.004, 3.4),
                 bsup = c(0.273, 4.6, 11.615, 74.75, 7.015, 6.785, 16.1, 16.1, 28.75,
                          16.33, 17.25, 52.67, 37720, 0.874, 0.426, 0.92, 0.874, 0.874,
                          0.15, 0.92, 0.874, 7.245, 1737.65, 0.005, 4.6),
                 scale = TRUE)
design <- Morris$X
#design is matrix of design points with 104 rows and 25 columns
#obtain vector of output y from running the model.
tell(Morris, y)
mu.star <- apply(Morris$ee, 2, function(Morris) mean(abs(Morris)))
sigma <- apply(Morris$ee, 2, sd)
plot(Morris)

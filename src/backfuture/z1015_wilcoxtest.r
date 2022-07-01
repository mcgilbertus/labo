#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


corrida <- list()

corrida$exp1  <- c( 20555000, 19775000, 20755000, 18950000, 19615000,
                    20245000, 19790000, 19235000, 21010000, 20185000,
                    19420000, 20310000, 18905000, 20220000, 20870000 )

corrida$exp2  <- c( 21950000, 20285000, 20840000, 21050000, 21680000,
                    20870000, 21510000, 20785000, 20905000, 21435000,
                    20585000, 21320000, 21480000, 21125000, 21440000 )



#Sobre el experimento 1 y el experimento 2
wilcox.test(  corrida$exp1[ 1:1], corrida$exp2[ 1:1])
wilcox.test(  corrida$exp1[ 1:2], corrida$exp2[ 1:2])
wilcox.test(  corrida$exp1[ 1:3], corrida$exp2[ 1:3])
wilcox.test(  corrida$exp1[ 1:4], corrida$exp2[ 1:4])
wilcox.test(  corrida$exp1[ 1:5], corrida$exp2[ 1:5])
wilcox.test(  corrida$exp1[ 1:6], corrida$exp2[ 1:6])
wilcox.test(  corrida$exp1[ 1:7], corrida$exp2[ 1:7])
wilcox.test(  corrida$exp1[ 1:8], corrida$exp2[ 1:8])
wilcox.test(  corrida$exp1[ 1:9], corrida$exp2[ 1:9])


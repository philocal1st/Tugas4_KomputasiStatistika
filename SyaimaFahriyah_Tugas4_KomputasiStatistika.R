#1. Sebaran Poisson
# X = jumlah pelanggan yang datang dalam 1 jam
# X ~ Poisson(lambda = 3)
lambda <- 3

# P(X >= 5) = 1 - P(X <= 4)
p_ge_5 <- 1 - ppois(4, lambda)
p_ge_5

# PMF Poisson
x <- 0:12
pmf <- dpois(x, lambda)
plot(x, pmf, type = "h", lwd = 3,
     main = "Poisson (lambda = 3)",
     xlab = "Jumlah pelanggan",
     ylab = "P(X = x)")

#Sebarann Hipergeometrik
# X = jumlah bola merah yang terambil
# X ~ Hypergeometric(N=100, K=20, n=10)
N <- 100    # ukuran populasi
K <- 20     # jumlah bola merah
n <- 10     # ukuran sampel

#Domain X
x <- seq(from = max(0, n + K - N),
         to = min(n, K))
#PMF
pmf <- dhyper(x, m = K, n = N - K, k = n)
data.frame(x = x, P = pmf)

#Plot PMF
plot(x, pmf, type = "h", lwd = 3,
     main = "Hypergeometric (N=100, K=20, n=10)",
     xlab = "Jumlah bola merah",
     ylab = "P(X = x)")

#Simulasi sampling tanpa pengembalian
m <- 10000
samp <- rhyper(m,
               m = K,
               n = N - K,
               k = n)

mean(samp)

#Ekspektasi teoritis
n * K / N

#Distribusi Binomial
n <- 15
p <- 0.4
m <- 1000

#Simulasi 1000 percobaan Binomial
set.seed(2025)
samp <- rbinom(m, size = n, prob = p)
head(samp, n = 10)

#Histogram simulasi
hist(samp,
     breaks = seq(-0.5, n + 0.5, by = 1),
     probability = TRUE,
     main = "Simulasi Binomial(n=15, p=0.4)",
     xlab = "Jumlah sukses",
     ylab = "Probabilitas")

#PMF teoretis
x <- 0:n
pmf <- dbinom(x, size = n, prob = p)

data.frame(x = x, P = pmf)

#PMF Binomial
plot(x, pmf,
     type = "h",
     lwd = 3,
     main = "PMF Binomial(n=15, p=0.4)",
     xlab = "Jumlah sukses",
     ylab = "P(X=x)")

# Histogram simulasi
hist(samp,
     breaks = seq(-0.5, n + 0.5, by = 1),
     probability = TRUE,
     main = "Perbandingan Simulasi dan PMF Binomial",
     xlab = "Jumlah sukses",
     ylab = "Probabilitas")

#PMF teoretis
points(x, pmf, type = "h", lwd = 3)
points(x, pmf, pch = 16)

#Rata-rata hasil simulasi
mean(samp)

#Ekspektasi teoritis
n * p


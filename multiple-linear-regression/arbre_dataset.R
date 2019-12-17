
A = load('arbre.Rdata')
dim(arbre)
head(arbre)

arbre$hauteur # Response (numeric)
arbre$hetraie # Explanatory variable (factors)

attach(arbre)
boxplot(hauteur~hetraie)

# assuming all ok on the noise
L = lm(hauteur~hetraie,data=arbre)
anova(L)
# pvalue small --> all mu_i not equal --> there is an influence

summary(L)

L2 = lm(hauteur~C(hetraie,sum),data=arbre)
summary(L2)

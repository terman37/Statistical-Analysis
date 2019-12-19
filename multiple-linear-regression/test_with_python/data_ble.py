# Imports
import pandas as pd
import pingouin as pg

# Importing file
Ble = pd.read_csv('../Dataset_Ble.txt', header=0, sep=';', index_col=0)
# Changing to Category datatype for explanatory variables
Ble['variete'] = Ble['variete'].astype('category')
Ble['phyto'] = Ble['phyto'].astype('category')

# Analysis of rdt vs variete
Yname = 'rdt'
Xname = 'variete'
print('\n******** %s vs %s **********' % (Yname, Xname))
#
# Homoscedasticity
Homo = pg.homoscedasticity(data=Ble, dv=Yname, group=Xname, method="levene")
print(Homo)
# Normality
Norm = pg.normality(data=Ble, dv=Yname, group=Xname, method="shapiro")
print(Norm)
# Normality of residuals
lm = pg.linear_regression(Ble[Xname].cat.codes, Ble[Yname])
Normall = pg.normality(lm.residuals_)
print(Normall)

# OneWay Anova
aov = Ble.anova(dv=Yname, between=Xname, detailed=True)
print(aov)

# Analysis of rdt vs variete
Yname = 'rdt'
Xname = 'phyto'
print('\n******** %s vs %s **********' % (Yname, Xname))
#
# Homoscedasticity
Homo = pg.homoscedasticity(data=Ble, dv=Yname, group=Xname)
print(Homo)
# Normality
Norm = pg.normality(data=Ble, dv=Yname, group=Xname)
print(Norm)
# Normality of residuals
lm = pg.linear_regression(Ble[Xname].cat.codes, Ble[Yname])
Normall = pg.normality(lm.residuals_)
print(Normall)

# OneWay Anova
aov = Ble.anova(dv=Yname, between=Xname, detailed=True)
print(aov)

# Analysis of rdt vs variete
Yname = 'rdt'
Xname = ['variete', 'phyto']
print('\n******** %s vs %s **********' % (Yname, Xname))
#
# Homoscedasticity
# Homo = pg.homoscedasticity(data=Ble,dv=Yname,group=Xname)
# print(Homo)
# # Normality
# Norm = pg.normality(data=Ble,dv=Yname,group=Xname)
# print(Norm)
Ble2 = Ble.copy()
Ble2['variete'] = Ble2['variete'].cat.codes
Ble2['phyto'] = Ble2['phyto'].cat.codes

# Normality of residuals
lm = pg.linear_regression(Ble2[['variete', 'phyto']], Ble2[Yname])
Normall = pg.normality(lm.residuals_)
print(Normall)

# Two Way Anova
aov = Ble.anova(dv=Yname, between=Xname, detailed=True)
print(aov)

# # Basic dataset info
# print("\nShape of data: ")
# print(Ble.shape)
# print("\nHead(): ")
# print(Ble.head(5))
#
# Ble.boxplot(column=['rdt'], by='variete')
# # plt.show()
#
# model = ols('rdt ~ variete', data=Ble).fit()
#
# # Shapiro Wilk Test : check the normal distribution of residuals
# w, pvalue = stats.shapiro(model.resid)
# print("\nShapiro p-value: %.3f" % pvalue)
# if pvalue < 0.05:
#     print("--> Normality not OK !!!")
# else:
#     print("--> Normality OK")
#
# # Levene Test: check the Homogeneity of variances
# w, pvalue = stats.levene(Ble['rdt'][Ble['variete'] == "V1"], Ble['rdt'][Ble['variete'] == "V2"],
#                          Ble['rdt'][Ble['variete'] == "V3"], Ble['rdt'][Ble['variete'] == "V4"])
#
# print("\nLevene p-value: %.3f" % pvalue)
# if pvalue < 0.05:
#     print("--> Homoscedasticity not OK !!!")
# else:
#     print("--> Homoscedasticity OK")
#
#
#
# aov_table = sm.stats.anova_lm(model, typ=2)
# print("\nAnova Table:")
# print(aov_table)
# pvalue = aov_table.loc[['variete'],['PR(>F)']].values[0][0]
# # print(type(aov_table))
#
# if pvalue < 0.05:
#     print("--> There is an influence")
# else:
#     print("--> There is no influence")

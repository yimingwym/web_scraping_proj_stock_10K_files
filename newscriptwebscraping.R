setwd('/Users/Yimimac/Downloads/webscraping_proj/Transportations')
list_tp= dir('/Users/Yimimac/Downloads/webscraping_proj/Transportations/')
list_tp = grep('.csv', list_tp, value=TRUE)
list_tp = Filter(function(x) !any(grepl("company", x)), list_tp)
list_tp = sapply(list_tp, function(x) gsub('.csv', '', x))
list_tp = unname(list_tp)

result_tp = data.frame(list_tp, list(rep(0, length(list_tp))))
colnames(result_tp) = c('symbols', 'annual_return')

for (i in c(1: length(list_tp))) {
  temp = read.csv(paste0(list_tp[i], '.csv'))
  result_tp[i, 2] <- (temp[nrow(temp), 5] / temp[1, 2] - 1)
}

result = result_basic_ind
result = rbind(result, result_cnd)
result = rbind(result, result_conser)
result = rbind(result, result_cpl_goods)
result = rbind(result, result_en)
result = rbind(result, result_fn)
result = rbind(result, result_hc)
result = rbind(result, result_mc)
result = rbind(result, result_pu)
result = rbind(result, result_te)
result = rbind(result, result_tp)

roa_BI <- roa %>% filter(sector == 'Basic_industries')
roa_CG <- roa %>% filter(sector == 'Capital_goods')
roa_CND <- roa %>% filter(sector == 'Consumer_non_durables')
roa_CS <- roa %>% filter(sector == 'Consumer_services')
roa_E <- roa %>% filter(sector == 'Energy')
roa_F <- roa %>% filter(sector == 'Finance')
roa_H <- roa %>% filter(sector == 'Healthcare')
roa_M <- roa %>% filter(sector == 'Miscellaneous')
roa_PU <- roa %>% filter(sector == 'Public_utilities')
roa_TE <- roa %>% filter(sector == 'Technology')
roa_TR <- roa %>% filter(sector == 'Transportation')

cr_BI <- cr %>% filter(sector == 'Basic_industries')
cr_CG <- cr %>% filter(sector == 'Capital_goods')
cr_CND <- cr %>% filter(sector == 'Consumer_non_durables')
cr_CS <- cr %>% filter(sector == 'Consumer_services')
cr_E <- cr %>% filter(sector == 'Energy')
cr_F <- cr %>% filter(sector == 'Finance')
cr_H <- cr %>% filter(sector == 'Healthcare')
cr_M <- cr %>% filter(sector == 'Miscellaneous')
cr_PU <- cr %>% filter(sector == 'Public_utilities')
cr_TE <- cr %>% filter(sector == 'Technology')
cr_TR <- cr %>% filter(sector == 'Transportation')

stock_return_BI <- result_stock_return_less %>% filter(sector == 'Basic_industries')
stock_return_CG <- result_stock_return_less %>% filter(sector == 'Capital_goods')
stock_return_CND <- result_stock_return_less %>% filter(sector == 'Consumer_non_durables')
stock_return_CS <- result_stock_return_less %>% filter(sector == 'Consumer_services')
stock_return_E <- result_stock_return_less %>% filter(sector == 'Energy')
stock_return_F <- result_stock_return_less %>% filter(sector == 'Finance')
stock_return_H <- result_stock_return_less %>% filter(sector == 'Healthcare')
stock_return_M <- result_stock_return_less %>% filter(sector == 'Miscellaneous')
stock_return_PU <- result_stock_return_less %>% filter(sector == 'Public_utilities')
stock_return_TE <- result_stock_return_less %>% filter(sector == 'Technology')
stock_return_TR <- result_stock_return_less %>% filter(sector == 'Transportation')

roe_BI_shift <- roe_BI
roe_BI_shift$annual_return <- roe_BI_shift$annual_return + 1.1
roe_bi_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_BI_shift)
roe_bi_cox = boxCox(roe_bi_shift_mdl)
roe_bi_lambda = roe_bi_cox$x[which(roe_bi_cox$y == max(roe_bi_cox$y))]
roe_bi_annual_return = (roe_BI_shift$annual_return^roe_bi_lambda - 1)/roe_bi_lambda 
roe_bi_shift_model = lm(roe_bi_annual_return ~ roe_BI_shift$return_on_equity) 
summary(roe_bi_shift_model) 


roe_bi_mdl <- lm(annual_return ~ return_on_equity, data = roe_BI)
roe_cg_mdl <- lm(annual_return ~ return_on_equity, data = roe_CG)
roe_cnd_mdl <- lm(annual_return ~ return_on_equity, data = roe_CND)
roe_cs_mdl <- lm(annual_return ~ return_on_equity, data = roe_CS)
roe_e_mdl <- lm(annual_return ~ return_on_equity, data = roe_E)
roe_f_mdl <- lm(annual_return ~ return_on_equity, data = roe_F)
roe_h_mdl <- lm(annual_return ~ return_on_equity, data = roe_H)
roe_m_mdl <- lm(annual_return ~ return_on_equity, data = roe_M)
roe_pu_mdl <- lm(annual_return ~ return_on_equity, data = roe_PU)
roe_te_mdl <- lm(annual_return ~ return_on_equity, data = roe_TE)
roe_tr_mdl <- lm(annual_return ~ return_on_equity, data = roe_TR)

roa_bi_mdl <- lm(annual_return ~ return_on_assets, data = roa_BI)
roa_cg_mdl <- lm(annual_return ~ return_on_assets, data = roa_CG)
roa_cnd_mdl <- lm(annual_return ~ return_on_assets, data = roa_CND)
roa_cs_mdl <- lm(annual_return ~ return_on_assets, data = roa_CS)
roa_e_mdl <- lm(annual_return ~ return_on_assets, data = roa_E)
roa_f_mdl <- lm(annual_return ~ return_on_assets, data = roa_F)
roa_h_mdl <- lm(annual_return ~ return_on_assets, data = roa_H)
roa_m_mdl <- lm(annual_return ~ return_on_assets, data = roa_M)
roa_pu_mdl <- lm(annual_return ~ return_on_assets, data = roa_PU)
roa_te_mdl <- lm(annual_return ~ return_on_assets, data = roa_TE)
roa_tr_mdl <- lm(annual_return ~ return_on_assets, data = roa_TR)

cr_bi_mdl <- lm(annual_return ~ current_ratio, data = cr_BI)
cr_cg_mdl <- lm(annual_return ~ current_ratio, data = cr_CG)
cr_cnd_mdl <- lm(annual_return ~ current_ratio, data = cr_CND)
cr_cs_mdl <- lm(annual_return ~ current_ratio, data = cr_CS)
cr_e_mdl <- lm(annual_return ~ current_ratio, data = cr_E)
cr_f_mdl <- lm(annual_return ~ current_ratio, data = cr_F)
cr_h_mdl <- lm(annual_return ~ current_ratio, data = cr_H)
cr_m_mdl <- lm(annual_return ~ current_ratio, data = cr_M)
cr_pu_mdl <- lm(annual_return ~ current_ratio, data = cr_PU)
cr_te_mdl <- lm(annual_return ~ current_ratio, data = cr_TE)
cr_tr_mdl <- lm(annual_return ~ current_ratio, data = cr_TR)

roe_final[1,2] <- summary(roe_bi_mdl)$coefficients[,1][[1]]
roe_final[1,3] <- summary(roe_bi_mdl)$coefficients[,1][[2]]
roe_final[1,4] <- summary(roe_bi_mdl)$coefficients[,4][[1]]
roe_final[1,5] <- summary(roe_bi_mdl)$coefficients[,4][[2]]

roe_final[2,2] <- summary(roe_cg_mdl)$coefficients[,1][[1]]
roe_final[2,3] <- summary(roe_cg_mdl)$coefficients[,1][[2]]
roe_final[2,4] <- summary(roe_cg_mdl)$coefficients[,4][[1]]
roe_final[2,5] <- summary(roe_cg_mdl)$coefficients[,4][[2]]

roe_final[3,2] <- summary(roe_cnd_mdl)$coefficients[,1][[1]]
roe_final[3,3] <- summary(roe_cnd_mdl)$coefficients[,1][[2]]
roe_final[3,4] <- summary(roe_cnd_mdl)$coefficients[,4][[1]]
roe_final[3,5] <- summary(roe_cnd_mdl)$coefficients[,4][[2]]

roe_final[4,2] <- summary(roe_cs_mdl)$coefficients[,1][[1]]
roe_final[4,3] <- summary(roe_cs_mdl)$coefficients[,1][[2]]
roe_final[4,4] <- summary(roe_cs_mdl)$coefficients[,4][[1]]
roe_final[4,5] <- summary(roe_cs_mdl)$coefficients[,4][[2]]

roe_final[5,2] <- summary(roe_e_mdl)$coefficients[,1][[1]]
roe_final[5,3] <- summary(roe_e_mdl)$coefficients[,1][[2]]
roe_final[5,4] <- summary(roe_e_mdl)$coefficients[,4][[1]]
roe_final[5,5] <- summary(roe_e_mdl)$coefficients[,4][[2]]

roe_final[6,2] <- summary(roe_f_mdl)$coefficients[,1][[1]]
roe_final[6,3] <- summary(roe_f_mdl)$coefficients[,1][[2]]
roe_final[6,4] <- summary(roe_f_mdl)$coefficients[,4][[1]]
roe_final[6,5] <- summary(roe_f_mdl)$coefficients[,4][[2]]

roe_final[7,2] <- summary(roe_h_mdl)$coefficients[,1][[1]]
roe_final[7,3] <- summary(roe_h_mdl)$coefficients[,1][[2]]
roe_final[7,4] <- summary(roe_h_mdl)$coefficients[,4][[1]]
roe_final[7,5] <- summary(roe_h_mdl)$coefficients[,4][[2]]

roe_final[8,2] <- summary(roe_m_mdl)$coefficients[,1][[1]]
roe_final[8,3] <- summary(roe_m_mdl)$coefficients[,1][[2]]
roe_final[8,4] <- summary(roe_m_mdl)$coefficients[,4][[1]]
roe_final[8,5] <- summary(roe_m_mdl)$coefficients[,4][[2]]

roe_final[9,2] <- summary(roe_pu_mdl)$coefficients[,1][[1]]
roe_final[9,3] <- summary(roe_pu_mdl)$coefficients[,1][[2]]
roe_final[9,4] <- summary(roe_pu_mdl)$coefficients[,4][[1]]
roe_final[9,5] <- summary(roe_pu_mdl)$coefficients[,4][[2]]

roe_final[10,2] <- summary(roe_te_mdl)$coefficients[,1][[1]]
roe_final[10,3] <- summary(roe_te_mdl)$coefficients[,1][[2]]
roe_final[10,4] <- summary(roe_te_mdl)$coefficients[,4][[1]]
roe_final[10,5] <- summary(roe_te_mdl)$coefficients[,4][[2]]

roe_final[11,2] <- summary(roe_tr_mdl)$coefficients[,1][[1]]
roe_final[11,3] <- summary(roe_tr_mdl)$coefficients[,1][[2]]
roe_final[11,4] <- summary(roe_tr_mdl)$coefficients[,4][[1]]
roe_final[11,5] <- summary(roe_tr_mdl)$coefficients[,4][[2]]


roa_final[1,2] <- summary(roa_bi_mdl)$coefficients[,1][[1]]
roa_final[1,3] <- summary(roa_bi_mdl)$coefficients[,1][[2]]
roa_final[1,4] <- summary(roa_bi_mdl)$coefficients[,4][[1]]
roa_final[1,5] <- summary(roa_bi_mdl)$coefficients[,4][[2]]

roa_final[2,2] <- summary(roa_cg_mdl)$coefficients[,1][[1]]
roa_final[2,3] <- summary(roa_cg_mdl)$coefficients[,1][[2]]
roa_final[2,4] <- summary(roa_cg_mdl)$coefficients[,4][[1]]
roa_final[2,5] <- summary(roa_cg_mdl)$coefficients[,4][[2]]

roa_final[3,2] <- summary(roa_cnd_mdl)$coefficients[,1][[1]]
roa_final[3,3] <- summary(roa_cnd_mdl)$coefficients[,1][[2]]
roa_final[3,4] <- summary(roa_cnd_mdl)$coefficients[,4][[1]]
roa_final[3,5] <- summary(roa_cnd_mdl)$coefficients[,4][[2]]

roa_final[4,2] <- summary(roa_cs_mdl)$coefficients[,1][[1]]
roa_final[4,3] <- summary(roa_cs_mdl)$coefficients[,1][[2]]
roa_final[4,4] <- summary(roa_cs_mdl)$coefficients[,4][[1]]
roa_final[4,5] <- summary(roa_cs_mdl)$coefficients[,4][[2]]

roa_final[5,2] <- summary(roa_e_mdl)$coefficients[,1][[1]]
roa_final[5,3] <- summary(roa_e_mdl)$coefficients[,1][[2]]
roa_final[5,4] <- summary(roa_e_mdl)$coefficients[,4][[1]]
roa_final[5,5] <- summary(roa_e_mdl)$coefficients[,4][[2]]

roa_final[6,2] <- summary(roa_f_mdl)$coefficients[,1][[1]]
roa_final[6,3] <- summary(roa_f_mdl)$coefficients[,1][[2]]
roa_final[6,4] <- summary(roa_f_mdl)$coefficients[,4][[1]]
roa_final[6,5] <- summary(roa_f_mdl)$coefficients[,4][[2]]

roa_final[7,2] <- summary(roa_h_mdl)$coefficients[,1][[1]]
roa_final[7,3] <- summary(roa_h_mdl)$coefficients[,1][[2]]
roa_final[7,4] <- summary(roa_h_mdl)$coefficients[,4][[1]]
roa_final[7,5] <- summary(roa_h_mdl)$coefficients[,4][[2]]

roa_final[8,2] <- summary(roa_m_mdl)$coefficients[,1][[1]]
roa_final[8,3] <- summary(roa_m_mdl)$coefficients[,1][[2]]
roa_final[8,4] <- summary(roa_m_mdl)$coefficients[,4][[1]]
roa_final[8,5] <- summary(roa_m_mdl)$coefficients[,4][[2]]

roa_final[9,2] <- summary(roa_pu_mdl)$coefficients[,1][[1]]
roa_final[9,3] <- summary(roa_pu_mdl)$coefficients[,1][[2]]
roa_final[9,4] <- summary(roa_pu_mdl)$coefficients[,4][[1]]
roa_final[9,5] <- summary(roa_pu_mdl)$coefficients[,4][[2]]

roa_final[10,2] <- summary(roa_te_mdl)$coefficients[,1][[1]]
roa_final[10,3] <- summary(roa_te_mdl)$coefficients[,1][[2]]
roa_final[10,4] <- summary(roa_te_mdl)$coefficients[,4][[1]]
roa_final[10,5] <- summary(roa_te_mdl)$coefficients[,4][[2]]

roa_final[11,2] <- summary(roa_tr_mdl)$coefficients[,1][[1]]
roa_final[11,3] <- summary(roa_tr_mdl)$coefficients[,1][[2]]
roa_final[11,4] <- summary(roa_tr_mdl)$coefficients[,4][[1]]
roa_final[11,5] <- summary(roa_tr_mdl)$coefficients[,4][[2]]

cr_final[1,2] <- summary(cr_bi_mdl)$coefficients[,1][[1]]
cr_final[1,3] <- summary(cr_bi_mdl)$coefficients[,1][[2]]
cr_final[1,4] <- summary(cr_bi_mdl)$coefficients[,4][[1]]
cr_final[1,5] <- summary(cr_bi_mdl)$coefficients[,4][[2]]

cr_final[2,2] <- summary(cr_cg_mdl)$coefficients[,1][[1]]
cr_final[2,3] <- summary(cr_cg_mdl)$coefficients[,1][[2]]
cr_final[2,4] <- summary(cr_cg_mdl)$coefficients[,4][[1]]
cr_final[2,5] <- summary(cr_cg_mdl)$coefficients[,4][[2]]

cr_final[3,2] <- summary(cr_cnd_mdl)$coefficients[,1][[1]]
cr_final[3,3] <- summary(cr_cnd_mdl)$coefficients[,1][[2]]
cr_final[3,4] <- summary(cr_cnd_mdl)$coefficients[,4][[1]]
cr_final[3,5] <- summary(cr_cnd_mdl)$coefficients[,4][[2]]

cr_final[4,2] <- summary(cr_cs_mdl)$coefficients[,1][[1]]
cr_final[4,3] <- summary(cr_cs_mdl)$coefficients[,1][[2]]
cr_final[4,4] <- summary(cr_cs_mdl)$coefficients[,4][[1]]
cr_final[4,5] <- summary(cr_cs_mdl)$coefficients[,4][[2]]

cr_final[5,2] <- summary(cr_e_mdl)$coefficients[,1][[1]]
cr_final[5,3] <- summary(cr_e_mdl)$coefficients[,1][[2]]
cr_final[5,4] <- summary(cr_e_mdl)$coefficients[,4][[1]]
cr_final[5,5] <- summary(cr_e_mdl)$coefficients[,4][[2]]

cr_final[6,2] <- summary(cr_f_mdl)$coefficients[,1][[1]]
cr_final[6,3] <- summary(cr_f_mdl)$coefficients[,1][[2]]
cr_final[6,4] <- summary(cr_f_mdl)$coefficients[,4][[1]]
cr_final[6,5] <- summary(cr_f_mdl)$coefficients[,4][[2]]

cr_final[7,2] <- summary(cr_h_mdl)$coefficients[,1][[1]]
cr_final[7,3] <- summary(cr_h_mdl)$coefficients[,1][[2]]
cr_final[7,4] <- summary(cr_h_mdl)$coefficients[,4][[1]]
cr_final[7,5] <- summary(cr_h_mdl)$coefficients[,4][[2]]

cr_final[8,2] <- summary(cr_m_mdl)$coefficients[,1][[1]]
cr_final[8,3] <- summary(cr_m_mdl)$coefficients[,1][[2]]
cr_final[8,4] <- summary(cr_m_mdl)$coefficients[,4][[1]]
cr_final[8,5] <- summary(cr_m_mdl)$coefficients[,4][[2]]

cr_final[9,2] <- summary(cr_pu_mdl)$coefficients[,1][[1]]
cr_final[9,3] <- summary(cr_pu_mdl)$coefficients[,1][[2]]
cr_final[9,4] <- summary(cr_pu_mdl)$coefficients[,4][[1]]
cr_final[9,5] <- summary(cr_pu_mdl)$coefficients[,4][[2]]

cr_final[10,2] <- summary(cr_te_mdl)$coefficients[,1][[1]]
cr_final[10,3] <- summary(cr_te_mdl)$coefficients[,1][[2]]
cr_final[10,4] <- summary(cr_te_mdl)$coefficients[,4][[1]]
cr_final[10,5] <- summary(cr_te_mdl)$coefficients[,4][[2]]

cr_final[11,2] <- summary(cr_tr_mdl)$coefficients[,1][[1]]
cr_final[11,3] <- summary(cr_tr_mdl)$coefficients[,1][[2]]
cr_final[11,4] <- summary(cr_tr_mdl)$coefficients[,4][[1]]
cr_final[11,5] <- summary(cr_tr_mdl)$coefficients[,4][[2]]


ggplot(ass, aes(x=sector, y=log(assets), fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ylab('Total assets (ln)') + xlab('Industry Sectors') + ggtitle('Total assets of NASDAQ listings in fiscal year of 2016') + labs(fill='Sectors') + theme(text = element_text(size=20)) 

ggplot(result_stock_return_less, aes(x=sector, y=annual_return, fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ylab('Annual Stock Return') + xlab('Industry Sectors') + ggtitle('Annual stock return for all NASDAQ listings in 2016') + labs(fill='Sectors') + theme(text = element_text(size=20)) 

ggplot(licu, aes(x=sector, y=log(current_liabilities), fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(sheq, aes(x=sector, y=log(shareholders_equity), fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(nein, aes(x=sector, y=log(net_income), fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(ascu, aes(x=sector, y=log(current_assets), fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(roa, aes(x=sector, y=return_on_assets, fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(c(-0.5, 0.5)) + ylab('Return on Assets') + xlab('Industry Sectors') + ggtitle('Return on assets of NASDAQ listings in the fiscal year of 2016') + labs(fill='Sectors') + theme(text = element_text(size=20)) 

ggplot(roe, aes(x=sector, y=return_on_equity, fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  ylim(c(-1, 1)) + ylab('Return on Equity') + xlab('Industry Sectors') + ggtitle('Return on equity of NASDAQ listings in the fiscal year of 2016') + labs(fill='Sectors') + theme(text = element_text(size=20)) 

ggplot(cr, aes(x=sector, y=log(current_ratio), fill=sector)) + geom_boxplot() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab('Current Ratio') + xlab('Industry Sectors') + ggtitle('Current ratio of NASDAQ listings in the fiscal year of 2016') + labs(fill='Sectors') + theme(text = element_text(size=20)) 


roe_BI_shift <- roe_BI
roe_BI_shift$annual_return <- roe_BI_shift$annual_return + 1.1
roe_bi_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_BI_shift)
roe_bi_cox = boxCox(roe_bi_shift_mdl)
roe_bi_lambda = roe_bi_cox$x[which(roe_bi_cox$y == max(roe_bi_cox$y))]
roe_bi_annual_return = (roe_BI_shift$annual_return^roe_bi_lambda - 1)/roe_bi_lambda 
roe_bi_shift_model = lm(roe_bi_annual_return ~ roe_BI_shift$return_on_equity) 
summary(roe_bi_shift_model) 

roe_CG_shift <- roe_CG
roe_CG_shift$annual_return <- roe_CG_shift$annual_return + 1.1
roe_cg_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_CG_shift)
roe_cg_cox = boxCox(roe_cg_shift_mdl)
roe_cg_lambda = roe_cg_cox$x[which(roe_cg_cox$y == max(roe_cg_cox$y))]
roe_cg_annual_return = (roe_CG_shift$annual_return^roe_cg_lambda - 1)/roe_cg_lambda 
roe_cg_shift_model = lm(roe_cg_annual_return ~ roe_CG_shift$return_on_equity) 
summary(roe_cg_shift_model) 

roe_CND_shift <- roe_CND
roe_CND_shift$annual_return <- roe_CND_shift$annual_return + 1.1
roe_cnd_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_CND_shift)
roe_cnd_cox = boxCox(roe_cnd_shift_mdl)
roe_cnd_lambda = roe_cnd_cox$x[which(roe_cnd_cox$y == max(roe_cnd_cox$y))]
roe_cnd_annual_return = (roe_CND_shift$annual_return^roe_cnd_lambda - 1)/roe_cnd_lambda 
roe_cnd_shift_model = lm(roe_cnd_annual_return ~ roe_CND_shift$return_on_equity) 
summary(roe_cnd_shift_model) 

roe_CS_shift <- roe_CS
roe_CS_shift$annual_return <- roe_CS_shift$annual_return + 1.1
roe_cs_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_CS_shift)
roe_cs_cox = boxCox(roe_cs_shift_mdl)
roe_cs_lambda = roe_cs_cox$x[which(roe_cs_cox$y == max(roe_cs_cox$y))]
roe_cs_annual_return = (roe_CS_shift$annual_return^roe_cs_lambda - 1)/roe_cs_lambda 
roe_cs_shift_model = lm(roe_cs_annual_return ~ roe_CS_shift$return_on_equity) 
summary(roe_cs_shift_model) 

roe_E_shift <- roe_E
roe_E_shift$annual_return <- roe_E_shift$annual_return + 1.1
roe_e_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_E_shift)
roe_e_cox = boxCox(roe_e_shift_mdl)
roe_e_lambda = roe_e_cox$x[which(roe_e_cox$y == max(roe_e_cox$y))]
roe_e_annual_return = (roe_E_shift$annual_return^roe_e_lambda - 1)/roe_e_lambda 
roe_e_shift_model = lm(roe_e_annual_return ~ roe_E_shift$return_on_equity) 
summary(roe_e_shift_model)

roe_F_shift <- roe_F
roe_F_shift$annual_return <- roe_F_shift$annual_return + 1.1
roe_f_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_F_shift)
roe_f_cox = boxCox(roe_f_shift_mdl)
roe_f_lambda = roe_f_cox$x[which(roe_f_cox$y == max(roe_f_cox$y))]
roe_f_annual_return = (roe_F_shift$annual_return^roe_f_lambda - 1)/roe_f_lambda 
roe_f_shift_model = lm(roe_f_annual_return ~ roe_F_shift$return_on_equity) 
summary(roe_f_shift_model)

roe_H_shift <- roe_H
roe_H_shift$annual_return <- roe_H_shift$annual_return + 1.1
roe_h_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_H_shift)
roe_h_cox = boxCox(roe_h_shift_mdl)
roe_h_lambda = roe_h_cox$x[which(roe_h_cox$y == max(roe_h_cox$y))]
roe_h_annual_return = (roe_H_shift$annual_return^roe_h_lambda - 1)/roe_h_lambda 
roe_h_shift_model = lm(roe_h_annual_return ~ roe_H_shift$return_on_equity) 
summary(roe_h_shift_model)

roe_M_shift <- roe_M
roe_M_shift$annual_return <- roe_M_shift$annual_return + 1.1
roe_m_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_M_shift)
roe_m_cox = boxCox(roe_m_shift_mdl)
roe_m_lambda = roe_m_cox$x[which(roe_m_cox$y == max(roe_m_cox$y))]
roe_m_annual_return = (roe_M_shift$annual_return^roe_m_lambda - 1)/roe_m_lambda 
roe_m_shift_model = lm(roe_m_annual_return ~ roe_M_shift$return_on_equity) 
summary(roe_m_shift_model)

roe_PU_shift <- roe_PU
roe_PU_shift$annual_return <- roe_PU_shift$annual_return + 1.1
roe_pu_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_PU_shift)
roe_pu_cox = boxCox(roe_pu_shift_mdl)
roe_pu_lambda = roe_pu_cox$x[which(roe_pu_cox$y == max(roe_pu_cox$y))]
roe_pu_annual_return = (roe_PU_shift$annual_return^roe_pu_lambda - 1)/roe_pu_lambda 
roe_pu_shift_model = lm(roe_pu_annual_return ~ roe_PU_shift$return_on_equity) 
summary(roe_pu_shift_model)

roe_TE_shift <- roe_TE
roe_TE_shift$annual_return <- roe_TE_shift$annual_return + 1.1
roe_te_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_TE_shift)
roe_te_cox = boxCox(roe_te_shift_mdl)
roe_te_lambda = roe_te_cox$x[which(roe_te_cox$y == max(roe_te_cox$y))]
roe_te_annual_return = (roe_TE_shift$annual_return^roe_te_lambda - 1)/roe_te_lambda 
roe_te_shift_model = lm(roe_te_annual_return ~ roe_TE_shift$return_on_equity) 
summary(roe_te_shift_model)

roe_TR_shift <- roe_TR
roe_TR_shift$annual_return <- roe_TR_shift$annual_return + 1.1
roe_tr_shift_mdl <- lm(annual_return ~ return_on_equity, data = roe_TR_shift)
roe_tr_cox = boxCox(roe_tr_shift_mdl)
roe_tr_lambda = roe_tr_cox$x[which(roe_tr_cox$y == max(roe_tr_cox$y))]
roe_tr_annual_return = (roe_TR_shift$annual_return^roe_tr_lambda - 1)/roe_tr_lambda 
roe_tr_shift_model = lm(roe_tr_annual_return ~ roe_TR_shift$return_on_equity) 
summary(roe_tr_shift_model)


########################################


roa_BI_shift <- roa_BI
roa_BI_shift$annual_return <- roa_BI_shift$annual_return + 1.1
roa_bi_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_BI_shift)
roa_bi_cox = boxCox(roa_bi_shift_mdl)
roa_bi_lambda = roa_bi_cox$x[which(roa_bi_cox$y == max(roa_bi_cox$y))]
roa_bi_annual_return = (roa_BI_shift$annual_return^roa_bi_lambda - 1)/roa_bi_lambda 
roa_bi_shift_model = lm(roa_bi_annual_return ~ roa_BI_shift$return_on_assets) 
summary(roa_bi_shift_model) 

roa_CG_shift <- roa_CG
roa_CG_shift$annual_return <- roa_CG_shift$annual_return + 1.1
roa_cg_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_CG_shift)
roa_cg_cox = boxCox(roa_cg_shift_mdl)
roa_cg_lambda = roa_cg_cox$x[which(roa_cg_cox$y == max(roa_cg_cox$y))]
roa_cg_annual_return = (roa_CG_shift$annual_return^roa_cg_lambda - 1)/roa_cg_lambda 
roa_cg_shift_model = lm(roa_cg_annual_return ~ roa_CG_shift$return_on_assets) 
summary(roa_cg_shift_model) 

roa_CND_shift <- roa_CND
roa_CND_shift$annual_return <- roa_CND_shift$annual_return + 1.1
roa_cnd_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_CND_shift)
roa_cnd_cox = boxCox(roa_cnd_shift_mdl)
roa_cnd_lambda = roa_cnd_cox$x[which(roa_cnd_cox$y == max(roa_cnd_cox$y))]
roa_cnd_annual_return = (roa_CND_shift$annual_return^roa_cnd_lambda - 1)/roa_cnd_lambda 
roa_cnd_shift_model = lm(roa_cnd_annual_return ~ roa_CND_shift$return_on_assets) 
summary(roa_cnd_shift_model) 

roa_CS_shift <- roa_CS
roa_CS_shift$annual_return <- roa_CS_shift$annual_return + 1.1
roa_cs_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_CS_shift)
roa_cs_cox = boxCox(roa_cs_shift_mdl)
roa_cs_lambda = roa_cs_cox$x[which(roa_cs_cox$y == max(roa_cs_cox$y))]
roa_cs_annual_return = (roa_CS_shift$annual_return^roa_cs_lambda - 1)/roa_cs_lambda 
roa_cs_shift_model = lm(roa_cs_annual_return ~ roa_CS_shift$return_on_assets) 
summary(roa_cs_shift_model) 

roa_E_shift <- roa_E
roa_E_shift$annual_return <- roa_E_shift$annual_return + 1.1
roa_e_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_E_shift)
roa_e_cox = boxCox(roa_e_shift_mdl)
roa_e_lambda = roa_e_cox$x[which(roa_e_cox$y == max(roa_e_cox$y))]
roa_e_annual_return = (roa_E_shift$annual_return^roa_e_lambda - 1)/roa_e_lambda 
roa_e_shift_model = lm(roa_e_annual_return ~ roa_E_shift$return_on_assets) 
summary(roa_e_shift_model)

roa_F_shift <- roa_F
roa_F_shift$annual_return <- roa_F_shift$annual_return + 1.1
roa_f_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_F_shift)
roa_f_cox = boxCox(roa_f_shift_mdl)
roa_f_lambda = roa_f_cox$x[which(roa_f_cox$y == max(roa_f_cox$y))]
roa_f_annual_return = (roa_F_shift$annual_return^roa_f_lambda - 1)/roa_f_lambda 
roa_f_shift_model = lm(roa_f_annual_return ~ roa_F_shift$return_on_assets) 
summary(roa_f_shift_model)

roa_H_shift <- roa_H
roa_H_shift$annual_return <- roa_H_shift$annual_return + 1.1
roa_h_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_H_shift)
roa_h_cox = boxCox(roa_h_shift_mdl)
roa_h_lambda = roa_h_cox$x[which(roa_h_cox$y == max(roa_h_cox$y))]
roa_h_annual_return = (roa_H_shift$annual_return^roa_h_lambda - 1)/roa_h_lambda 
roa_h_shift_model = lm(roa_h_annual_return ~ roa_H_shift$return_on_assets) 
summary(roa_h_shift_model)

roa_M_shift <- roa_M
roa_M_shift$annual_return <- roa_M_shift$annual_return + 1.1
roa_m_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_M_shift)
roa_m_cox = boxCox(roa_m_shift_mdl)
roa_m_lambda = roa_m_cox$x[which(roa_m_cox$y == max(roa_m_cox$y))]
roa_m_annual_return = (roa_M_shift$annual_return^roa_m_lambda - 1)/roa_m_lambda 
roa_m_shift_model = lm(roa_m_annual_return ~ roa_M_shift$return_on_assets) 
summary(roa_m_shift_model)

roa_PU_shift <- roa_PU
roa_PU_shift$annual_return <- roa_PU_shift$annual_return + 1.1
roa_pu_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_PU_shift)
roa_pu_cox = boxCox(roa_pu_shift_mdl)
roa_pu_lambda = roa_pu_cox$x[which(roa_pu_cox$y == max(roa_pu_cox$y))]
roa_pu_annual_return = (roa_PU_shift$annual_return^roa_pu_lambda - 1)/roa_pu_lambda 
roa_pu_shift_model = lm(roa_pu_annual_return ~ roa_PU_shift$return_on_assets) 
summary(roa_pu_shift_model)

roa_TE_shift <- roa_TE
roa_TE_shift$annual_return <- roa_TE_shift$annual_return + 1.1
roa_te_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_TE_shift)
roa_te_cox = boxCox(roa_te_shift_mdl)
roa_te_lambda = roa_te_cox$x[which(roa_te_cox$y == max(roa_te_cox$y))]
roa_te_annual_return = (roa_TE_shift$annual_return^roa_te_lambda - 1)/roa_te_lambda 
roa_te_shift_model = lm(roa_te_annual_return ~ roa_TE_shift$return_on_assets) 
summary(roa_te_shift_model)

roa_TR_shift <- roa_TR
roa_TR_shift$annual_return <- roa_TR_shift$annual_return + 1.1
roa_tr_shift_mdl <- lm(annual_return ~ return_on_assets, data = roa_TR_shift)
roa_tr_cox = boxCox(roa_tr_shift_mdl)
roa_tr_lambda = roa_tr_cox$x[which(roa_tr_cox$y == max(roa_tr_cox$y))]
roa_tr_annual_return = (roa_TR_shift$annual_return^roa_tr_lambda - 1)/roa_tr_lambda 
roa_tr_shift_model = lm(roa_tr_annual_return ~ roa_TR_shift$return_on_assets) 
summary(roa_tr_shift_model)


######################################################cr


cr_BI_shift <- cr_BI
cr_BI_shift$annual_return <- cr_BI_shift$annual_return + 1.1
cr_bi_shift_mdl <- lm(annual_return ~ current_assets, data = cr_BI_shift)
cr_bi_cox = boxCox(cr_bi_shift_mdl)
cr_bi_lambda = cr_bi_cox$x[which(cr_bi_cox$y == max(cr_bi_cox$y))]
cr_bi_annual_return = (cr_BI_shift$annual_return^cr_bi_lambda - 1)/cr_bi_lambda 
cr_bi_shift_model = lm(cr_bi_annual_return ~ cr_BI_shift$current_assets) 
summary(cr_bi_shift_model) 

cr_CG_shift <- cr_CG
cr_CG_shift$annual_return <- cr_CG_shift$annual_return + 1.1
cr_cg_shift_mdl <- lm(annual_return ~ current_assets, data = cr_CG_shift)
cr_cg_cox = boxCox(cr_cg_shift_mdl)
cr_cg_lambda = cr_cg_cox$x[which(cr_cg_cox$y == max(cr_cg_cox$y))]
cr_cg_annual_return = (cr_CG_shift$annual_return^cr_cg_lambda - 1)/cr_cg_lambda 
cr_cg_shift_model = lm(cr_cg_annual_return ~ cr_CG_shift$current_assets) 
summary(cr_cg_shift_model) 

cr_CND_shift <- cr_CND
cr_CND_shift$annual_return <- cr_CND_shift$annual_return + 1.1
cr_cnd_shift_mdl <- lm(annual_return ~ current_assets, data = cr_CND_shift)
cr_cnd_cox = boxCox(cr_cnd_shift_mdl)
cr_cnd_lambda = cr_cnd_cox$x[which(cr_cnd_cox$y == max(cr_cnd_cox$y))]
cr_cnd_annual_return = (cr_CND_shift$annual_return^cr_cnd_lambda - 1)/cr_cnd_lambda 
cr_cnd_shift_model = lm(cr_cnd_annual_return ~ cr_CND_shift$current_assets) 
summary(cr_cnd_shift_model) 

cr_CS_shift <- cr_CS
cr_CS_shift$annual_return <- cr_CS_shift$annual_return + 1.1
cr_cs_shift_mdl <- lm(annual_return ~ current_assets, data = cr_CS_shift)
cr_cs_cox = boxCox(cr_cs_shift_mdl)
cr_cs_lambda = cr_cs_cox$x[which(cr_cs_cox$y == max(cr_cs_cox$y))]
cr_cs_annual_return = (cr_CS_shift$annual_return^cr_cs_lambda - 1)/cr_cs_lambda 
cr_cs_shift_model = lm(cr_cs_annual_return ~ cr_CS_shift$current_assets) 
summary(cr_cs_shift_model) 

cr_E_shift <- cr_E
cr_E_shift$annual_return <- cr_E_shift$annual_return + 1.1
cr_e_shift_mdl <- lm(annual_return ~ current_assets, data = cr_E_shift)
cr_e_cox = boxCox(cr_e_shift_mdl)
cr_e_lambda = cr_e_cox$x[which(cr_e_cox$y == max(cr_e_cox$y))]
cr_e_annual_return = (cr_E_shift$annual_return^cr_e_lambda - 1)/cr_e_lambda 
cr_e_shift_model = lm(cr_e_annual_return ~ cr_E_shift$current_assets) 
summary(cr_e_shift_model)

cr_F_shift <- cr_F
cr_F_shift$annual_return <- cr_F_shift$annual_return + 1.1
cr_f_shift_mdl <- lm(annual_return ~ current_assets, data = cr_F_shift)
cr_f_cox = boxCox(cr_f_shift_mdl)
cr_f_lambda = cr_f_cox$x[which(cr_f_cox$y == max(cr_f_cox$y))]
cr_f_annual_return = (cr_F_shift$annual_return^cr_f_lambda - 1)/cr_f_lambda 
cr_f_shift_model = lm(cr_f_annual_return ~ cr_F_shift$current_assets) 
summary(cr_f_shift_model)

cr_H_shift <- cr_H
cr_H_shift$annual_return <- cr_H_shift$annual_return + 1.1
cr_h_shift_mdl <- lm(annual_return ~ current_assets, data = cr_H_shift)
cr_h_cox = boxCox(cr_h_shift_mdl)
cr_h_lambda = cr_h_cox$x[which(cr_h_cox$y == max(cr_h_cox$y))]
cr_h_annual_return = (cr_H_shift$annual_return^cr_h_lambda - 1)/cr_h_lambda 
cr_h_shift_model = lm(cr_h_annual_return ~ cr_H_shift$current_assets) 
summary(cr_h_shift_model)

cr_M_shift <- cr_M
cr_M_shift$annual_return <- cr_M_shift$annual_return + 1.1
cr_m_shift_mdl <- lm(annual_return ~ current_assets, data = cr_M_shift)
cr_m_cox = boxCox(cr_m_shift_mdl)
cr_m_lambda = cr_m_cox$x[which(cr_m_cox$y == max(cr_m_cox$y))]
cr_m_annual_return = (cr_M_shift$annual_return^cr_m_lambda - 1)/cr_m_lambda 
cr_m_shift_model = lm(cr_m_annual_return ~ cr_M_shift$current_assets) 
summary(cr_m_shift_model)

cr_PU_shift <- cr_PU
cr_PU_shift$annual_return <- cr_PU_shift$annual_return + 1.1
cr_pu_shift_mdl <- lm(annual_return ~ current_assets, data = cr_PU_shift)
cr_pu_cox = boxCox(cr_pu_shift_mdl)
cr_pu_lambda = cr_pu_cox$x[which(cr_pu_cox$y == max(cr_pu_cox$y))]
cr_pu_annual_return = (cr_PU_shift$annual_return^cr_pu_lambda - 1)/cr_pu_lambda 
cr_pu_shift_model = lm(cr_pu_annual_return ~ cr_PU_shift$current_assets) 
summary(cr_pu_shift_model)

cr_TE_shift <- cr_TE
cr_TE_shift$annual_return <- cr_TE_shift$annual_return + 1.1
cr_te_shift_mdl <- lm(annual_return ~ current_assets, data = cr_TE_shift)
cr_te_cox = boxCox(cr_te_shift_mdl)
cr_te_lambda = cr_te_cox$x[which(cr_te_cox$y == max(cr_te_cox$y))]
cr_te_annual_return = (cr_TE_shift$annual_return^cr_te_lambda - 1)/cr_te_lambda 
cr_te_shift_model = lm(cr_te_annual_return ~ cr_TE_shift$current_assets) 
summary(cr_te_shift_model)

cr_TR_shift <- cr_TR
cr_TR_shift$annual_return <- cr_TR_shift$annual_return + 1.1
cr_tr_shift_mdl <- lm(annual_return ~ current_assets, data = cr_TR_shift)
cr_tr_cox = boxCox(cr_tr_shift_mdl)
cr_tr_lambda = cr_tr_cox$x[which(cr_tr_cox$y == max(cr_tr_cox$y))]
cr_tr_annual_return = (cr_TR_shift$annual_return^cr_tr_lambda - 1)/cr_tr_lambda 
cr_tr_shift_model = lm(cr_tr_annual_return ~ cr_TR_shift$current_assets) 
summary(cr_tr_shift_model)

###############################final

roe_final_cox <- roe_final
roa_final_cox <- roa_final
cr_final_cox <- cr_final

roe_final_cox[1,2] <- summary(roe_bi_shift_model)$coefficients[,1][[1]]
roe_final_cox[1,3] <- summary(roe_bi_shift_model)$coefficients[,1][[2]]
roe_final_cox[1,4] <- summary(roe_bi_shift_model)$coefficients[,4][[1]]
roe_final_cox[1,5] <- summary(roe_bi_shift_model)$coefficients[,4][[2]]

roe_final_cox[2,2] <- summary(roe_cg_shift_model)$coefficients[,1][[1]]
roe_final_cox[2,3] <- summary(roe_cg_shift_model)$coefficients[,1][[2]]
roe_final_cox[2,4] <- summary(roe_cg_shift_model)$coefficients[,4][[1]]
roe_final_cox[2,5] <- summary(roe_cg_shift_model)$coefficients[,4][[2]]

roe_final_cox[3,2] <- summary(roe_cnd_shift_model)$coefficients[,1][[1]]
roe_final_cox[3,3] <- summary(roe_cnd_shift_model)$coefficients[,1][[2]]
roe_final_cox[3,4] <- summary(roe_cnd_shift_model)$coefficients[,4][[1]]
roe_final_cox[3,5] <- summary(roe_cnd_shift_model)$coefficients[,4][[2]]

roe_final_cox[4,2] <- summary(roe_cs_shift_model)$coefficients[,1][[1]]
roe_final_cox[4,3] <- summary(roe_cs_shift_model)$coefficients[,1][[2]]
roe_final_cox[4,4] <- summary(roe_cs_shift_model)$coefficients[,4][[1]]
roe_final_cox[4,5] <- summary(roe_cs_shift_model)$coefficients[,4][[2]]

roe_final_cox[5,2] <- summary(roe_e_shift_model)$coefficients[,1][[1]]
roe_final_cox[5,3] <- summary(roe_e_shift_model)$coefficients[,1][[2]]
roe_final_cox[5,4] <- summary(roe_e_shift_model)$coefficients[,4][[1]]
roe_final_cox[5,5] <- summary(roe_e_shift_model)$coefficients[,4][[2]]

roe_final_cox[6,2] <- summary(roe_f_shift_model)$coefficients[,1][[1]]
roe_final_cox[6,3] <- summary(roe_f_shift_model)$coefficients[,1][[2]]
roe_final_cox[6,4] <- summary(roe_f_shift_model)$coefficients[,4][[1]]
roe_final_cox[6,5] <- summary(roe_f_shift_model)$coefficients[,4][[2]]

roe_final_cox[7,2] <- summary(roe_h_shift_model)$coefficients[,1][[1]]
roe_final_cox[7,3] <- summary(roe_h_shift_model)$coefficients[,1][[2]]
roe_final_cox[7,4] <- summary(roe_h_shift_model)$coefficients[,4][[1]]
roe_final_cox[7,5] <- summary(roe_h_shift_model)$coefficients[,4][[2]]

roe_final_cox[8,2] <- summary(roe_m_shift_model)$coefficients[,1][[1]]
roe_final_cox[8,3] <- summary(roe_m_shift_model)$coefficients[,1][[2]]
roe_final_cox[8,4] <- summary(roe_m_shift_model)$coefficients[,4][[1]]
roe_final_cox[8,5] <- summary(roe_m_shift_model)$coefficients[,4][[2]]

roe_final_cox[9,2] <- summary(roe_pu_shift_model)$coefficients[,1][[1]]
roe_final_cox[9,3] <- summary(roe_pu_shift_model)$coefficients[,1][[2]]
roe_final_cox[9,4] <- summary(roe_pu_shift_model)$coefficients[,4][[1]]
roe_final_cox[9,5] <- summary(roe_pu_shift_model)$coefficients[,4][[2]]

roe_final_cox[10,2] <- summary(roe_te_shift_model)$coefficients[,1][[1]]
roe_final_cox[10,3] <- summary(roe_te_shift_model)$coefficients[,1][[2]]
roe_final_cox[10,4] <- summary(roe_te_shift_model)$coefficients[,4][[1]]
roe_final_cox[10,5] <- summary(roe_te_shift_model)$coefficients[,4][[2]]

roe_final_cox[11,2] <- summary(roe_tr_shift_model)$coefficients[,1][[1]]
roe_final_cox[11,3] <- summary(roe_tr_shift_model)$coefficients[,1][[2]]
roe_final_cox[11,4] <- summary(roe_tr_shift_model)$coefficients[,4][[1]]
roe_final_cox[11,5] <- summary(roe_tr_shift_model)$coefficients[,4][[2]]


roa_final_cox[1,2] <- summary(roa_bi_shift_model)$coefficients[,1][[1]]
roa_final_cox[1,3] <- summary(roa_bi_shift_model)$coefficients[,1][[2]]
roa_final_cox[1,4] <- summary(roa_bi_shift_model)$coefficients[,4][[1]]
roa_final_cox[1,5] <- summary(roa_bi_shift_model)$coefficients[,4][[2]]

roa_final_cox[2,2] <- summary(roa_cg_shift_model)$coefficients[,1][[1]]
roa_final_cox[2,3] <- summary(roa_cg_shift_model)$coefficients[,1][[2]]
roa_final_cox[2,4] <- summary(roa_cg_shift_model)$coefficients[,4][[1]]
roa_final_cox[2,5] <- summary(roa_cg_shift_model)$coefficients[,4][[2]]

roa_final_cox[3,2] <- summary(roa_cnd_shift_model)$coefficients[,1][[1]]
roa_final_cox[3,3] <- summary(roa_cnd_shift_model)$coefficients[,1][[2]]
roa_final_cox[3,4] <- summary(roa_cnd_shift_model)$coefficients[,4][[1]]
roa_final_cox[3,5] <- summary(roa_cnd_shift_model)$coefficients[,4][[2]]

roa_final_cox[4,2] <- summary(roa_cs_shift_model)$coefficients[,1][[1]]
roa_final_cox[4,3] <- summary(roa_cs_shift_model)$coefficients[,1][[2]]
roa_final_cox[4,4] <- summary(roa_cs_shift_model)$coefficients[,4][[1]]
roa_final_cox[4,5] <- summary(roa_cs_shift_model)$coefficients[,4][[2]]

roa_final_cox[5,2] <- summary(roa_e_shift_model)$coefficients[,1][[1]]
roa_final_cox[5,3] <- summary(roa_e_shift_model)$coefficients[,1][[2]]
roa_final_cox[5,4] <- summary(roa_e_shift_model)$coefficients[,4][[1]]
roa_final_cox[5,5] <- summary(roa_e_shift_model)$coefficients[,4][[2]]

roa_final_cox[6,2] <- summary(roa_f_shift_model)$coefficients[,1][[1]]
roa_final_cox[6,3] <- summary(roa_f_shift_model)$coefficients[,1][[2]]
roa_final_cox[6,4] <- summary(roa_f_shift_model)$coefficients[,4][[1]]
roa_final_cox[6,5] <- summary(roa_f_shift_model)$coefficients[,4][[2]]

roa_final_cox[7,2] <- summary(roa_h_shift_model)$coefficients[,1][[1]]
roa_final_cox[7,3] <- summary(roa_h_shift_model)$coefficients[,1][[2]]
roa_final_cox[7,4] <- summary(roa_h_shift_model)$coefficients[,4][[1]]
roa_final_cox[7,5] <- summary(roa_h_shift_model)$coefficients[,4][[2]]

roa_final_cox[8,2] <- summary(roa_m_shift_model)$coefficients[,1][[1]]
roa_final_cox[8,3] <- summary(roa_m_shift_model)$coefficients[,1][[2]]
roa_final_cox[8,4] <- summary(roa_m_shift_model)$coefficients[,4][[1]]
roa_final_cox[8,5] <- summary(roa_m_shift_model)$coefficients[,4][[2]]

roa_final_cox[9,2] <- summary(roa_pu_shift_model)$coefficients[,1][[1]]
roa_final_cox[9,3] <- summary(roa_pu_shift_model)$coefficients[,1][[2]]
roa_final_cox[9,4] <- summary(roa_pu_shift_model)$coefficients[,4][[1]]
roa_final_cox[9,5] <- summary(roa_pu_shift_model)$coefficients[,4][[2]]

roa_final_cox[10,2] <- summary(roa_te_shift_model)$coefficients[,1][[1]]
roa_final_cox[10,3] <- summary(roa_te_shift_model)$coefficients[,1][[2]]
roa_final_cox[10,4] <- summary(roa_te_shift_model)$coefficients[,4][[1]]
roa_final_cox[10,5] <- summary(roa_te_shift_model)$coefficients[,4][[2]]

roa_final_cox[11,2] <- summary(roa_tr_shift_model)$coefficients[,1][[1]]
roa_final_cox[11,3] <- summary(roa_tr_shift_model)$coefficients[,1][[2]]
roa_final_cox[11,4] <- summary(roa_tr_shift_model)$coefficients[,4][[1]]
roa_final_cox[11,5] <- summary(roa_tr_shift_model)$coefficients[,4][[2]]

cr_final_cox[1,2] <- summary(cr_bi_shift_model)$coefficients[,1][[1]]
cr_final_cox[1,3] <- summary(cr_bi_shift_model)$coefficients[,1][[2]]
cr_final_cox[1,4] <- summary(cr_bi_shift_model)$coefficients[,4][[1]]
cr_final_cox[1,5] <- summary(cr_bi_shift_model)$coefficients[,4][[2]]

cr_final_cox[2,2] <- summary(cr_cg_shift_model)$coefficients[,1][[1]]
cr_final_cox[2,3] <- summary(cr_cg_shift_model)$coefficients[,1][[2]]
cr_final_cox[2,4] <- summary(cr_cg_shift_model)$coefficients[,4][[1]]
cr_final_cox[2,5] <- summary(cr_cg_shift_model)$coefficients[,4][[2]]

cr_final_cox[3,2] <- summary(cr_cnd_shift_model)$coefficients[,1][[1]]
cr_final_cox[3,3] <- summary(cr_cnd_shift_model)$coefficients[,1][[2]]
cr_final_cox[3,4] <- summary(cr_cnd_shift_model)$coefficients[,4][[1]]
cr_final_cox[3,5] <- summary(cr_cnd_shift_model)$coefficients[,4][[2]]

cr_final_cox[4,2] <- summary(cr_cs_shift_model)$coefficients[,1][[1]]
cr_final_cox[4,3] <- summary(cr_cs_shift_model)$coefficients[,1][[2]]
cr_final_cox[4,4] <- summary(cr_cs_shift_model)$coefficients[,4][[1]]
cr_final_cox[4,5] <- summary(cr_cs_shift_model)$coefficients[,4][[2]]

cr_final_cox[5,2] <- summary(cr_e_shift_model)$coefficients[,1][[1]]
cr_final_cox[5,3] <- summary(cr_e_shift_model)$coefficients[,1][[2]]
cr_final_cox[5,4] <- summary(cr_e_shift_model)$coefficients[,4][[1]]
cr_final_cox[5,5] <- summary(cr_e_shift_model)$coefficients[,4][[2]]

cr_final_cox[6,2] <- summary(cr_f_shift_model)$coefficients[,1][[1]]
cr_final_cox[6,3] <- summary(cr_f_shift_model)$coefficients[,1][[2]]
cr_final_cox[6,4] <- summary(cr_f_shift_model)$coefficients[,4][[1]]
cr_final_cox[6,5] <- summary(cr_f_shift_model)$coefficients[,4][[2]]

cr_final_cox[7,2] <- summary(cr_h_shift_model)$coefficients[,1][[1]]
cr_final_cox[7,3] <- summary(cr_h_shift_model)$coefficients[,1][[2]]
cr_final_cox[7,4] <- summary(cr_h_shift_model)$coefficients[,4][[1]]
cr_final_cox[7,5] <- summary(cr_h_shift_model)$coefficients[,4][[2]]

cr_final_cox[8,2] <- summary(cr_m_shift_model)$coefficients[,1][[1]]
cr_final_cox[8,3] <- summary(cr_m_shift_model)$coefficients[,1][[2]]
cr_final_cox[8,4] <- summary(cr_m_shift_model)$coefficients[,4][[1]]
cr_final_cox[8,5] <- summary(cr_m_shift_model)$coefficients[,4][[2]]

cr_final_cox[9,2] <- summary(cr_pu_shift_model)$coefficients[,1][[1]]
cr_final_cox[9,3] <- summary(cr_pu_shift_model)$coefficients[,1][[2]]
cr_final_cox[9,4] <- summary(cr_pu_shift_model)$coefficients[,4][[1]]
cr_final_cox[9,5] <- summary(cr_pu_shift_model)$coefficients[,4][[2]]

cr_final_cox[10,2] <- summary(cr_te_shift_model)$coefficients[,1][[1]]
cr_final_cox[10,3] <- summary(cr_te_shift_model)$coefficients[,1][[2]]
cr_final_cox[10,4] <- summary(cr_te_shift_model)$coefficients[,4][[1]]
cr_final_cox[10,5] <- summary(cr_te_shift_model)$coefficients[,4][[2]]

cr_final_cox[11,2] <- summary(cr_tr_shift_model)$coefficients[,1][[1]]
cr_final_cox[11,3] <- summary(cr_tr_shift_model)$coefficients[,1][[2]]
cr_final_cox[11,4] <- summary(cr_tr_shift_model)$coefficients[,4][[1]]
cr_final_cox[11,5] <- summary(cr_tr_shift_model)$coefficients[,4][[2]]



cr_for_cox <- cr %>% select(symbol, current_ratio, annual_return, sector)
cr_for_cox$annual_return <- cr_for_cox$annual_return + 1
cr_for_cox_mdl <- lm(annual_return ~ current_ratio, data = cr_for_cox)
cr_cox_result = boxCox(cr_for_cox_mdl)
cr_cox_lambda = cr_cox_result$x[which(cr_cox_result$y == max(cr_cox_result$y))]
cr_cox_annual_return = (cr_for_cox$annual_return^cr_cox_lambda - 1)/cr_cox_lambda 
cr_cox_model = lm(cr_cox_annual_return ~ cr_for_cox$current_ratio) 
summary(cr_cox_model) 

roa_for_cox <- roa %>% select(symbol, return_on_assets, annual_return, sector)
roa_for_cox$annual_return <- roa_for_cox$annual_return + 1
roa_for_cox_mdl <- lm(annual_return ~ return_on_assets, data = roa_for_cox)
roa_cox_result = boxCox(roa_for_cox_mdl)
roa_cox_lambda = roa_cox_result$x[which(roa_cox_result$y == max(roa_cox_result$y))]
roa_cox_annual_return = (roa_for_cox$annual_return^roa_cox_lambda - 1)/roa_cox_lambda 
roa_cox_model = lm(roa_cox_annual_return ~ roa_for_cox$return_on_assets) 
summary(roa_cox_model)


roe_for_cox <- roe %>% select(symbol, return_on_equity, annual_return, sector)
roe_for_cox$annual_return <- roe_for_cox$annual_return + 1
roe_for_cox_mdl <- lm(annual_return ~ return_on_equity, data = roe_for_cox)
roe_cox_result = boxCox(roe_for_cox_mdl)
roe_cox_lambda = roe_cox_result$x[which(roe_cox_result$y == max(roe_cox_result$y))]
roe_cox_annual_return = (roe_for_cox$annual_return^roe_cox_lambda - 1)/roe_cox_lambda 
roe_cox_model = lm(roe_cox_annual_return ~ roe_for_cox$return_on_equity) 
summary(roe_cox_model)

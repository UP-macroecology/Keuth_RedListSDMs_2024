library(modelsummary)
load("/import/ecoc9z/data-zurell/keuth/Model_Brms_cluster.Rdata")
modelsummary(model_mbrms, statistic = "mad", output = "/import/ecoc9z/data-zurell/keuth/table_models_bayesian_poploss_hsloss.html")
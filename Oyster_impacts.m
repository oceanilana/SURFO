[Result_pred,Headers,Niceheaders]=CO2SYS(2010,1889,1,2,29,19,19,0,0,15,1,1,4,1)

pCO2_predicted_mean=Result_pred(4)  %%%607
Omega_predicted_mean=Result_pred(16)  %%% 1.5


CO2flux_mean_predicted=co_co2flux(pCO2_predicted_mean,410,19,29,8)/3 *4 *16956.34*44.01/1000000 %% metric ton in trip's 4.17 acres farm

%%% 9.3741

[Result_calc,Headers,Niceheaders]=CO2SYS(2010-42,1889-21,1,2,29,19,19,0,0,15,1,1,4,1) % we yse this because delta DIC and TA calculated have phytoplankton
pCO2_calc_mean=Result_calc(4)  %%%691.9802
Omega_calc_mean=Result_calc(16)  %%% 1.3135

CO2flux_mean_calc=co_co2flux(pCO2_calc_mean,410,19,29,8)/3 *4 *16956.34*44.01/1000000 %% metric ton in trip's 4.17 acres farm

%13.4453


%TA budget tells us the increase air Co2 flux should be 225, 
%so the 'true' co2 emission should not limited to the farm regions only, to match the TA-based CO2 emission,  the CO2 outgas should cover a regions of:

225/CO2flux_mean_calc*4.19;





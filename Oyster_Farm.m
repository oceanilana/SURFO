close all;
clear all;
% add the data 
addpath("/Users/ilanajacobs/SURFO")
bottlesamples_s=readtable("Wickford_Oyster_Farm_Data_Surface.csv");
bottlesamples_b=readtable("Wickford_Oyster_Farm_Data_Bottom.csv");
bottlesamples_s.Density_TA=densityEOS(bottlesamples_s.T_TA,bottlesamples_s.M_S); 
bottlesamples_s.Density_DIC=densityEOS(bottlesamples_s.T_DIC, bottlesamples_s.M_S);
bottlesamples_s.TA_umol_kg=bottlesamples_s.TA_uM./bottlesamples_s.Density_TA;
bottlesamples_s.DIC_umol_kg=bottlesamples_s.DIC_uM./bottlesamples_s.Density_DIC;
bottlesamples_b.Density_TA=densityEOS(bottlesamples_b.T_TA,bottlesamples_b.M_S); 
bottlesamples_b.Density_DIC=densityEOS(bottlesamples_b.T_DIC, bottlesamples_b.M_S);
bottlesamples_b.TA_umol_kg=bottlesamples_b.TA_uM./bottlesamples_b.Density_TA;
bottlesamples_b.DIC_umol_kg=bottlesamples_b.DIC_uM./bottlesamples_b.Density_DIC;
% now start
%calculate carbonate remaining params for surface
result_s=CO2SYS(bottlesamples_s.TA_umol_kg,bottlesamples_s.DIC_umol_kg,1,2,bottlesamples_s.M_S, bottlesamples_s.IS_T, bottlesamples_s.IS_T,0,0,0,0,1,10,1);
bottlesamples_s.pHcalc=result_s(:,3);
bottlesamples_s.pCO2calc=result_s(:,4); 
bottlesamples_s.OmegaCacalc=result_s(:,15);
bottlesamples_s.OmegaArcalc=result_s(:,16);
%calculate carbonate remaining params for bottom
result_b=CO2SYS(bottlesamples_b.TA_umol_kg,bottlesamples_b.DIC_umol_kg,1,2,bottlesamples_b.M_S, bottlesamples_b.IS_T, bottlesamples_b.IS_T,0,0,0,0,1,10,1);
bottlesamples_b.pHcalc=result_b(:,3);
bottlesamples_b.pCO2calc=result_b(:,4); 
bottlesamples_b.OmegaCacalc=result_b(:,15);
bottlesamples_b.OmegaArcalc=result_b(:,16);
writetable(bottlesamples_s, '/Users/ilanajacobs/downloads/surface_co2sys.csv');
writetable(bottlesamples_b, 'Wickford_Oyster_Farm_Data_Bottom_CO2SYS.csv');

% Pimenta Work 
pimenta=readtable("Pimenta_CO2SYS.csv");
result_pimenta=CO2SYS(pimenta.TA,pimenta.DIC,1,2,pimenta.Salinity_ppt,pimenta.Temp_C, pimenta.Temp_C, 0,0,0,0,1,10,1);
pimenta.pHcalc=result_pimenta(:,3);
pimenta.pCO2calc=result_pimenta(:,4);
pimenta.OmegaCacalc=result_pimenta(:,15);
pimenta.OmegaArcalc=result_pimenta(:,16);

% Predicted pco2
surfacesamples=readtable("surface_co2sys.csv");
result_samples=CO2SYS(surfacesamples.TA_predicted, surfacesamples.DIC_predicted,1,2,surfacesamples.IS_S, surfacesamples.IS_T, surfacesamples.IS_T,0,0,0,0,1,10,1);
surfacesamples.pHpredicted=result_samples(:,3);
surfacesamples.pCO2predicted=result_samples(:,4);
surfacesamples.OmegaCapredicted=result_samples(:,15);
surfacesamples.OmegaArpredicted=result_samples(:,16);
writetable(surfacesamples, '/Users/ilanajacobs/downloads/surface_co2sys.csv')
% Plot TA vs Sal, Surface
%figure
%hold on
%scatter(bottlesamples_s.M_S, bottlesamples_s.TA_umol_kg, 50,bottlesamples_s.IS_T, 'filled','MarkerEdgeColor','k')
%colorbar;
% Plot DIC vs Sal, Surface
%figure
%hold on
%scatter(bottlesamples_s.M_S, bottlesamples_s.DIC_umol_kg, 50,bottlesamples_s.IS_T, 'filled','MarkerEdgeColor','k')
%colorbar;
% Plot TA vs Sal, Bottom
%figure
%hold on
%scatter(bottlesamples_b.M_S, bottlesamples_b.TA_umol_kg, 50,bottlesamples_b.IS_T, 'filled','MarkerEdgeColor','k')
%colorbar;
%From Wang Lab %
function co2_flux = co_co2flux(pco2_sea, pco2_air, sst, sss, wspd)

kt = co_gas_transfer_velocity(sst,wspd,0.251);
K0 = co_K0_Weiss(sst,sss);
dpco2 = pco2_sea - pco2_air;
co2_flux = kt .* K0 .* dpco2 * (24*365/100000); % Because kt is in cm/hour, pCO2 is in µatm, flux in most case is in mol C/m2/yr

end

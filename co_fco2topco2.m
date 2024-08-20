function pCO2 = co_fco2topco2(fCO2, T, P)
% pco2 = CO_FCO2TOPCO2(fco2,sst,sss)
% A function converted fCO2 to pCO2, tanslated from seacarb
% 
% Algorithm: 
% fco2 = pco2 * exp(B(T)p/RT);
% B = -1636.75 + 12.0408*sst - 3.27957e-2 * sst.^2 + 3.16528e-5 * sst.^3;
% T^3
% R = 831447, T: Kelvin Degree
% 
% References:
% seacarb: https://www.rdocumentation.org/packages/seacarb/versions/3.2.16
% Weiss, R. F. (1974). Carbon dioxide in water and seawater: the solubility of a non-ideal gas. Marine Chemistry, 2(3), 203–215. https://doi.org/10.1016/0304-4203(74)90015-2
% Dickson, A. G., Sabine, C. L., Christian, J. R., Bargeron, C. P., & North Pacific Marine Science Organization (Eds.). (2007). Guide to best practices for ocean CO2 measurements. Sidney, BC: North Pacific Marine Science Organization.
%
% Author: 
% Zelun Wu
% Ph.D. student.
% University of Delaware & Xiamen University
% zelunwu@outlook.com
% zelunwu.github.io
%%
tk = 273.15;           % [K] (for conversion [deg C] => [K])
TK = T + tk;           % TK [K]; T[C]
if nargin <3
    Patm = 1;
    P = 0;
end
% Original "seacarb" f2pCO2 calculation:
% B = (-1636.75+12.0408*TK-0.0327957*(TK*TK)+0.0000316528*(TK*TK*TK))*1e-6
% pCO2 =  fCO2*(1/exp((1*100000)*(B+2*(57.7-0.118*TK)*1e-6)/(8.314*TK)))
% Above calculation:
% - uses incorrect R (wrong units, incompatible with pressure in atm)
% - neglects a term "x2" (see below)
% - assumes pressure is always 1 atm (wrong for subsurface)

% Corrected approach to compute the fugacity coefficient, with which we convert pCO2 => fCO2
Phydro_atm = P / 1.01325;  % convert hydrostatic pressure from bar to atm (1.01325 bar / atm)
Ptot = Patm + Phydro_atm;  % total pressure (in atm)
R = 82.05736;              % (cm3 * atm) / (mol * K)  CODATA (2006)

% To compute fugcoeff, we need 3 other terms (B, Del, xc2) in addition to 3 others above (TK, Ptot, R)
B   = -1636.75 + 12.0408.*TK - 0.0327957.*TK.^2 + 0.0000316528*TK.^3;
Del = 57.7-0.118*TK;

% "x2" term often neglected (assumed = 1) in applications of Weiss's (1974) equation 9
% x2 = 1 - x1 = 1 - xCO2 (it is close to 1, but not quite)
% Let's assume that xCO2 = fCO2. Resulting fugcoeff is identical to 8th digit after the decimal.
xCO2approx = fCO2;
xc2 = (1 - xCO2approx*1e-6).^2; 

fugcoeff = exp(Ptot*(B + 2.*xc2.*Del)./(82.057.*TK));
pCO2 = fCO2 ./ fugcoeff;
end
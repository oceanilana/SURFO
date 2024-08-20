% From Wang Lab %
function kt = co_gas_transfer_velocity(sst,wspd,c,unit)
% kt = co_gas_transfer_velocity(sst,wspd,c,unit)
% Calculate the gas transfer velocity from sst, sss, and wind speed
% Input:
%     sst: sea surface temperature
%     wspd: wind speed
%     c: coefficient, default is 0.251 (Wanninkhof, 2014)
%     unit: 1, or 2. 1 represents unit for k is hour/cm; 2 represents m/yr. default is 1.
%
% Output:
%     kt: gas transfer velocity
% 
% Uncertainty is 20% on the global average, but should be calibrated with the mooring measured wind speed in the
% regional ocean.
%
% Algorithm: 
% kt = c * wpsd^2 * (Sc/660)^-0.5 
% Sc is the Schmit numer, which is a function of SST: 
% Sc = A + B*(sst) + C*(sst.^2) + D*(sst.^3) + E*(sst.^4);
% A = 2116.8; B = -136.25; C = 4.7353; D = -0.092307; E = 0.0007555;
% 
% References:
% Wanninkhof, R. (2014). Relationship between wind speed and gas exchange over the ocean revisited. Limnology and Oceanography: Methods, 12(6), 351–362. https://doi.org/10.4319/lom.2014.12.351
% 
% Author: 
% Zelun Wu
% Ph.D. student.
% University of Delaware & Xiamen University
% zelunwu@outlook.com
% zelunwu.github.io
%%
if nargin<3
    c = 0.251;
    unit = 1;
elseif nargin < 4
    unit = 1;
end
    
A = 2116.8; B = -136.25; C = 4.7353; D = -0.092307; E = 0.0007555;
Sc = A + B*(sst) + C*(sst.^2) + D*(sst.^3) + E*(sst.^4); % Jähne et al. (1987), Wanninkhof 2014
kt = c .* wspd.^2 .*((Sc/660).^(-0.5)); % unit: cm/hour
if unit == 2
    kt = kt*(24*365/100); % unit: m/yr
end
end

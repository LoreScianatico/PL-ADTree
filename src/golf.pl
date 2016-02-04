%golf dataset
%valori
esempio([1,[sunny, 85, 85, false], n]).
esempio([2,[sunny, 80, 90, true], n]).
esempio([3,[overcast, 83, 78, false], p]).
esempio([4,[rain, 70, 96, false], p]).
esempio([5,[rain, 68, 80, false], p]).
esempio([6,[rain, 65, 70, true], n]).
esempio([7,[overcast, 64, 65, true], p]).
esempio([8,[sunny, 72, 95, false], n]).
esempio([9,[sunny, 69, 70, false], p]).
esempio([10,[rain, 75, 80, false], p]).
esempio([11,[sunny, 75, 70, true], p]).
esempio([12,[overcast, 72, 90, true], p]).
esempio([13,[overcast, 81, 75, false], p]).
esempio([14,[rain, 71, 80, true], n]).

%attributi
attributo([outlook,temperature,humidity,wind]).

%dominio
dominio(outlook,[overcast,sunny,rain]).
dominio(temperature,continuous).
dominio(humidity,continuous).
dominio(wind,[true,false]).

%tennis dataset
%attributi e dominio
attributo([outlook,temperature,humidity,wind]).

dominio(outlook,[overcast,sunny,rain]).
dominio(temperature,[hot,mild,cool]).
dominio(humidity,[high,normal]).
dominio(wind,[weak,strong]).

%valori
esempio([1,[sunny, hot, high, weak], n]).
esempio([2,[sunny, hot, high, strong], n]).
esempio([3,[overcast, hot, high, weak], p]).
esempio([4,[rain, mild, high, weak], p]).
esempio([5,[rain, cool, normal, weak], p]).
esempio([6,[rain, cool, normal, strong], n]).
esempio([7,[overcast, cool, normal, strong], p]).
esempio([8,[sunny, mild, high, weak], n]).
esempio([9,[sunny, cool, normal, weak], p]).
esempio([10,[rain, mild, normal, weak], p]).
esempio([11,[sunny, mild, normal, strong], p]).
esempio([12,[overcast, mild, high, strong], p]).
esempio([13,[overcast, hot, normal, weak], p]).
esempio([14,[rain, mild, high, strong], n]).
